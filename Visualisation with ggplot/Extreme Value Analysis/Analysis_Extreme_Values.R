#----------------- Analysing Data of a Temp. and Light Data Logger  -----------
# Nico Wagner
# https://github.com/NicoW1994
# 26.04.2019
#------------------------------------------------------------------------------

# Packages laden
library(tidyverse)
library(lubridate)
library(zoo)
library(quantreg)
library(PerformanceAnalytics)
library(lmomco)
library(MASS)
library(fitdistrplus)
library(logspline)

# connect to GitHub dircetory for accessing data
path <- "https://raw.githubusercontent.com/NicoW1994/PortfolioProjects/main/Visualisation%20with%20ggplot/Extreme%20Value%20Analysis/Data/"

#Funktionen (u.a. trotz fehlender Werte ausfuehren)
meanNA <- function(x){mean(x, na.rm = TRUE)}
minNA <- function(x){min(x, na.rm = TRUE)}
maxNA <- function(x){max(x, na.rm = TRUE)}       
sumMissing <- function(x){sum(is.na(x))} 


neutral <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              strip.background = element_rect(colour="grey85"),
                              panel.border = element_rect(colour = "grey85")) 

#Importieren der Daten und Formatierung des Datums in POSIXct
#---------Abfluss importieren und aufbereiten-------#
Abfluss <- read.csv(paste0(path,"Abfluss_Oppenweiler.csv"), 
                    header = FALSE, sep = ";", dec =",", skip = "2", stringsAsFactors = FALSE, 
                    col.names = c("Datum", "Q", "Geprueft"))

Abfluss$Datum <- as.POSIXct(Abfluss$Datum, tz = "etc/GMT+1", format = "%d.%m.%Y %H:%M")

summary(Abfluss) # summary zeigt dass es Min. Werte von -999.000
str(Abfluss)     

#Einen Wert im Data Frame durch NA ersetzen   
Abfluss$Q[Abfluss$Q == -999.000] <- NA
# summary(Abfluss)

#Fehlende Zeitwerte durch Differenz von 1 Stunde ermitteln  
Abfluss$Differenz <- c(1, diff(Abfluss$Datum))  
sum(Abfluss$Differenz, na.rm = TRUE)
minNA(Abfluss$Differenz)
maxNA(Abfluss$Differenz) 
# keine fehlenden Zeitschritte

#Fehlende Werte (NA)?    
apply(Abfluss,2,sumMissing) 
# 5909 NA´s bei Q 

#------- Niederschlag importieren und aufbereiten------#
Niederschlag <- read.csv("https://raw.githubusercontent.com/NicoW1994/PortfolioProjects/main/Visualisation%20with%20ggplot/Extreme%20Value%20Analysis/Data/Ns_hour.txt", 
               header = TRUE, sep = ";", stringsAsFactors = FALSE)

#Formatierung des Datums in POSIXct
date_character <- as.character(Niederschlag$MESS_DATUM)
Niederschlag$Datum <- as.POSIXct(date_character,tz = "etc/GMT+1", format = "%Y%m%d%H")
str(Niederschlag)
summary(Niederschlag)

#Fehlende Zeitwerte durch Differenz von 1 Stunde ermitteln  
Niederschlag$Differenz <- c(1, diff(Niederschlag$Datum))  
minNA(Niederschlag$Differenz)
maxNA(Niederschlag$Differenz) 
# Zeitschritte fehlen

#Fehlende Werte (NA)?    
apply(Niederschlag,2,sumMissing) 
#keine NA´s vorhanden

#Jahreszahl auslesen und anhängen
Niederschlag$Jahr <- as.numeric(substring(Niederschlag$Datum, 1, 4))

#----------Import Verdunstungswerte-------------#
Verdunstung <- read.csv(paste0(path,"Verdunstung.txt"), 
                  header = TRUE, sep = ";", stringsAsFactors = FALSE)

Verdunstung$Datum <- as.POSIXct(as.character(Verdunstung$Datum),"%Y%m%d",tz="etc/GMT+1")

Verdunstung$Jahr <- as.numeric(substring(Verdunstung$Datum, 1, 4))

# Daten miteinander vergleichen und Plausibilität prüfen

# 1. Extremwert Zeitreihen, Visualisierung, Smoothing

##Für Zeitreihe die mittleren Stundenwerte des Abflusses am Pegel berechnen
# Tages- und Monatsmittel
Jahr <- as.numeric(substring(Abfluss$Datum, 1, 4))
Monat <- as.numeric(substring(Abfluss$Datum, 6, 7))
Tag <- as.numeric(substring(Abfluss$Datum, 9, 10))

df <- data.frame(date = Abfluss$Datum, Q = Abfluss$Q , Jahr, Monat, Tag, stringsAsFactors = FALSE)

# Tageswerte aggregieren
Q_day <- aggregate(Q ~ Tag + Monat + Jahr, mean, na.action = na.omit, data = df)
Q_day$date <- as.Date(with(Q_day, paste(Jahr, Monat, Tag,sep="-")), "%Y-%m-%d")

# Monatswerte aggregieren
Q_month <- aggregate(Q ~ Monat + Jahr, mean, na.action = na.omit, data = df)
Q_month$date <- as.character(with(Q_month, paste(Jahr, Monat, sep="-")))
Q_month$date <- as.Date(as.yearmon(Q_month$date))

## jährliche maximale Abflüsse (Jahreshochwasser)
HW <- aggregate(Q ~ Jahr, max, na.action = na.omit, data = df)
HW_1 <- filter(df, Q %in% HW$Q & Jahr %in% HW$Jahr)

## jährlicher Niedrigwasserkennwert aus den Tageswerten
# Niedrigwasserperiode bestimmen
NM7Q <-  Q_day %>% mutate(Qmean7 = rollapply(Q, width = 7, FUN = "mean", partial = TRUE, align = "center"))
NM7Q_1 <- aggregate(Qmean7 ~ Jahr, min, na.action = na.omit, data = NM7Q)

## Visualisierung der Monatswerte als heatmap. X-Asche 12 Kalendermonate, Y-Achse n Jahre

# Monat als Name anhängen
Q_month$m <- month(ymd(Q_month$date), label = TRUE, abbr = TRUE)

# Pardekoeffizient für Abfluss berechnen
# Pk <- MMQ/MJQ

# Jahresmittelwert
Q_year <- aggregate(Q ~ Jahr, mean, data = Q_day)
MQ <- mean(Q_year$Q)
# merge by year to get a new data frame (left_join)
Q_month1 <- left_join(Q_year, Q_month, by = "Jahr")
# Pardekoeffizient eines Einzeljahres für den Monat i im Jahr j
Q_month1$Pk_ij <- Q_month1$Q.y/Q_month1$Q.x

# Plot mittlerer Pardekoeffizient 
# Farbverlauf festlegen
# gradient <- scale_colour_gradient(low = "blue", high = "yellow")

ggplot(data = Q_month1, mapping = aes(x = m, y = Jahr)) + neutral +
  geom_tile(aes(fill = Pk_ij), color = "white") +
  scale_fill_continuous(low = "blue", high = "yellow") +
  labs(x = "Monat\n", y = "Jahr\n", fill = "Pardekoeffizient")

## Visualiserung der Extremereignisse 1. normal 2. geglättet
extreme <- data.frame(Jahr = HW$Jahr, HW = HW$Q, NM7Q = NM7Q_1$Qmean7)

# smoothen mit spline bzw. gam
require(mgcv)
HW_fm <- gam(HW ~ s(Jahr), data=extreme)
HW_pred <- predict(HW_fm)

HW_lm <- lm(HW ~ Jahr, data=extreme)
HW_pred2 <- predict(HW_lm)

NM7Q_fm <- gam(NM7Q ~ s(Jahr), data=extreme)
NM7Q_pred <- predict(NM7Q_fm)

require(gridExtra)

# Hochwasser
plot_HW <- ggplot() + neutral +
  geom_line(data = extreme, mapping = aes(x=Jahr, y = HW, color = "HQ"))+
   geom_smooth(data = extreme, mapping = aes(x=Jahr, y = HW, color = "HQ-loess")) +
    geom_line(mapping = aes(x = extreme$Jahr, y = HW_pred, color = "HQ-GAM")) +
     geom_point(data = extreme, mapping = aes(x=Jahr, y = HW), pch = 1) +
      scale_color_manual(values = c("firebrick", "black", "blue")) +
       labs(color ="", y = expression(paste("Abfluss [",m^3,"/", s,"]"))) +
        theme(legend.position = c(0.12, 0.89), legend.background = element_blank())

# Niedrigwasser
plot_NW <- ggplot() + neutral +
  geom_line(data = extreme, mapping = aes(x = Jahr, y = NM7Q, color = "NM7Q")) +
   geom_smooth(data = extreme, mapping = aes(x=Jahr, y = NM7Q, color = "NM7Q-loess")) +
    geom_line(mapping = aes(x = extreme$Jahr, y = NM7Q_pred, color = "NM7Q-GAM"))+
     geom_point(data = extreme, mapping = aes(x=Jahr, y = NM7Q), pch = 1) +
      scale_color_manual(values = c("steelblue","black","blue")) +
       labs(color ="", y = expression(paste("Abfluss [",m^3,"/", s,"]"))) +
        theme(legend.position = c(0.13, 0.88), legend.background = element_blank())

#Hyptohese: Die Hochwasserereignisse sind seit dem Jahr 1985 extremer ausgefallen

# 2. Trends in den Extremwerten

# t-Test: Vergleich zweier Mittelwerte
# sollten: Annähernd Gaußverteilung, Varianzhomogenität, Unabhänigkeit, intervallskaliert
# Hochwasserereignisse nehmen seit dem Jahr 2000 ab
# Das heißt Datensatz wird aufgeteilt in Jahr < 2000 und > 2000 und daraus zwei Mittelwerte gebildet die miteinander verglichen werden:
# p-value < 0.05 sind die Ergebnisse signifikant 

# Hyothese1: past_2k < bf_2k
# Null Hypothese: past_2k = bf_2k

# p-value = 0.9937 damit sind die Ergebnisse nicht signifikant
# Nullhypothese wird somit bestätigt und die Mittelwerte unterscheiden sich nicht signifikant voneinander

HW_2kp <- filter(HW, Jahr >=1985)
HW_2kb <- filter(HW, Jahr < 1985)

result_t.test <- t.test(HW_2kp$Q, HW_2kb$Q, alternative = "greater", var.equal = FALSE)

# Hypothese 2: Die Niedrigwasserereignisse nach dem Jahr 1990 fielen extremer aus
# Aufteilung Datensatz NM7Q

# ANOVA - Analysis of Variance
# needs an object containing the results returned by a model fitting function  
HW_gam <- gam(HW ~ s(Jahr), data=extreme)

anova(HW_fm)
# p-value von 0.415 zeigt dass das Jahr nicht entscheident ist für Hochwasserereignisse, sind also relativ gleich verteilt über die geasmte Zeitreihe
anova(NM7Q_fm)
# p-value von 0.039 zeigt dass das Jahr entscheident ist für den Niedrigwasserabfluss

# aov, benötigt mindestens 3 Gruppen zum vergleichen 
# HW
av1 <- filter(HW, Jahr >= 1968 & Jahr <= 1984) %>% 
  mutate(part = "(1) 1968 - 1984")
av2 <- filter(HW, Jahr >= 1985 & Jahr <= 2000) %>% 
  mutate(part = "(2) 1985 - 2000")
av3 <- filter(HW, Jahr > 2000) %>% 
  mutate(part = "(3) ab 2000")
av_HW <- rbind(av1,av2,av3)

anov_HW <- aov(formula = Q ~ part, data = av_HW)

# tukey
result_tukey <- TukeyHSD(anov_HW)

# man-kendallt Auswertung
mann.kendall <- function(t,x){
  sig.fun <- function(x) {sign(x[2]-x[1])}
  
  x <- x[which(is.finite(x))]
  t <- t[which(is.finite(x))]
  
  Q <- sum(combn(x,m=2,FUN=sig.fun,simplify=T))
  
  n <- length(x)
  va <- 1/18 * n * (n-1) * (2*n+5)
  p <- 1 - pnorm(abs(Q), mean =0, sd = sqrt(va))
  
  dt.all <- combn(t, m =2, FUN = diff, simplify = T)
  dx.all <- combn(x, m =2, FUN = diff, simplify = T)
  
  slope <- median(dx.all / dt.all)
  intercept <- median(x) - slope * median(t)
  
  #output
  return(list(S=Q/sqrt(va),p_one_side = p, slope = slope, intercept = intercept))
  
}

trend <- mann.kendall(HW$Jahr,HW$Q)
require(zyp)
slope <- zyp.trend.vector(HW$Q, HW$Jahr, preserve.range.for.sig.test = FALSE)

# Plot Mann-Kendall Test
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))
plot(HW$Jahr, HW$Q, type = "o", col = "black", 
     ylab = expression(paste("HQ [",m^3,"/", s,"]")) , xlab ="Jahre")
#abline(slope[11], slope[2], col = "blue")
abline(trend$intercept, trend$slope, col = "green")
abline(HW_lm, col = "red")
legend(x = 1968, y = 145, legend = c("Lineare Regression", "Mann Kendall Test"), lty = 1, 
       col = c("red","green"), bty = "n", 
       y.intersp = 0.7, cex = 0.8)


# parametrische trends
# anova test
# HW
HW_gam <- gam(HW ~ s(Jahr), data=extreme)
HW_pred <- predict(HW_fm)
HW_lm <- lm(HW ~ Jahr, data=extreme)
HW_pred2 <- predict(HW_lm)

anova(HW_gam)
anova(HW_lm)

# Erstellung der confidence Intervalle und prediction lines
# jährliche maximale Abflüsse

#confidence vs prediction interval
pred_HW_c <- predict(HW_lm, interval = "confidence")
pred_HW_p <- predict(HW_lm, interval = "prediction")

# NW
NM7Q_gam <- gam(NM7Q ~ s(Jahr), data = extreme)
NM7Q_pred <- predict(NM7Q_fm)
NM7Q_lm <- lm(NM7Q ~ Jahr, data = extreme)
NM7Q_pred2 <- predict(NM7Q_lm)

#anova
anova(NM7Q_gam)
anova(NM7Q_lm)

# plotting linear quantiles
rq10 <- rq(formula = Q ~ Jahr, tau = 0.1 ,data = HW)
rq25 <- rq(formula = Q ~ Jahr, tau = 0.25 ,data = HW)
rq50 <- rq(formula = Q ~ Jahr, tau = 0.5 ,data = HW)
rq75 <- rq(formula = Q ~ Jahr, tau = 0.75 ,data = HW)
rq90 <- rq(formula = Q ~ Jahr, tau = 0.9 ,data = HW)

# quantile Regression

quant <- seq(0.05, 0.95, 0.05)
n <- length(quant)
slope <- rep(0,n)
lowbd <- rep(0,n)
upbd <- rep(0,n)

for(i in 1:n) {
  Q <- summary(rq(Q ~ Jahr, tau=quant[i], data = HW), se = "rank")
  slope[i] <- Q$coefficients[2]
  lowbd[i] <- Q$coefficients[4]
  upbd[i] <- Q$coefficients[6]
}

# Regression mit Trendgeraden
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))
matplot(HW$Jahr, cbind(pred_HW_c, pred_HW_p[,-1]), lty =c(1,2,2,3,3) ,type = "l", 
        col = c("firebrick","steelblue","steelblue","black","black"), xlab = "Jahre", 
        ylab = expression(paste("HQ [",m^3,"/", s,"]")), ylim = c(-12,150),
        lwd = c(2,1.5,1.5,1.5,1.5))
points(HW$Jahr, HW$Q, type = "o", col = "black")
legend(x = 1968, y = 153, legend = c("Lineare Regression"," 95% Konfidenzintervall",
                                     "Prediction Interval"), lty = c(1, 2, 3), 
       col = c("red","blue","black"), bty = "n", 
       y.intersp = 0.9, cex = 0.8)

# Plot der Linearen regression mit Perzentilen 10%,25%,50% und 90%
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))
plot(HW$Jahr, HW$Q, xlab = "Jahr", ylab = expression(paste("HQ [",m^3,"/", s,"]")))
points(HW$Jahr, HW$Q, type = "o", col = "black")
abline(HW_lm, col = "red")
abline(rq10, col = "black")
abline(rq25, col = "blue")
abline(rq50, col = "steelblue")
abline(rq75, col = "purple3")
abline(rq90, col = "cadetblue1")  
legend(x = 1968, y = 153, legend = c("Lineare Regression","10%","25%","50%","75%","90%"), lty = 1, 
       col = c("red","black","blue","steelblue","purple3","cadetblue1"), bty = "n", 
       y.intersp = 0.7, cex =0.8)

# Trendanalyse der Quantile
plot(quant, slope, type = "b", xlab ="Quantil (tau)", ylab = "Trend (m3/s/a)", xlim = c(0,1),
     ylim = c(min(lowbd), max(upbd)))
lines(quant, lowbd, col ="red")
lines(quant, upbd, col = "red")
abline(a=0, b=0)

# Extreme value analysis
# Hoch- und Niedrigwasserwahrscheinlichkeit

# Cummulative Density Function (CDF) nach Weibull für Hochwasserereignisse berechnen 
k_1 <- rank(extreme$HW)
n_1 <- max(k_1)
P_u1 <- k_1/(n_1+1)
Tw = 1/(1-P_u1)

# HQ kann nun nach Unterschreitungswahrschienlichkeit oder nach Wiederkehtintervall geplottet werden

# Weibull NM7Q
k_2 <- rank(-extreme$NM7Q)
n_2 <- max(k_2)
P_u2 <- k_2/(n_2+1)
Tw_2 = 1/(1-P_u2)

# (HQ) Vergleich von kurtosis und Skewness um geeignete Verteilungsfunktion zu finden. Es zeigt sich, dass beta, lognormal, gamma 
# und exponential geeignete Kandidaten sind.

#(NM7Q) Auswertung fällt anderst aus als bei HQ. Es zeigt sich, dass logistic, lognormal, gamm und normal geeignete Kandidaten sind.

#
plotdist(extreme$HW, histo = TRUE, demp = TRUE)

#
plotdist(extreme$NM7Q, histo = TRUE, demp = TRUE)


# Methods of Moments werden verwendet um einen Datensatz bzw. eine Stichprobe zu beschreiben. 
# Also Verteilung, Abweichungen etc. Momente können verwendet werden um die Parameter einer Verteilung zu bestimmtén bzw. abzuschätzen. 

# L-moemnts: tail of the distributions describe low probability events. it is very difficult to estimate the shape of the tale.



# plot Empirische Wahrscheinlichkeit und berechnete Verteilungsfunktionen nach Methode der L-Momente (LM) fuer HQ
## moment based estimators L-moments
mean_exHQ <- mean(extreme$HW)
sd_exHQ <- sd(extreme$HW)
sk_exHQ <- skewness(extreme$HW)

mean_exNQ <- mean(extreme$NM7Q)
sd_exNQ <- sd(extreme$NM7Q)
sk_exNQ <- skewness(extreme$NM7Q)

F <- 1:1000/(1000+1)
F_2 <- 1000:1/(1000+1)
T <- sort(1/F)

par(mar = c(c(5.1, 5.5, 4.1, 2.1)))

# HQ log-normal
plot(Tw, extreme$HW, log = "x" , xlim = c(1,200), ylim = c(5,200) ,xlab ="Jährlichkeit",
     ylab = expression(paste("HQ [",m^3,"/", s,"]")))
lines(T,qlnorm(F, meanlog = mean(log(extreme$HW)), sdlog = sd(log(extreme$HW)), 
               log = FALSE), col = "blue")

# probability weighted moments (L-moments)
# gamma
GAM <- pargam(lmom.ub(extreme$HW))
lines(T, quagam(F,GAM), col = "firebrick")

# Weibul using L-Moments
WEI <- parwei(lmom.ub(extreme$HW))
lines(T, quawei(F,WEI), col = "forestgreen")

# l_Moments for Pearson 3
#PE3 <- parpe3(lmom.ub(extreme$HW))
#lines(T, quape3(F,PE3), col = "green", lty = 2, lwd = 0.7)

legend(x = 1, y = 200, legend = c("Log-Normal (LM)","Gamma (LM)","Weibull (LM)"),
       lty = c(1,1,1), 
       col = c("blue","firebrick","forestgreen"), bty = "n", 
       y.intersp = 0.8, cex= 0.7)

abline(v = 100, lty = 2)
abline(v = 200, lty = 2)

# 100- und 200 Jährlichkeit berechnen
#sigma <- 17.89183
#mu <- 32.04551
#x <- 100
#F_NORM = 1/(sqrt(2*pi)*sigma * x) * exp(-(log(x) - mu)^2 / (2* sigma^2))

# Plot Empirische Wahrscheinlichkeit und berechnete Verteilungsfunktionen nach Maximum-Likelihood Methode (MLE) fuer HQ
# Maximum Likelihood for log normal (HQ): function fitdistr {MASS}
par_log <- fitdistr(extreme$HW, "log-normal")
#
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))

# lognormal ML
plot(Tw, extreme$HW, log = "x" , xlim = c(1,250), ylim = c(5,200) ,xlab ="Jährlichkeit",
     ylab = expression(paste("HQ [",m^3,"/", s,"]")))
lines(T, qlnorm(F, par_log$estimate[1], par_log$estimate[2], log = FALSE), col = "blue")
lines(T, qlnorm(F, par_log$estimate[1] + 2*par_log$sd[1], par_log$estimate[2] 
                + 2*par_log$sd[2] ,log = FALSE), col = "blue", lty =2)
lines(T, qlnorm(F, par_log$estimate[1] - 2*par_log$sd[1], par_log$estimate[2] 
                - 2*par_log$sd[2] , log = FALSE), col = "blue", lty =2)

# Weibull ML
par_wb <- fitdistr(extreme$HW, "weibull")


# Gamma
par_gam <- fitdistr(extreme$HW, "gamma")
lines(T, qgamma(F, par_gam$estimate[1], par_gam$estimate[2], log = FALSE), 
      col = "firebrick")
lines(T, qgamma(F, par_gam$estimate[1] + 2*par_gam$sd[1], par_gam$estimate[2] 
                + 2*par_gam$sd[2] ,log = FALSE), col = "firebrick", lty =2)
lines(T, qgamma(F, par_gam$estimate[1] - 2*par_gam$sd[1], par_gam$estimate[2] - 
                  2*par_gam$sd[2] ,log = FALSE), col = "firebrick", lty =2)
legend(x = 1, y = 200, legend = c("Log-Normal (MLE)","Gamma (MLE)","95%-Konfidenz."), lty = c(1,1,2), 
       col = c("blue","firebrick","black"), bty = "n", 
       y.intersp = 0.8, cex = 0.7)

abline(v = 100, lty = 3)
abline(v = 200, lty = 3)

##-- 100 und 200 Jährige Hochwasserwahrscheinlichkeit berechnen LM
# lognormal
norm_100 <- round(qlnorm(0.99, meanlog = mean(log(extreme$HW)), sdlog = sd(log(extreme$HW)), 
                         log = FALSE),2)
norm_200 <- round(qlnorm(0.995, meanlog = mean(log(extreme$HW)), sdlog = sd(log(extreme$HW)), 
                         log = FALSE),2)
# Gamma
gamma_100 <- round(quagam(0.99,GAM),2)
gamma_200 <- round(quagam(0.995,GAM),2)

# Weibull
wei_100 <- round(quawei(0.99,WEI),2)
wei_200 <- round(quawei(0.995, WEI),2)

#--- 100 und 200 jährliche HQ mit MLE
# log-normal
norm_100MLE <- round(qlnorm(0.99, par_log$estimate[1], par_log$estimate[2], log = FALSE),2)
norm_200MLE <- round(qlnorm(0.995, par_log$estimate[1], par_log$estimate[2], log = FALSE),2)
# gamma
gamma_100MLE <- round(qgamma(0.99, par_gam$estimate[1], par_gam$estimate[2], log = FALSE),2)
gamma_200MLE <- round(qgamma(0.995, par_gam$estimate[1], par_gam$estimate[2], log = FALSE),2)

# Unsicherheit für HQ berechnen
gamma_100MLE_low <- qgamma(0.99, par_gam$estimate[1] + 2*par_gam$sd[1], par_gam$estimate[2] 
                           + 2*par_gam$sd[2] ,log = FALSE)
gamma_100MLE_high <- qgamma(0.99, par_gam$estimate[1] - 2*par_gam$sd[1], par_gam$estimate[2] - 
                              2*par_gam$sd[2] ,log = FALSE)
gamma_200MLE_low <- qgamma(0.995, par_gam$estimate[1] + 2*par_gam$sd[1], par_gam$estimate[2] 
                           + 2*par_gam$sd[2] ,log = FALSE)
gamma_200MLE_high <- qgamma(0.995, par_gam$estimate[1] - 2*par_gam$sd[1], par_gam$estimate[2] - 
                              2*par_gam$sd[2] ,log = FALSE)
gamma_unsicherheit_100_high <- round(gamma_100MLE_high - gamma_100MLE,2)
gamma_unsicherheit_100_low <- round(gamma_100MLE - gamma_100MLE_low ,2)
gamma_unsicherheit_100 <- mean(c(gamma_unsicherheit_100_high, gamma_unsicherheit_100_low))

gamma_unsicherheit_200_high <- round(gamma_200MLE_high - gamma_200MLE,2)
gamma_unsicherheit_200_low <- round(gamma_200MLE - gamma_200MLE_low ,2)
gamma_unsicherheit_200 <- mean(c(gamma_unsicherheit_200_high, gamma_unsicherheit_200_low))

norm_100MLE_high <- qlnorm(0.99, par_log$estimate[1] + 2*par_log$sd[1], par_log$estimate[2] 
                           + 2*par_log$sd[2] ,log = FALSE)
norm_100MLE_low <- qlnorm(0.99, par_log$estimate[1] - 2*par_log$sd[1], par_log$estimate[2] 
                          - 2*par_log$sd[2] , log = FALSE)
norm_200MLE_high <- qlnorm(0.995, par_log$estimate[1] + 2*par_log$sd[1], par_log$estimate[2] 
                           + 2*par_log$sd[2] ,log = FALSE)
norm_200MLE_low <- qlnorm(0.995, par_log$estimate[1] - 2*par_log$sd[1], par_log$estimate[2] 
                          - 2*par_log$sd[2] , log = FALSE)
norm_unsicherheit_100_high <-round(norm_100MLE_high - norm_100MLE,2)
norm_unsicherheit_100_low <-round(norm_100MLE - norm_100MLE_low,2)
norm_unsicherheit_100 <- mean(c(norm_unsicherheit_100_high, norm_unsicherheit_100_low))

norm_unsicherheit_200_high <-round(norm_200MLE_high - norm_200MLE,2)
norm_unsicherheit_200_low <-round(norm_200MLE - norm_200MLE_low,2)
norm_unsicherheit_200 <- mean(c(norm_unsicherheit_200_high, norm_unsicherheit_200_low))


# Plot Empirische Wahrscheinlichkeit und berechnete Verteilungsfunktionen nach Methode der L-Momente (LM) fuer NM7Q"
# NM7Q log-normal
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))
plot(Tw_2, extreme$NM7Q, log = "x" , xlim = c(1,200), ylim = c(0.1,1.2) ,xlab ="Jährlichkeit",
     ylab = expression(paste("NM7Q [",m^3,"/", s,"]")))
lines(T,qlnorm(F_2, meanlog = mean(log(extreme$NM7Q)), sdlog = sd(log(extreme$NM7Q)), log = FALSE),
      col = "blue")

# Weibul using L-Moments
WEI_2 <- parwei(lmom.ub(extreme$NM7Q))
lines(T, quawei(F_2,WEI_2), col = "firebrick", lty = 1, lwd = 0.7)

legend(x = 30, y = 1.2, legend = c("Log-Normal (LM)","Weibull (LM)"), lty = c(1,1), 
       col = c("blue","firebrick"), bty = "n", 
       y.intersp = 0.8, cex=0.7)

# Plot Empirische Wahrscheinlichkeit und berechnete Verteilungsfunktionen nach Maximum-Likelihood Methode (MLE) fuer NM7Q"
# Maximum Likelihood for log normal (NM7Q): funtion fitdistr {MASS}
par_log2 <- fitdistr(extreme$NM7Q, "log-normal")

# lognormal ML
par(mar = c(c(5.1, 5.5, 4.1, 2.1)))
plot(Tw_2, extreme$NM7Q, log = "x" , xlim = c(1,200), ylim = c(0.1,1.2) ,xlab ="Jährlichkeit",
     ylab = expression(paste("NM7Q [",m^3,"/", s,"]")))
lines(T, qlnorm(F_2, par_log2$estimate[1], par_log2$estimate[2], log = FALSE), col = "blue")
lines(T, qlnorm(F_2, par_log2$estimate[1] + 2*par_log2$sd[1], par_log2$estimate[2] + 2*par_log2$sd[2] ,
                log = FALSE), col = "blue", lty =2)
lines(T, qlnorm(F_2, par_log2$estimate[1] - 2*par_log2$sd[1], par_log2$estimate[2] - 2*par_log2$sd[2] ,
                log = FALSE), col = "blue", lty =2)

par_wei2 <- fitdist(extreme$NM7Q, "weibull", method = "mle")
lines(T, qweibull(F_2, par_wei2$estimate[1], par_wei2$estimate[2], log = FALSE), col = "firebrick")
lines(T, qweibull(F_2, par_wei2$estimate[1] + 2*par_wei2$sd[1], par_wei2$estimate[2] + 2*par_wei2$sd[2] ,
                  log = FALSE), col = "firebrick", lty =2)
lines(T, qweibull(F_2, par_wei2$estimate[1] - 2*par_wei2$sd[1], par_wei2$estimate[2] - 2*par_wei2$sd[2] ,
                  log = FALSE), col = "firebrick", lty =2)

legend(x = 30, y = 1.2, legend = c("Log-Normal (MLE)","Weibull (MLE)","95%-Konfidenz."), lty = c(1,1,2), 
       col = c("blue","firebrick","black"), bty = "n", 
       y.intersp = 0.8, cex = 0.7)


##-- 100 und 200 Jährige Niedrigwasserwahrscheinlichkeiten berechnen LM
# lognormal
norm_100NQ <- round(qlnorm(0.01, meanlog = mean(log(extreme$NM7Q)), sdlog = sd(log(extreme$NM7Q)), 
                           log = FALSE),2)
norm_200NQ <- round(qlnorm(0.005, meanlog = mean(log(extreme$NM7Q)), sdlog = sd(log(extreme$NM7Q)), 
                           log = FALSE),2)

# Weibull
wei_100NQ <- round(quawei(0.01,WEI_2),2)
wei_200NQ <- round(quawei(0.005, WEI_2),2)

##-- 100 und 200 Jährige Niedrigwasserwahrscheinlichkeiten berechnen MLE
# lognormal
norm_100NQMLE <- round(qlnorm(0.01, par_log2$estimate[1], par_log2$estimate[2], log = FALSE),2)
norm_200NQMLE <- round(qlnorm(0.005, par_log2$estimate[1], par_log2$estimate[2], log = FALSE),2)

# Weibull
wei_100NQMLE <- round(qweibull(0.01, par_wei2$estimate[1], par_wei2$estimate[2], log = FALSE),2)
wei_200NQMLE <- round(qweibull(0.005, par_wei2$estimate[1], par_wei2$estimate[2], log = FALSE),2)

# Unsicherheiten für NQ berechnen
# Log-Normal
norm_100NQMLE_low <- qlnorm(0.01, par_log2$estimate[1] + 2*par_log2$sd[1], par_log2$estimate[2] 
                            + 2*par_log2$sd[2] ,log = FALSE)
norm_100NQMLE_high <- qlnorm(0.01, par_log2$estimate[1] - 2*par_log2$sd[1], par_log2$estimate[2] 
                             - 2*par_log2$sd[2] ,log = FALSE)
norm_200NQMLE_low <- qlnorm(0.005, par_log2$estimate[1] + 2*par_log2$sd[1], par_log2$estimate[2] 
                            + 2*par_log2$sd[2] ,log = FALSE)
norm_200NQMLE_high <- qlnorm(0.005, par_log2$estimate[1] - 2*par_log2$sd[1], par_log2$estimate[2] 
                             - 2*par_log2$sd[2] ,log = FALSE)

norm_unsicherheit_NQ100 <-round(norm_100NQMLE_high - norm_100NQMLE,2)
norm_unsicherheit_NQ200 <-round(norm_200NQMLE_high - norm_200NQMLE,2)

# Weibull

wei_100NQMLE_high <- qweibull(0.01, par_wei2$estimate[1] + 2*par_wei2$sd[1], 
                              par_wei2$estimate[2] + 2*par_wei2$sd[2] ,log = FALSE)
wei_100NQMLE_low <- qweibull(0.01, par_wei2$estimate[1] - 2*par_wei2$sd[1], 
                             par_wei2$estimate[2] - 2*par_wei2$sd[2] ,log = FALSE)
wei_200NQMLE_high <- qweibull(0.005, par_wei2$estimate[1] + 2*par_wei2$sd[1], 
                              par_wei2$estimate[2] + 2*par_wei2$sd[2] ,log = FALSE)
wei_200NQMLE_low <- qweibull(0.005, par_wei2$estimate[1] - 2*par_wei2$sd[1], 
                             par_wei2$estimate[2] - 2*par_wei2$sd[2] ,log = FALSE)

wei_unsicherheit_NQ100 <-round(wei_100NQMLE_high - wei_100NQMLE,2)
wei_unsicherheit_NQ200 <-round(wei_200NQMLE_high - wei_200NQMLE,2)

## Diagnostics
# ks-test: Nullhypothese = Die Zufallsvariablen X und Y besitzen die gleiche Wahrsceinlichkeitsverteilung
# Alternativhypothese = X besitzt eine andere Wahrscienlichkeitsverteilung als Y
# Nullhypthese wird bei einem Signifikanzniveau alpha abgelehtn, falls d_n,m den kritischen Wert 

# HQ
fit.gamma_1mle <- fitdist(extreme$HW, "gamma", method = "mle")
fit.gamma_1mme <- fitdist(extreme$HW, "gamma", method = "mme")
#fit.exp_1 <- fitdist(extreme$HW, "exp")
LNORM <- parnor(lmom.ub(extreme$HW))
fit.lnorm_1mle <- fitdist(extreme$HW, "lnorm", method ="mle")
fit.lnorm_1mme <- fitdist(extreme$HW, "lnorm", method ="mme")
fit.weibull_1mle <- fitdist(extreme$HW,"weibull", method ="mle")

# NQ
fit.lnorm_2mle <- fitdist(extreme$NM7Q,"lnorm", method = "mle")
fit.lnorm_2mme <- fitdist(extreme$NM7Q,"lnorm", method = "mme")
fit.weibull_2mle <- fitdist(extreme$NM7Q,"weibull", method = "mle")

#Kolmogorov-Smirnov test simulation (ks-test)

ks.test(extreme$HW, "plnorm", meanlog = fit.lnorm_1mle$estimate["meanlog"],
       sdlog = fit.lnorm_1mle$estimate["sdlog"])

ks.test(extreme$HW, "plnorm", meanlog = fit.lnorm_1mme$estimate["meanlog"],
        sdlog = fit.lnorm_1mme$estimate["sdlog"])

ks.test(extreme$HW, "plnorm", LNORM$para["mu"], LNORM$para["sigma"])

# ks-test HQ gamma
ks.test(extreme$HW, "pgamma", shape = fit.gamma_1mle$estimate["shape"], rate = fit.gamma_1mle$estimate["rate"])
ks.test(extreme$HW, "pgamma", shape = fit.gamma_1mme$estimate["shape"], rate = fit.gamma_1mme$estimate["rate"])

# ks-test HQ weibull
ks.test(extreme$HW, "pweibull", shape = fit.weibull_1mle$estimate["shape"],
        scale = fit.weibull_1mle$estimate["scale"])

#ks-test NQ log normal
ks.test(extreme$NM7Q, "plnorm", meanlog = fit.lnorm_2mle$estimate["meanlog"],
        sdlog = fit.lnorm_2mle$estimate["sdlog"])

ks.test(extreme$NM7Q, "plnorm", meanlog = fit.lnorm_2mme$estimate["meanlog"],
        sdlog = fit.lnorm_2mme$estimate["sdlog"])

#ks-test NQ weibull
ks.test(extreme$NM7Q, "pweibull", shape = fit.weibull_2mle$estimate["shape"],
        scale = fit.weibull_2mle$estimate["scale"])

##--- Überprüfung und Bewertung der Stationarität

# Aufteilung der Zeitreihe und für einzelne Abschnitte das Bemessungshochwasser/Niedrigwasser berechnen
J68_80 <- filter(extreme, Jahr <=1980 & Jahr >= 1968)
J80_92 <- filter(extreme, Jahr >=1980 & Jahr <= 1992)
J92_04 <- filter(extreme, Jahr >= 1992 & Jahr <= 2004)
J04_18 <- filter(extreme, Jahr >= 2004)

# Berechnung der Bemessungshochwässer/Niedrigwasser der einzelnen Zeitabschnitte
# Für Hochwasser mit Log-Normal Verteilung nach MLE
par_logHQ1 <- fitdistr(J68_80$HW, "log-normal")
par_logHQ2 <- fitdistr(J80_92$HW, "log-normal")
par_logHQ3 <- fitdistr(J92_04$HW, "log-normal")
par_logHQ4 <- fitdistr(J04_18$HW, "log-normal")

J68_80_HQ100 <- round(qlnorm(0.99, par_logHQ1$estimate[1], par_logHQ1$estimate[2], log = FALSE),2)
J80_92_HQ100 <- round(qlnorm(0.99, par_logHQ2$estimate[1], par_logHQ2$estimate[2], log = FALSE),2)
J92_04_HQ100 <- round(qlnorm(0.99, par_logHQ3$estimate[1], par_logHQ3$estimate[2], log = FALSE),2)
J04_18_HQ100 <- round(qlnorm(0.99, par_logHQ4$estimate[1], par_logHQ4$estimate[2], log = FALSE),2)

J68_80_HQ200 <- round(qlnorm(0.995, par_logHQ1$estimate[1], par_logHQ1$estimate[2], log = FALSE),2)
J80_92_HQ200 <- round(qlnorm(0.995, par_logHQ2$estimate[1], par_logHQ2$estimate[2], log = FALSE),2)
J92_04_HQ200 <- round(qlnorm(0.995, par_logHQ3$estimate[1], par_logHQ3$estimate[2], log = FALSE),2)
J04_18_HQ200 <- round(qlnorm(0.995, par_logHQ4$estimate[1], par_logHQ4$estimate[2], log = FALSE),2)

# Für Hochwasser mit gamma Verteilung nach MLE
par_gamHQ1 <- fitdistr(J68_80$HW, "gamma")
par_gamHQ2 <- fitdistr(J80_92$HW, "gamma")
par_gamHQ3 <- fitdistr(J92_04$HW, "gamma")
par_gamHQ4 <- fitdistr(J04_18$HW, "gamma")

J68_80_HQ100gam <- round(qgamma(0.99, par_gamHQ1$estimate[1], par_gamHQ1$estimate[2], log = FALSE),2)
J80_92_HQ100gam <- round(qgamma(0.99, par_gamHQ2$estimate[1], par_gamHQ2$estimate[2], log = FALSE),2)
J92_04_HQ100gam <- round(qgamma(0.99, par_gamHQ3$estimate[1], par_gamHQ3$estimate[2], log = FALSE),2)
J04_18_HQ100gam <- round(qgamma(0.99, par_gamHQ4$estimate[1], par_gamHQ4$estimate[2], log = FALSE),2)

J68_80_HQ200gam <- round(qgamma(0.995, par_gamHQ1$estimate[1], par_gamHQ1$estimate[2], log = FALSE),2)
J80_92_HQ200gam <- round(qgamma(0.995, par_gamHQ2$estimate[1], par_gamHQ2$estimate[2], log = FALSE),2)
J92_04_HQ200gam <- round(qgamma(0.995, par_gamHQ3$estimate[1], par_gamHQ3$estimate[2], log = FALSE),2)
J04_18_HQ200gam <- round(qgamma(0.995, par_gamHQ4$estimate[1], par_gamHQ4$estimate[2], log = FALSE),2)

# Berechnung der Bemessungshochwässer/Niedrigwasser der einzelnen Zeitabschnitte
# Für Niedrigwasser mit weibull Verteilung nach MLE
par_weiNQ1 <- fitdist(J68_80$NM7Q, "weibull", method = "mle")
par_weiNQ2 <- fitdist(J80_92$NM7Q, "weibull", method = "mle")
par_weiNQ3 <- fitdist(J92_04$NM7Q, "weibull", method = "mle")
par_weiNQ4 <- fitdist(J04_18$NM7Q, "weibull", method = "mle")

# Berechnung für NQ_100
J68_80_NQ100 <- round(qweibull(0.01, par_weiNQ1$estimate[1], par_weiNQ1$estimate[2], log = FALSE),2)
J80_92_NQ100 <- round(qweibull(0.01, par_weiNQ2$estimate[1], par_weiNQ2$estimate[2], log = FALSE),2)
J92_04_NQ100 <- round(qweibull(0.01, par_weiNQ3$estimate[1], par_weiNQ3$estimate[2], log = FALSE),2)
J04_18_NQ100 <- round(qweibull(0.01, par_weiNQ4$estimate[1], par_weiNQ4$estimate[2], log = FALSE),2)

# Berechnung für NQ_200
J68_80_NQ200 <- round(qweibull(0.005, par_weiNQ1$estimate[1], par_weiNQ1$estimate[2], log = FALSE),2)
J80_92_NQ200 <- round(qweibull(0.005, par_weiNQ2$estimate[1], par_weiNQ2$estimate[2], log = FALSE),2)
J92_04_NQ200 <- round(qweibull(0.005, par_weiNQ3$estimate[1], par_weiNQ3$estimate[2], log = FALSE),2)
J04_18_NQ200 <- round(qweibull(0.005, par_weiNQ4$estimate[1], par_weiNQ4$estimate[2], log = FALSE),2)


# 4. Dimensionsreduktion mittels PCA
# Principal Component Analysis (PCA) 

#  Annual data
# generate annual mean/max/min/etc values 
PCA_Jahr_HQ <- aggregate(Q ~ Jahr, max, data = df)
PCA_Jahr_HQ <- PCA_Jahr_HQ[-12,]
PCA_Jahr_min <- round(aggregate(Q ~ Jahr, min, data = df),2)
PCA_Jahr_min <- PCA_Jahr_min[-12,]
PCA_Jahr_mean <- round(aggregate(Q ~ Jahr, mean, data = df),2)
PCA_Jahr_mean <- PCA_Jahr_mean[-12,]
PCA_Jahr_NM7Q <- NM7Q_1[-12,]
PCA_Jahr_NM7Q$Qmean7 <- round(PCA_Jahr_NM7Q$Qmean7,2)

# max Werte der Quartale
PCA_Jahr_1quartal_a <- filter(df, Monat == 12 | Monat == 1 | Monat == 2)
PCA_Jahr_1quartal_b <- aggregate(Q ~ Jahr , max, data = PCA_Jahr_1quartal_a)
PCA_Jahr_1quartal_b <- PCA_Jahr_1quartal_b[-12,]
PCA_Jahr_2quartal_a <- filter(df, Monat == 3 | Monat == 4 | Monat == 5)
PCA_Jahr_2quartal_b <- aggregate(Q ~ Jahr , max, data = PCA_Jahr_2quartal_a)
PCA_Jahr_3quartal_a <- filter(df, Monat == 6 | Monat == 7 | Monat == 8)
PCA_Jahr_3quartal_b <- aggregate(Q ~ Jahr , max, data = PCA_Jahr_3quartal_a)
PCA_Jahr_3quartal_b <- PCA_Jahr_3quartal_b[-12,]
PCA_Jahr_4quartal_a <- filter(df, Monat == 9 | Monat == 10 | Monat == 11)
PCA_Jahr_4quartal_b <- aggregate(Q ~ Jahr , max, data = PCA_Jahr_4quartal_a)
PCA_Jahr_4quartal_b <- PCA_Jahr_4quartal_b[-12,]

# min Werte der Quartale
PCA_Jahr_1quartal_min_a <- filter(df, Monat == 12 | Monat == 1 | Monat == 2)
PCA_Jahr_1quartal_min_b <- aggregate(Q ~ Jahr , min, data = PCA_Jahr_1quartal_min_a)
PCA_Jahr_2quartal_min_a <- filter(df, Monat == 3 | Monat == 4 | Monat == 5)
PCA_Jahr_2quartal_min_b <- aggregate(Q ~ Jahr , min, data = PCA_Jahr_2quartal_min_a)
PCA_Jahr_3quartal_min_a <- filter(df, Monat == 6 | Monat == 7 | Monat == 8)
PCA_Jahr_3quartal_min_b <- aggregate(Q ~ Jahr , min, data = PCA_Jahr_3quartal_min_a)
PCA_Jahr_4quartal_min_a <- filter(df, Monat == 9 | Monat == 10 | Monat == 11)
PCA_Jahr_4quartal_min_b <- aggregate(Q ~ Jahr , min, data = PCA_Jahr_4quartal_min_a)

# mean Werte der Quartale
PCA_Jahr_1quartal_mean_a <- filter(df, Monat == 12 | Monat == 1 | Monat == 2)
PCA_Jahr_1quartal_mean_b <- aggregate(Q ~ Jahr , mean, data = PCA_Jahr_1quartal_mean_a)
PCA_Jahr_2quartal_mean_a <- filter(df, Monat == 3 | Monat == 4 | Monat == 5)
PCA_Jahr_2quartal_mean_b <- aggregate(Q ~ Jahr , mean, data = PCA_Jahr_2quartal_mean_a)
PCA_Jahr_3quartal_mean_a <- filter(df, Monat == 6 | Monat == 7 | Monat == 8)
PCA_Jahr_3quartal_mean_b <- aggregate(Q ~ Jahr , mean, data = PCA_Jahr_3quartal_mean_a)
PCA_Jahr_4quartal_mean_a <- filter(df, Monat == 9 | Monat == 10 | Monat == 11)
PCA_Jahr_4quartal_mean_b <- aggregate(Q ~ Jahr , mean, data = PCA_Jahr_4quartal_mean_a)


# Quantile der Jahre berechnen
df_quantile <- df %>% 
  group_by(Jahr) %>% 
  summarize(quants = quantile(Q, probs = 0.9, na.rm = TRUE))
df_quantile <- as.data.frame(df_quantile[-12,])


# Daten in ein Data Frame packen
df_PCA <- data.frame(Jahr = PCA_Jahr_HQ$Jahr, HQ = PCA_Jahr_HQ$Q, Q_mean = PCA_Jahr_mean$Q, 
                     Q_min = PCA_Jahr_min$Q, NM7Q = PCA_Jahr_NM7Q$Qmean7, 
                     Winter_max =PCA_Jahr_1quartal_b$Q,
                     Spring_max = PCA_Jahr_2quartal_b$Q, Summer_max = PCA_Jahr_3quartal_b$Q, 
                     Autumn_max = PCA_Jahr_4quartal_b$Q)


names(df_PCA) <- c("Jahr","HQ","Q_mean","Q_min","NM7Q","HQ_Winter","HQ_Spring",
                   "HQ_Summer", "HQ_Autumn", "Q_tau0.9")
data_PCA <- as.matrix(df_PCA[,-1])
pc.cr <- princomp(data_PCA, cor = TRUE)

#library(factoextra)
fviz_eig(pc.cr)

# biplot
biplot(pc.cr)

# Score-plot
#Damit können die über das Jahr auftretenden Hochwasserereignisse gut durch die extremen Abflüsse im Winter reprästentiert werden
plot(PCA_Jahr_HQ$Jahr,pc.cr$scores[,1],type="o", col="blue", xlab = "Jahr", ylab = "Scores")
points(PCA_Jahr_HQ$Jahr, pc.cr$scores[,2], type="o", col="firebrick")
points(PCA_Jahr_HQ$Jahr, pc.cr$scores[,3], type="o", col="forestgreen")
legend(x = 1968, y = 6.5, legend = c("Comp.1","Comp.2", "Comp.3"), lty = c(1,1), 
       col = c("blue","firebrick","forestgreen"), bty = "n", 
       y.intersp = 0.8, cex=0.7)


