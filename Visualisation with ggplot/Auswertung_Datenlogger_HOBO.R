#---
# title: "Excercise 2 - HOBO data quality control"
#---

#library(tidyverse)
#install.packages("tinytex")
#tinytex::install_tinytex()

### 1. Quality control 

# set working directory
path <- "/Users/nico/Documents/VSC/website/Projekte/Datensammler_HOBO/Daten/"

# load packages
library(lubridate)
library(tidyverse)
library(zoo)

# load prepared Hobo files
df1 <- read_tsv(paste0(path,"10347320.tsv"), col_names = TRUE, skip = 5)

df2 <- filter(df1, date > "2018-12-09")    
df2 <- df2 %>% mutate(dttm = seq(ymd_hm('2018-12-10 00:00'),ymd_hm('2019-01-06 23:50'), by = '10 mins'),
                      id = seq(1:length(df2$date)))   # correct id / reset to 1

#--*Figure 1**: Correlation between measured temperature and light intensity
ggplot(data = df2, mapping = aes(x = dttm, y = ta)) +
  geom_point(aes(colour = lux)) +
  labs(x = "Date", y = "Temperature [dgr. Celsius]", colour = "Light intensity [lux]")


#### 1.1 Measeurement range (plausible values)

#The collected data will be compared to the to measurement range of the HOBO logger. 
#The device is able to measure temperatures in a range from - 20°C to + 70°C and light intensity from 0 to 320,000 lux:

#`minT`  = -3.079 °C,  `minL` = 0 lux
#`maxT`  = 22.525 °C,  `maxL` = 46844.8 lux
#`meanT` = 4.419 °C,  `meanL` = 1484.184 lux

# There are no values exceeding the measurement range.

round(min(df2$ta, na.rm = TRUE),3)
round(mean(df2$ta, na.rm = TRUE),3)
round(max(df2$ta, na.rm = TRUE),3)

round(min(df2$lux, na.rm = TRUE),3)
round(mean(df2$lux, na.rm = TRUE),3)
round(max(df2$lux, na.rm = TRUE),3)

#### 1.2 Plausible rate of change (qc1)

# In this step each temperature point will be checked to have not more than 1K temperature change compared to the previous data point. 
# Therefore the temperature differences between each measurement point is calculated and flagged if the value is > 1 or < -1.

# add an extra column and compute differences between each temperature measurement
df2$dif <- abs(c(1,diff(df2$ta)))
# use if_else condition to mark the flagged values with 1
df3 <- df2 %>%  mutate(qc1 = if_else(dif > 1 | dif < -1, 1, 0))
# check number of flagged values
length(which(df3$qc1 == 1)) # 110 flagged values

#Qc1 has a total of 110 flagged values.

#### 1.3 Minimum variability (Persistence qc2)

# In this step we will check if the temperature has not changed during a period of 60 minutes 
# (i.e actual data point $T_i$ plus 5 data points before from $T_{i-1}$ to $T_{i-5}$) and 
# flag the corresponding data points.If there are 6 following temperature differences which equal 0, the data point $T_i$ is flagged. 

df4 <-  mutate(df3, qc2_sum = rollapply(dif, width = 6, FUN = "sum", partial = TRUE, align = "right"),
               qc2 = if_else(qc2_sum == 0, 1, 0))

length(which(df4$qc2 == 1)) # 51 flagged values

# Qc2 has a total of 51 flagged values.


#### 1.4 Maximum variability (Consistency qc3)

#In this step we will check the consistency of the measured temperature values and flag the data 
#if following condition is not fullfilled: $|T_i - T_{i-_1}| + |T_i - T_{i+1}| >= 4 * \sigma_T$
#This means if the standard deviation of the temperature ($\sigma_T$) multiplied by 4, 
#calculated from the previous 6 data points is larger than the sum of the differences to $T_i$, the data point is flagged. 

# Qc3 has a total of 190 flagged values


#### 1.5 Light intensity (qc4)

# check the measured light intensity and flag the corresponding temperature points. 
# First it is necessary to define two thresholds L1 and L2, where L2 > L1. 
#To define these thresholds we have to take a closer look at the values for light intensity:

# min = 0 lux,  
# max = 46844.8 lux, 
# mean = 1484.184 lux

# In total there are 166 values which exceed 10,000 lux, 79 of these are between 46.844 and 20.000 lux.
# To ensure to only flag data points which are influenced by direct sunlight, 
# the quality control was executed with two values for each threshold (L1 = 10000, 20000 ; L2 = 20000, 30000). 
# It turned out that the quality control with higher threshold did miss some of the highly influenced data points, hence L1 = 10000 lux and L2 = 20000 lux was used.

# Qc4 has a total of 254 flagged values


#### 2. Flagging system to identify bad data

# After performing four major quality checks, the total flags of each hour are counted. If one hour of data has one or no flag, the values are aggregated to hourly averages. 
# If there is more than 1 total flag, the data points are replaced with NA´s.
# Qc_final has a total of 89 flagged hours plus 24 missing values for the last day. This mean a total of 113 temperature values have to be replaced.

sigma_all <- mutate(df4, sigma = rollapply(lag(ta, K = 1), width = 6, FUN = "sd", partial = TRUE, align = "right"),
                    sigma4 = 4 * sigma) 

df5 <- mutate(sigma_all, qc3_sum = rollapply(dif, width = 3, FUN = "sum", partial = TRUE, align = "center") - dif,
              qc3 = as.numeric(qc3_sum >  sigma4)) %>% 
  select(id, dttm, ta, lux, qc1, qc2, qc3)

L1  <- 10000
L2  <- 20000

# check on which position condition is true
L_1 <- which(df5$lux > L1)
L_2 <- which(df5$lux > L2)

# Flag temperature value Ti ( and also Ti-1 and Ti+1) if measuered light intensity 
# is higher than L1( i.e 3 values in total are flagged)
# second method with rollapply

# firstly use mutate(ifelse to fill in values for specific condition)
df5 <- df5 %>%  mutate(lux_th1 = if_else(lux > L1, 1, 0),
                       lux_th2 = if_else(lux > L2, 1, 0))

df5 <- df5 %>% mutate(qc4_l1 = rollapply(lux_th1, width = 3, FUN = "sum", align = "center", fill = NA),
                      qc4_l2 = rollapply(lux_th2, width = 7, FUN = "sum", align = "center", fill = NA),
                      qc4_l1 = if_else(qc4_l1 > 0, 1, 0),
                      qc4_l2 = if_else(qc4_l2 > 0, 1, 0),
                      qc4 = if_else(qc4_l2 > 0, 1, qc4_l1)) %>% 
  select(id, dttm, ta, lux, qc1, qc2, qc3, qc4)

# summarise qc columns
df5 <- df5 %>%  mutate(qc_total = rowSums(.[5:8]),
                       qc_total_x = if_else(qc_total > 0, 1, 0)) # add new column for values > 0 (=1)

# aggregate to hours, it is important to ungroup afterwards !
df6 <-  mutate(df5, hr = hour(dttm),
               dat = date(dttm)) %>% 
  group_by(dat, hr) %>%
  summarise(qc = sum(qc_total_x, na.rm = TRUE)) %>%
  ungroup() %>% 
  select(qc)

# check number of flagged hours
length(which(df6$qc > 1)) # 89 values to remove

#### 3. Filling gaps with regression model

# Before replacing the NA's in the HOBO data it is necessary to decide which of the three reference stations in Freiburg are most suitbale.
# At irst the distances between the location of the HOBO logger and the official weather stations of the DWD/WBI is determined. 
# As a second step a linear regression is perfomed with the Hobo data and the data of each station, which leads to the following results:

# check number of flagged hours
length(which(df6$qc > 1)) #  with L1 = 10000, L2 = 20000 89 values to remove 

# aggregate to hours and later remove hours with flags > 1
df7 <-  mutate(df5, hr = hour(dttm),
               dat = date(dttm)) %>% 
  group_by(dat, hr) %>%
  summarise(ta = mean(ta, na.rm = TRUE), lux = mean(lux, na.rm = TRUE)) %>% 
  ungroup()

# combine tibbles
df_hour <- cbind(df7, df6)

# create new sequence for dttm (by "1 hour")
dttm_hr <- seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour")

# add 
df_hour$dttm <- dttm_hr

# check number of flagged values (count Fraction of NA-Values)
length(which(df_hour$qc > 1)) # 89 flagged hours + 1 whole day with NA´s (24) --> total:  113 NA´s

# change temperature values with qc > 1 to NaN
df_hour <- df_hour %>% mutate(ta = if_else(qc > 1, NaN, ta)) %>% 
  select(dttm, ta, lux)

# convert NaN#s to NA's
df_hour$ta[is.nan(df_hour$ta)] <- NA

# check number of NA's again
length(which(is.na(df_hour$ta))) # 113 NA´s

# load in BWI files
WBI <- read_csv2(paste0(path, "Stunde_096.csv"))
str(WBI)

# check length, structure etc.
length(WBI$AVG_TA200) # 672 --> good
summary(WBI)

# add dttm
WBI <- WBI %>% mutate(dttm = seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour")) %>% 
  select(dttm, AVG_TA200)
names(WBI) <- c("dttm", "ta")

# load in DWD, filter for time range and add column with dttm
DWD_urb <- read_delim(paste0(path, "DWD_13667_akt.txt"), delim = ";", col_names = FALSE, skip = 1) %>% 
  mutate(filt = as.numeric(X2)) %>% 
  filter(filt > 2018120923 & filt <= 2019010623) %>% 
  mutate(dttm = seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour"),
         ta = as.numeric(X5)) %>% 
  select(dttm, ta)

# load in DWD Station 1443
DWD <- read_delim(paste0(path, "Station_01443.txt"), delim = ";", col_names = FALSE, skip = 1) %>% 
  mutate(filt = as.numeric(X2)) %>% 
  filter(filt > 2018120923 & filt <= 2019010623) %>% 
  mutate(dttm = seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour"),
         ta = as.numeric(X4)) %>% 
  select(dttm, ta)


# build one data_frame containing all sources
df_reg <- cbind(df_hour, WBI = WBI$ta, DWD_urb = DWD_urb$ta, DWD = DWD$ta)

# put regression data in one tibble for use in rmarkdown
station <- as_tibble(c("WBI", "DWD", "DWD_urban"))
distances_km <- as_tibble(c(3.4, 5, 3.5))
y_intercept <- as_tibble(c(1.140, 1.5137, 0.4845))
slope <- as_tibble(c(0.905, 0.8559, 0.9449 ))
R_2 <- as_tibble(c(0.9729, 0.9582, 0.9604))

regression_dat <- cbind(station, distances_km, y_intercept, slope, R_2)
names(regression_dat) <- c("station", "distance_Hobo [km]", "y_intercept", "slope", "R^2")

# create plot for regression model
par(mfrow=c(1,3),mar=c(4,4,1,0))
par(pty="s")
plot(df_reg$ta, df_reg$DWD_urb, type = "p", col = "orangered", xlab="",ylab="",ylim=c(-5,13),xlim=c(-5,13), main ="DWD_urban")
mtext(side = 2, text = "Temperature [dgr.Celsius]", line = 2.6)

plot(df_reg$ta, df_reg$DWD, type = "p", col = "steelblue3", xlab="",ylab="",ylim=c(-5,13),xlim=c(-5,13), main ="DWD")
mtext(side = 1, text = "Temperature [dgr.Celsius]", line = 3)

plot(df_reg$ta, df_reg$ta, type  = "l", xlab ="",ylab ="",ylim=c(-5,13),xlim=c(-5,13))
abline(1.5137, 0.8559, col = "steelblue3")
abline(0.4845, 0.9449, col = "orangered")
legend("topleft", legend = c("DWD_urban","DWD", "Hobo data"),col =c("orangered","steelblue3","black"), lty =1, bty ="n")

# We can consider a regression model with a slope and $R^2$ close to 1 as perfect, 
# hence the data of the  DWD and the DWD_urb station has a better fit than the WBI station. 
# The linear regression with the DWD data shows the highest $R^2$ but the slope and the distance of the DWD_urban station are significally better. T
# he third plot of figure 2 also shows that the DWD_urban station fits better to the original HOBO data. 
# Therefore the regression model computed with the data from the DWD_urban station is used to fill in the gaps ($y = a + b * x$), where y is the new value. A
# As seen in table 2 the replacement of several flagged values led to an overall temperature decrease, mainly due to the removal of data points directly influenced by sunlight.

# create table
data <- c("original", "quality_checked")
min_t <- as_tibble(c(-3.079, -2.844))
max_t <- as_tibble(c(22.525, 13.173)) 
mean_t <- as_tibble(c(4.558, 4.045))
overview <- cbind(data, min_t, max_t, mean_t)

names(overview) <- c("data", "min_t", "max_t", "mean_t")

#---------------------------------------------------------------------------------------------------
# Part 2
#---------------------------------------------------------------------------------------------------
# set path
library("rio")
indices <- as.tibble(import("https://docs.google.com/spreadsheets/d/1UHox2I07ghbn0XL8W8j6Rwpewl625vPnnqodUE2Eq8M/edit#gid=1013859678")) %>%  
  filter(hobo == 10347320)
#---------------------------------------------------------------------------------------------------
# Part 3: "Graphs with ggplot2"
#---------------------------------------------------------------------------------------------------

### 1. Line plot 

# load devtools
library(devtools)

#Funktion: Neutrales Design fuer ggplot
neutral <- theme_bw() + theme(panel.grid.major = element_blank(),
                              panel.grid.minor = element_blank(),
                              strip.background = element_rect(colour="grey85"),
                              panel.border = element_rect(colour = "grey85")) 

# load in data and add column for day and night 
df1 <- read_tsv(paste0(path,"10347320_Th_2.tsv"), col_names = TRUE)
df1 <- df1 %>%  mutate(cycl = if_else(hour >= 6 & hour < 18, "DAY", "NIGHT"),
                       dttm = seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour"),
                       average = rollapply(th, width = 12, FUN = "mean", align = "right", fill = NA))

# line plot
ggplot(data = df1, mapping = aes(x = dttm, y = th)) + neutral +
  geom_point(mapping = aes(colour = cycl))+
  geom_line(mapping = aes(x = dttm, y = average)) +
  labs(x = "Date", y = "Temperature [Celsius]", colour = "") +
  scale_color_manual(values = c("darkgoldenrod3", "blue3"))

#### 2. Histogramm

ggplot(data = df1, mapping = aes(th)) +
  geom_histogram(binwidth = 0.5, fill = "azure1", color = "gray48") +
  labs(x = "Temperature [Celsius]")

# compare own histogramm to normal distribution
# mean, median and mode should be equal
mean(df1$th) # 4.183
median(df1$th) # 3.3795 
sort(table(df1$th),decreasing=TRUE)[1] # most occuring number = 4.192 --> all 3 are not equal

#### 3. Daily temperature distribution (boxplot)

# create new df for daily values
df2 <- mutate(df1, day = day(dttm)) %>% 
  group_by(date) %>%
  summarise(mean_temp = mean(th)) %>% 
  ungroup()

ggplot(data = df1, mapping = aes(x = date, y = th)) + 
  geom_boxplot(mapping = aes(y = th, group = date), outlier.color = "orangered3") +
  labs(x = "Date", y = "Temperature")

#### Heatmap

gradient <- scale_colour_gradient(low = "blue", high = "yellow")

ggplot(data = df1, mapping = aes(x = date, y = hour)) + neutral +
  geom_tile(aes(fill = th), color = "white") +
  scale_fill_continuous(low = "blue", high = "yellow") +
  geom_point(aes(pch = origin), color = "black") +
  labs(x = "Date", y = "Hour", fill = "Temp.", pch = "Origin") +
  scale_shape_manual(values = c(1,16))

#### Scatterplot matrix
library("rio")
indices <- as.tibble(import("https://docs.google.com/spreadsheets/d/1UHox2I07ghbn0XL8W8j6Rwpewl625vPnnqodUE2Eq8M/edit#gid=1013859678"))

# create additional colum to distinguish own HOBO data from the others
indices <- indices %>%  mutate(owner = if_else(hobo == 10347320, "mine", "others")) 

# use gather to transform data to a long format
indices_g <- indices %>%  gather(key = "variable", value = "value", c(Tamp, Tcv, Tfl, Trap, Tday, Tnht, Tna))


# create vector for facet labels
facet_labs <- as_labeller(c(`Tamp` = "Amplitude", `Tcv` = "Coef. of. Var.", 
                            `Tfl` = "Flashiness", `Trap` = "Max. Td_6hr",
                            `Tday` = "Mean Day T", `Tnht` = "Mean night T", `Tna` = "Fraction NA's"))

# -ggplot2 with facet_wrap()
ggplot(data=indices_g , aes(x =Tavg , y = value )) +
  geom_point(aes(pch = owner, colour = Exposition), size = 2) + 
  facet_wrap(~variable, nrow = 2, labeller = facet_labs) +
  theme_bw()+
  neutral +
  theme(panel.spacing = unit(0,"pt"), strip.background = element_rect(colour="black", fill="aliceblue"),
        strip.text.x = element_text(size=10)) +
  scale_shape_manual(values = c(3,16)) +
  labs(x = "Mean Temperature [Celsius]", y = "Value", pch = "Owner") +
  scale_color_manual(values = c("red3", "steelblue1", "gold", "forestgreen"))