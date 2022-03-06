#----------------- Analysing Data of a Temp. and Light Data Logger  -----------
# Nico Wagner
# https://github.com/NicoW1994
# 25.01.2019
#------------------------------------------------------------------------------

# load  required libraries
library(tidyverse)
library(lubridate)
library(zoo)
library(RCurl)

# connect to GitHub dircetory for accessing data
path <- "https://raw.githubusercontent.com/NicoW1994/PortfolioProjects/main/Visualisation%20with%20ggplot/Data%20Logger/"

### 1. Quality control of logged data

# load prepared files for Temperature and Light Intensity from data logger
df1 <- getURL(paste0(path,"10347320.tsv"))
df2 <- read_tsv(df1, col_names = TRUE, skip = 5) %>% 
        filter(date > "2018-12-09") %>%
          mutate(dttm = seq(ymd_hm('2018-12-10 00:00'),
                       ymd_hm('2019-01-06 23:50'), 
                       by = '10 mins'),
                       id = seq(1:length(date)))   # correct id / reset to 1

#------------------------------------------------------------------------------
#--*Figure 1**: Correlation between measured temperature and light intensity
ggplot(data = df2, mapping = aes(x = dttm, y = ta)) +
  geom_point(aes(colour = lux)) +
  labs
#------------------------------------------------------------------------------


#### 1.1 Measeurement range (plausible values)

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

# add an extra column and compute differences between each temperature measurement
df2$dif <- abs(c(1,diff(df2$ta)))
# use if_else condition to mark the flagged values with 1
df3 <- df2 %>%  mutate(qc1 = if_else(dif > 1 | dif < -1, 1, 0))
# check number of flagged values
length(which(df3$qc1 == 1)) # 110 flagged values

#### 1.3 Minimum variability (Persistence qc2)

# In this step we will check if the temperature has not changed during a period of 60 minutes 

df4 <-  mutate(df3, qc2_sum = rollapply(dif, width = 6, FUN = "sum", partial = TRUE, align = "right"),
               qc2 = if_else(qc2_sum == 0, 1, 0))

length(which(df4$qc2 == 1)) # 51 flagged values


#### 2. Flagging system to identify bad data

# After performing four major quality checks, the total flags of each hour are counted.

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
df1 <- df1 %>%  mutate(cycl = if_else(hour >= 6 & hour < 18, "DAY", "NIGHT"),
                       dttm = seq(ymd_hm("2018-12-10 00:00"), ymd_hm("2019-01-06 23:50"), by =  "1 hour"),
                       average = rollapply(th, width = 12, FUN = "mean", align = "right", fill = NA))

#------------------------------------------------------------------------------
# line plot
ggplot(data = df1, mapping = aes(x = dttm, y = th)) + neutral +
  geom_point(mapping = aes(colour = cycl))+
  geom_line(mapping = aes(x = dttm, y = average)) +
  labs(x = "Date", y = "Temperature [Celsius]", colour = "") +
  scale_color_manual(values = c("darkgoldenrod3", "blue3"))
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
#Histogramm
ggplot(data = df1, mapping = aes(th)) +
  geom_histogram(binwidth = 0.5, fill = "azure1", color = "gray48") +
  labs(x = "Temperature [Celsius]")
#------------------------------------------------------------------------------

# create new df for daily values
df2 <- mutate(df1, day = day(dttm)) %>% 
  group_by(date) %>%
  summarise(mean_temp = mean(th)) %>% 
  ungroup()

#------------------------------------------------------------------------------
# Boxplot - Daily temperature distribution 
ggplot(data = df1, mapping = aes(x = date, y = th)) + 
  geom_boxplot(mapping = aes(y = th, group = date), outlier.color = "orangered3") +
  labs(x = "Date", y = "Temperature")
#------------------------------------------------------------------------------

#------------------------------------------------------------------------------
# Heatmap

gradient <- scale_colour_gradient(low = "blue", high = "yellow")

ggplot(data = df1, mapping = aes(x = date, y = hour)) + neutral +
  geom_tile(aes(fill = th), color = "white") +
  scale_fill_continuous(low = "blue", high = "yellow") +
  geom_point(aes(pch = origin), color = "black") +
  labs(x = "Date", y = "Hour", fill = "Temp.", pch = "Origin") +
  scale_shape_manual(values = c(1,16))
#------------------------------------------------------------------------------

# Scatterplot matrix
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

#------------------------------------------------------------------------------
# facet_wrap
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
#------------------------------------------------------------------------------