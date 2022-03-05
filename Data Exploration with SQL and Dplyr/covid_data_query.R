#----------------------------- query covid database ---------------------------------
# Nico Wagner
# https://github.com/NicoW1994
# 01.03.2022
#------------------------------------------------------------------------------

# load necessary packages
library(devtools)

require(tidyverse)
require(lubridate)
require(zoo)

require(queryparser)
require(tidyquery)

#------------------------------------------------------------------------------

# access function to connect to database: connect_database()
source_url("https://raw.githubusercontent.com/NicoW1994/PortfolioProjects/main/dataBase_connection.R")

# Arguments:
# driver <- "MySQL"
# driver <- "PostgreSQL"

# db <- "covid_data"
# db <- "db_a834e2_covid"

# host <- "127.0.0.1"
# host <- "mysql5047.site4now.net"

# port <- 3306

# user <- "root"
# user <- "a834e2_covid"

# password <- "Platzhalter$1994!"
# password <- "mysql1994"

#------------------------------------------------------------------------------
# call function: connect_database()
con <- connect_database(driver = "MySQL", db ="covid_data", host ="127.0.0.1", port = 3306, 
                        user = "root", password = "Platzhalter$1994!")

# list available tables
dbListTables(con)

# does table exist ?
# dbExistsTable(con, dbListTables(con)[1])

# remove tables if neccessary
# dbRemoveTable(conn = con, (dbListTables(con)[1]))

# kill connection if necessary
# dbDisconnect(con)

#------------------------------------------------------------------------------
# query data
covid_deaths <- tbl(con, "covid_deaths")
covid_vaccines <- tbl(con, "covid_vaccines")

# query for joining deaths and vaccination tables. Selects only needed columns for further analysis
q1 <- covid_deaths %>%  
          filter(!is.null(continent)) %>% 
            left_join(covid_vaccines, by = c("location", "date")) %>% 
              select(continent.x, location, date, population.x, total_cases, new_cases, total_deaths, new_deaths,
                     total_vaccinations, people_fully_vaccinated, new_vaccinations)


# query to group and summarise by location and month
q2 <- q1 %>%
        mutate(Year = substring(date, 1, 4), 
               Month = substring(date, 6, 7),
               year_month = paste(Year, Month, "01" ,sep = "-")) %>% 
          group_by(location, year_month) %>% 
            summarise(deaths = sum(new_deaths,na.rm = T),
                      cases = sum(new_cases, na.rm = T),
                      vaccinations = sum(new_vaccinations, na.rm = T))

# collect data from database to R Environment
covid_deaths_q2 <- collect(q2)

# check structure and summary
str(covid_deaths_q2)
summary(covid_deaths_q2)

#year_month has to be transformed into date type
covid_deaths_q2 <- covid_deaths_q2 %>% mutate(date_year_month = as.yearmon(year_month))

# check data structure
head(covid_deaths_q2)
tail(covid_deaths_q2)

# Save single object for further use outside of the script
saveRDS(covid_deaths_q2, "covid_data_monthly.rds")
#------------------------------------------------------------------------------

