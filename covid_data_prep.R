#-------------- Preparation of datasets -------------
# Nico Wagner
# https://github.com/NicoW1994
# 26.02.2022
#------------------------------------------------------------------------------

# access function to connect to database
library(devtools)
source_url("https://raw.githubusercontent.com/NicoW1994/PortfolioProjects/main/dataBase_connection.R")

# call function: connect_database
con <- connect_database(driver = "MySQL", db ="covid_data", host ="127.0.0.1", port = 3306, 
                        user = "root", password = NULL)

# list available tables
dbListTables(con)

# check for existing connections
dbListConnections(MySQL())

# kill connections if necessary
# killDbConnections()

# set path to load in raw data
path <- ("/Users/nico/Documents/VSC/website/Projekte/Pandemic/")
setwd(path)

#------------------------------------------------------------------------------
# import and clean csv. files to write into database
covid_data <- read.csv(paste0(path,"owid-covid-data.csv"), na.strings = c("", NA))

# Relocate Data and drop columns with information about tests/vaccines
covid_deaths <- relocate(covid_data, population, .after = date) %>%
   dplyr::select(-c(27:ncol(covid_data)))

covid_vaccines <- dplyr::select(covid_data, -c(5:26))

#------------------------------------------------------------------------------
# write new tables into the database
# if error appears check: show global variables like 'local_infile';
# if turned off: set global local_infile=true;
 if ((dbWriteTable(con, "covid_deaths" ,covid_deaths, row.names = F, overwrite = T, na = )) == TRUE){
     print("datbase succesfully updated")
}

 if ((dbWriteTable(con, "covid_vaccines" ,covid_vaccines, row.names = F, overwrite = T)) == TRUE){
   print("datbase succesfully updated")
 }
