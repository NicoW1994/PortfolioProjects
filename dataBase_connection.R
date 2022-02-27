
#--------------Function to connect to MySQL or PostgreSQL Database-------------
# Nico Wagner
# https://github.com/NicoW1994
# 26.02.2022
#------------------------------------------------------------------------------

# Arguments needed to call function
# 1. Argument: driver (MySQL, PostgreSQL)
# 2 .Argument: db (database name)
# 3. Argument: host (host name)
# 4. Argument: port
# 5. Argument: user (user name)
# 6. Argument: password (optional)

# Disclaimer
# if error appears check: 
#               show global variables like 'local_infile';
# if turned off: 
#               set global local_infile=true;

#------------------------------------------------------------------------------
# create function
connect_database <- function(driver, db, host, port, user, password=NULL){

#------------------------------------------------------------------------------
# check, install and load necessary packages
if (!("DBI" %in% installed.packages())){
    install.packages("DBI")
}
if (!("RMySQL" %in% installed.packages())){
  install.packages("RMySQL")
}
if (!("RPostgreSQL" %in% installed.packages())){
  install.packages("RPostgreSQL")
}

require(DBI)
require(RMySQL)
require(RPostgreSQL)

#------------------------------------------------------------------------------
# load required functions
# kill all connections
killDbConnections <- function () {

  all_cons <- dbListConnections(dbDriver(driver))

  print(all_cons)

  for(con in all_cons)
    +  dbDisconnect(con)

  print(paste(length(all_cons), " connections killed."))

}

#------------------------------------------------------------------------------
# check for existing connections
if (length(dbListConnections(MySQL()))>0){
  answer <- readline(prompt="Existing connections found. Kill all connections ? ")
  if (answer == "Yes"){
  killDbConnections()
  } else {
    print("multiple connections open")
}} else {
  print("no pre-existing connections found")
 }

#------------------------------------------------------------------------------
# connect to database
con <- DBI::dbConnect(DBI::dbDriver(driver),
        db       = db,
        host     = host,
        port     = port,
        user     = user,
         if (is.null(password)){
         password = rstudioapi::askForPassword(paste0("Database user: ",user))
         } else {
        password = password
      }
)

#------------------------------------------------------------------------------
# check database for list of tables
if (length(DBI::dbListTables(con) > 0)){
    print(paste0("successfull connection to database: ", db, " number of tables = ",length(DBI::dbListTables(con))))
 }else {
  print("no tables found")
 }

# return statement to safe connection object outside of the function
return(con)

}
# END
                  