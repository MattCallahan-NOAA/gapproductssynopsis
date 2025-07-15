# species code validation
# load draft species codes from db
library(odbc)
library(tidyverse)
library(getPass)
library(RJDBC)
library(akfingapdata)

#connect
con <- dbConnect(odbc::odbc(), "akfin", UID=getPass(msg="USER NAME"), PWD=getPass())

# RJDBC
# 
jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../snippets/dbconnect/java/ojdbc8.jar")
# on velvet from a home/user directory
# jdbcDriver <- JDBC(driverClass="oracle.jdbc.OracleDriver", classPath="../../rjdbc_driver/ojdbc8.jar")

con_j <- dbConnect(jdbcDriver, "jdbc:oracle:thin:@akfin", getPass(msg="USER NAME"), getPass())


#query database
species <- dbGetQuery(con_j, paste0("select * from stock_species_code")) %>%
  rename_with(tolower) %>%
  mutate(species_code = as.numeric(species_code))


# pull species from Bridget's files
fishes<-readRDS("common_fishes.RDS")


# add common names for title
token <- create_token("callahan_akfin_api")
txn <- get_gap_taxonomic_groups()

# specify commercially important fishes
fishes_a <- txn %>%
  filter(species_code %in% c(
    30060,
    30420,
    21740,
    21720,
    10120,
    10110,
    10130,
    10261,
    10200,
    10210,
    10285,
    480,
    10112,
    20510,
    10140,
    21921,
    21230,
    30052,
    30020,
    30576,
    10262,
    21347,
    20510))


rf <- c(30150, 30151, 30152, 30535, 30475, 30420, 30100, 30430) %>% data.frame()
names(rf)<-"species_code"

bigfishlist <- species %>%
  full_join(fishes, by="species_code") %>%
  full_join(fishes_a, by="species_code") %>%
  full_join(rf, by="species_code")


# additions
newfishlist <- bigfishlist %>%
  filter(species_code %in% c(30052, 
                             10262,
                             30020,
                             21347,
                             10261)) %>%
  mutate(stock=1,
    species_name = common_name) %>%
  select(species_code, species_name, stock) %>%
    bind_rows(species)

# reupload
dbSendQuery(con_j, "TRUNCATE TABLE stock_species_code")

dbWriteTable(con_j, "STOCK_SPECIES_CODE", newfishlist, append=TRUE)  

species_new <- dbGetQuery(con_j, paste0("select * from stock_species_code")) %>%
  rename_with(tolower) %>%
  mutate(species_code = as.numeric(species_code))

dbDisconnect(con_j)
