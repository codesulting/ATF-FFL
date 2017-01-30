# ATF - Federal Firearms Licenses
# import and binding data

# https://www.atf.gov/firearms/listing-federal-firearms-licensees-ffls-2015

# load data -------------------------------------------------------------------

library(readxl)

# create a list of files for each directory
# loop through each directory, reading the .xlsx files
# add columns for month and year
# bind each .xlsx file into it's own dataframe

# read and bind data by year --------------------------------------------------

# 2016 ------------------------------------------------------------------------
# Sept (09) and Oct (10) are not available

# set directory to read data from
y2016_dir <- "~/Documents/ATF-FFL/data/2016"
setwd(y2016_dir)

# create a list of .xlsx files to read in
y2016_list <- list.files(path = y2016_dir, pattern = ".xlsx", recursive = T, all.files = T)

# initialize a dataframe to store imported data
y2016 <- data.frame()

# loop to read in data and bind

system.time(
for(i in 1:length(y2016_list)) {
  
  temp <- read_excel(y2016_list[i])
  
  # remove first row of excel-formatted dashes
  temp <- temp[-1, ]
  
  # initialize `Expire Date` variable
  # this column doesnt appear in months 01-07 but starts to after 08
  temp$`Expire Date` <- ""

  # add year and month columns
  temp$year <- as.factor("2016")
  temp$month <- paste(y2016_list[i])
  temp$month <- gsub(".xlsx", "", temp$month)
  temp$month <- gsub("16", "", temp$month)
  
  # bind
  y2016 <- rbind(temp, y2016)
  
}
)

#    user  system elapsed 
#  15.987   2.627  18.612 

rm(temp)
rm(y2016_dir)
rm(y2016_list)

# 2015 ------------------------------------------------------------------------
# Sept (09) and Oct (10) are not available

y2015dir <- "~/Documents/ATF-FFL/data/2015"
setwd(y2015dir)

y2015.list <- list.files(path = y2015dir, pattern = ".xlsx", recursive = T, all.files = T)
y2015 <- data.frame()

system.time(
for(i in 1:length(y2015.list)) {
  
  temp <- read_excel(y2015.list[i])
  temp <- temp[-1, ]
  temp$`Expire Date` <- ""
  
  temp$year <- as.factor("2015")
  temp$month <- paste(y2015.list[i])
  temp$month <- gsub(".xlsx", "", temp$month)
  temp$month <- gsub("15", "", temp$month)
  
  y2015 <- rbind(temp, y2015)
  
}
)

#    user  system elapsed 
#  16.990   2.628  19.650

rm(temp)
rm(y2015dir)
rm(y2015.list)

# 2014 ------------------------------------------------------------------------
# May (05) and July (07) are not available

y2014dir <- "~/Documents/ATF-FFL/data/2014"
setwd(y2014dir)

y2014.list <- list.files(path = y2014dir, recursive = T, all.files = T)
y2014 <- data.frame()

system.time(
  for(i in 1:length(y2014.list)) {
    
    temp <- read_excel(y2014.list[i])
    temp <- temp[-1, ]
    temp$`Expire Date` <- ""
    
    temp$year <- as.factor("2014")
    temp$month <- paste(y2014.list[i])
    temp$month <- gsub(".xlsx", "", temp$month)
    temp$month <- gsub(".xls", "", temp$month)
    temp$month <- gsub("14", "", temp$month)
    
    y2014 <- rbind(temp, y2014)
    
  }
)

#    user  system elapsed 
#  27.470   3.630  31.112

rm(temp)
rm(y2014dir)
rm(y2014.list)
rm(i)

# 2013 ------------------------------------------------------------------------
# Oct (10) is not available
# April (04) appears to be corrupt. Cross checked with Google sheets

y2013dir <- "~/Documents/ATF-FFL/data/2013"
setwd(y2013dir)

y2013.list <- list.files(path = y2013dir, pattern = ".xls", recursive = T, all.files = T)
y2013 <- data.frame()

system.time(
  for(i in 1:length(y2013.list)) {
    
    temp <- read_excel(y2013.list[i])
    temp <- temp[-1, ]
    temp$`Expire Date` <- ""
    
    temp$year <- as.factor("2013")
    temp$month <- paste(y2013.list[i])
    temp$month <- gsub(".xls", "", temp$month)
    temp$month <- gsub("13", "", temp$month)
    
    y2013 <- rbind(temp, y2013)
    
  }
)

#    user  system elapsed 
#  34.426   4.221  38.660 

rm(temp)
rm(y2013dir)
rm(y2013.list)
rm(i)

# bind all years --------------------------------------------------------------

setwd("~/Documents/ATF-FFL")

ATF_FFL <- rbind(y2013, y2014, y2015, y2016)


test15_16 <- rbind(y2015, y2016)

# rename columns
colnames(ATF_FFL) <- c("Region", "District", "County", "Type", "Expiration", "Seqn",
                       "LicenseName", "BusinessName", "PremiseStreet", "PremiseCity",
                       "PremiseState", "PremiseZIP", "MailStreet", "MailCity", "MailState",
                       "MailZIP", "Phone", "ExpireDate", "year", "month")

write.csv(ATF_FFL, file = "data/ATF_FFL.csv", row.names = F)

# each year
write.csv(y2013, file = "data/ffl-2013.csv", row.names = F)
write.csv(y2014, file = "data/ffl-2014.csv", row.names = F)
write.csv(y2015, file = "data/ffl-2015.csv", row.names = F)
write.csv(y2016, file = "data/ffl-2016.csv", row.names = F)
