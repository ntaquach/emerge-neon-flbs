## This is an R script to download discharge (Q), water temperature, and dissolved oxygen data for NEON streams.
## The data will be formatted for use with streamMetabolizer.

## Final discharge units should be cubic meters per second (they are downloaded as liters per second).
## Final temperature units should be degrees C.
## Final dissolved oxygen units should be mg/L.




###############################################################################
# Step 0. Load R libraries and set the working directory.
###############################################################################

# Start by loading (and/or install) the three required R packages: 'neonUtilities', 'tidyverse', and 'missForest'.

   library(neonUtilities)
   library(tidyverse)
   library(missForest)
   


# Next, set the R working directory. 

   # setwd('copy your working directory path here')	# Update this path.




###############################################################################
# Step 1. Specify parameter inputs that will be used in this script.
###############################################################################

# Assign your NEON token value to a new 'NEONtoken' variable.

NEONtoken <-	'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0aWVuYW5ocXVhY2huZ3V5ZW5AZ21haWwuY29tIiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxOTE1NjMwOTc3LCJpYXQiOjE3NTc5NTA5NzcsImVtYWlsIjoidGllbmFuaHF1YWNobmd1eWVuQGdtYWlsLmNvbSJ9.jypPyKBBBuWIVDH4YGv3UlZeIUtMmFOkebSWYAcEOFHyEe37bXfj42UmuE0NRMKy4DVBl5YFF1YJQ5ezFZJVvw'



# Specify the NEON site to download (e.g., SiteCode <- 'ARIK').
# To download all sites at once, use 'all' (i.e., site = 'all').

   SiteCode <- 'LECO'	# Update this line.



# Specify the YEAR and MONTH of the Start Date and End Date for the data records you will download.
# Use YYYY-MM format (e.g., '2018-10').
# To download ALL dates, set StartDate and EndDate to 'NA'.
  
   StartDate <- '2022-09'
  
   EndDate <- '2022-09'




###############################################################################
# Step 2. Download and format Discharge data.
###############################################################################

# Specify the NEON data product to download.
# Continuous discharge is data product 'DP4.00130.001'.
# NOTE: NEON Discharge units are Liters per Second.

   DataProd <- 'DP4.00130.001'



# Use the 'loadByProduct' function from the 'neonUtilities' package to download the NEON data.

   NEONdata <- loadByProduct(dpID = DataProd, site = SiteCode, startdate = StartDate, enddate = EndDate, 
  
							 token = NEONtoken, package = 'basic', check.size = F,
				
							 include.provisional = T, nCores = parallel::detectCores())



# Based on the NEON data product documentation, specify which of the sub-lists has the data you need.
# The sub-list for Discharge is 'csd_continuousDischarge'.

   SubList <- 'csd_continuousDischarge'



# Based on the NEON data product documentation, specify which specific environmental variable you need.
# The environmental variable for Discharge is 'maxpostDischarge'.

   EnvVar <- 'maxpostDischarge'



# Based on the NEON data product documentation, specify which specific QA/QC ('finalQF') variable you need.
# The QA/QC variable for Discharge is 'dischargeFinalQF'.

   QAflagVar <- 'dischargeFinalQF'



# Extract the SubList from the [NEONdata] dataframe and save as new [EnvData] dataframe.
  
   EnvData <- NEONdata[[SubList]]



# Detect the exact name of the 'endDate' variable in [EnvData].
# Use the 'dpylr' functions 'select' and 'contains'.

   DateVar <- EnvData %>% select(contains('endDate'))

   DateVar <- colnames(DateVar)



# Subset the 'siteID', DateVar, EnvVar, and QAflagVar columns for further analysis.

   EnvData <- EnvData[c('siteID', DateVar, EnvVar, QAflagVar)]



# Rename the 'QAname' column (the 4th column) as 'QAcheck'.

   colnames(EnvData)[4] <- 'QAcheck'



# Change observations that did not pass NEON QA/QC procedure (indicated by 'QAcheck = 1') to 'NA'.
# Change observations that passed QA/QC to '1'. OR Converting 0 to 1, and converting 1 to NA!
   
   EnvData <- EnvData %>% mutate(QAcheck = ifelse(QAcheck == 1, NA, 1))
   
   

# Multiply [EnvVar] values by QA flags to convert all failed QA checks to 'NA' values.

   EnvData[[EnvVar]] <- EnvData[[EnvVar]] * EnvData$QAcheck #times NA will be NA, times 1 will still be 1, that is why we needed to change on line 131



# Copy 'endDate' to new 'Date' column as character values.
   
   EnvData$Date <- as.character(EnvData$endDate)



# Create new 'Hour10Min' column to use for indexing 10 minute intervals.

   EnvData$Hour10Min <- substr(EnvData$Date, 12, 15)
   
   EnvData$Hour10Min[EnvData$Hour10Min == ''] <- '00:0'



# Remove hr:min:sec from 'Date'.
   
   EnvData$Date <- substr(EnvData$Date, 1, 10)



# Create a new time-series with 10-minute intervals and save as a new 'Data10discharge' dataframe.
# Use 'dplyr' function 'group_by' to calculate 10-minute average values from the original data.
# Here we use some special notation (.data[[]], !!, :=) to pass variables to the dplyr functions.

   Data10discharge <- EnvData %>% group_by(Date, Hour10Min) %>% summarize(disc = mean(.data[[EnvVar]]))
   


# Create new 'DateTime' column.

   Data10discharge$DateTime <- paste0(Data10discharge$Date, " ", Data10discharge$Hour10Min, "0:01")


 
# Convert 'DateTime' from character to date (POSIXct) format.

   Data10discharge$DateTime <- as.POSIXct(Data10discharge$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")



# Convert 'DateTime' values to UNIX time and save as new variable 'unixtime'.

   Data10discharge$unixtime <- as.numeric(Data10discharge$DateTime)



# Remove unnecessary columns.

   Data10discharge <- Data10discharge[, c('unixtime', 'disc')]



# Convert discharge units from liters-per-second to cubic meters-per-second.

   Data10discharge$disc <- Data10discharge$disc * 0.001




###############################################################################
# Step 3. Download Temperature data.
###############################################################################

# Specify the NEON data product to download.
# Surface Water Temperature is data product 'DP4.00130.001'.
# NOTE: NEON Surface Water Temperature units are Degrees Celsius.

   DataProd <- 'DP1.20054.001'



# Use the 'loadByProduct' function from the 'neonUtilities' package to download the NEON data.

   NEONdata <- loadByProduct(dpID = DataProd, site = SiteCode, startdate = StartDate, enddate = EndDate, 
  
							 token = NEONtoken, package = 'basic', check.size = F,
				
							 include.provisional = T, nCores = parallel::detectCores())



# Based on the NEON data product documentation, specify which of the sub-lists has the data you need.
# The sub-list for Water Temperature is 'TOSW_5_min'.

   SubList <- 'TOSW_5_min'



# Based on the NEON data product documentation, specify which specific environmental variable you need.
# The environmental variable for Water Temperature is 'surfacewaterTempMean'.

   EnvVar <- 'surfacewaterTempMean'



# Based on the NEON data product documentation, specify which specific QA/QC ('finalQF') variable you need.
# The QA/QC variable for Water Temperature is 'sWatTempFinalQF'.

   QAflagVar <- 'sWatTempFinalQF'



# Extract the SubList from the [NEONdata] dataframe and save as new [EnvData] dataframe.
  
   EnvData <- NEONdata[[SubList]]



# Detect the exact name of the 'endDate' variable in [EnvData].
# Use the 'dpylr' functions 'select' and 'contains'.

   DateVar <- EnvData %>% select(contains('endDate'))

   DateVar <- colnames(DateVar)



# Subset the 'siteID', DateVar, EnvVar, and QAflagVar columns for further analysis.

   EnvData <- EnvData[c('siteID', DateVar, EnvVar, QAflagVar)]



# Rename the 'QAname' column (the 4th column) as 'QAcheck'.

   colnames(EnvData)[4] <- 'QAcheck'



# Change QA flags for observations that did not pass NEON QA/QC procedure (indicated by 'QAcheck = 1') to 'NA'.
# Change QA flags for observations that passed QA/QC to '1'.
   
   EnvData <- EnvData %>% mutate(QAcheck = ifelse(QAcheck == 1, NA, 1))
   
   

# Multiply [EnvVar] values by QA flags to convert all failed QA checks to 'NA' values.

   EnvData[[EnvVar]] <- EnvData[[EnvVar]] * EnvData$QAcheck



# Copy 'endDate' to new 'Date' column as character values.
   
   EnvData$Date <- as.character(EnvData$endDate)



# Create new 'Hour10Min' column to use for indexing 10 minute intervals.

   EnvData$Hour10Min <- substr(EnvData$Date, 12, 15)
   
   EnvData$Hour10Min[EnvData$Hour10Min == ''] <- '00:0'



# Remove hr:min:sec from 'Date'.
  
   EnvData$Date <- substr(EnvData$Date, 1, 10)



# Create a new time-series with 10-minute intervals and save as a new 'Data10temperature' dataframe.
# Use 'dplyr' function 'group_by' to calculate 10-minute average values from the original data.
# Here we use some special notation (.data[[]], !!, :=) to pass variables to the dplyr functions.

   Data10temperature <- EnvData %>% group_by(Date, Hour10Min) %>% summarize(temp = mean(.data[[EnvVar]]))
   


# Create new 'DateTime' column.

   Data10temperature$DateTime <- paste0(Data10temperature$Date, " ", Data10temperature$Hour10Min, "0:01")



# Convert 'DateTime' from character to date (POSIXct) format.

   Data10temperature$DateTime <- as.POSIXct(Data10temperature$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")



# Convert 'DateTime' values to UNIX time and save as new variable 'unixtime'.

   Data10temperature$unixtime <- as.numeric(Data10temperature$DateTime)



# Remove unnecessary columns.

   Data10temperature <- Data10temperature[, c('unixtime', 'temp')]




###############################################################################
# Step 4. Download Dissolved Oxygen data.
###############################################################################

# Specify the NEON data product to download.
# Dissolved Oxygen is included in data product 'DP1.20288.001'.
# NOTE: NEON Dissolved Oxygen units are Milligrams per Liter.

   DataProd <- 'DP1.20288.001'



# Use the 'loadByProduct' function from the 'neonUtilities' package to download the NEON data.

   NEONdata <- loadByProduct(dpID = DataProd, site = SiteCode, startdate = StartDate, enddate = EndDate, 
  
							 token = NEONtoken, package = 'basic', check.size = F,
				
							 include.provisional = T, nCores = parallel::detectCores())



# Based on the NEON data product documentation, specify which of the sub-lists has the data you need.
# The sub-list for Dissolved Oxygen is 'waq_instantaneous'.

   SubList <- 'waq_instantaneous'



# Based on the NEON data product documentation, specify which specific environmental variable you need.
# The environmental variable for Dissolved Oxygen is 'dissolvedOxygen'.

   EnvVar <- 'dissolvedOxygen'



# Based on the NEON data product documentation, specify which specific QA/QC ('finalQF') variable you need.
# The QA/QC variable for Dissolved Oxygen is 'dissolvedOxygenFinalQF'.

   QAflagVar <- 'dissolvedOxygenFinalQF'



# Extract the SubList from the [NEONdata] dataframe and save as new [EnvData] dataframe.
  
   EnvData <- NEONdata[[SubList]]



# Detect the exact name of the 'endDate' variable in [EnvData].
# Use the 'dpylr' functions 'select' and 'contains'.

   DateVar <- EnvData %>% select(contains('endDate'))

   DateVar <- colnames(DateVar)



# Subset the 'siteID', DateVar, EnvVar, and QAflagVar columns for further analysis.

   EnvData <- EnvData[c('siteID', DateVar, EnvVar, QAflagVar)]



# Rename the 'QAname' column (the 4th column) as 'QAcheck'.

   colnames(EnvData)[4] <- 'QAcheck'



# Change QA flags for observations that did not pass NEON QA/QC procedure (indicated by 'QAcheck = 1') to 'NA'.
# Change QA flags for observations that passed QA/QC to '1'.
   
   EnvData <- EnvData %>% mutate(QAcheck = ifelse(QAcheck == 1, NA, 1))
   
   

# Multiply [EnvVar] values by QA flags to convert all failed QA checks to 'NA' values.

   EnvData[[EnvVar]] <- EnvData[[EnvVar]] * EnvData$QAcheck



# Copy 'endDate' to new 'Date' column as character values.
   
   EnvData$Date <- as.character(EnvData$endDate)



# Create new 'Hour10Min' column to use for indexing 10 minute intervals.

   EnvData$Hour10Min <- substr(EnvData$Date, 12, 15)
   
   EnvData$Hour10Min[EnvData$Hour10Min == ''] <- '00:0'



# Remove hr:min:sec from 'Date'.
  
   EnvData$Date <- substr(EnvData$Date, 1, 10)



# Create a new time-series with 10-minute intervals and save as a new 'Data10oxygen' dataframe.
# Use 'dplyr' function 'group_by' to calculate 10-minute average values from the original data.
# Here we use some special notation (.data[[]], !!, :=) to pass variables to the dplyr functions.

   Data10oxygen <- EnvData %>% group_by(Date, Hour10Min) %>% summarize(oxy = mean(.data[[EnvVar]]))
   


# Create new 'DateTime' column.

   Data10oxygen$DateTime <- paste0(Data10oxygen$Date, " ", Data10oxygen$Hour10Min, "0:01")



# Convert 'DateTime' from character to date (POSIXct) format.

   Data10oxygen$DateTime <- as.POSIXct(Data10oxygen$DateTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")



# Convert 'DateTime' values to UNIX time and save as new variable 'unixtime'.

   Data10oxygen$unixtime <- as.numeric(Data10oxygen$DateTime)



# Remove unnecessary columns.

   Data10oxygen <- Data10oxygen[, c('unixtime', 'oxy')]




###############################################################################
# Step 5. Combine the save the temperature, DO, and discharge time-series.
###############################################################################

   UnixTimeSeries <- merge(Data10temperature, Data10oxygen, by = 'unixtime')
   
   UnixTimeSeries <- merge(UnixTimeSeries, Data10discharge, by = 'unixtime')
   
   write.csv(UnixTimeSeries, './RandomForest/Temp_DO_Discharge.csv', row.names = FALSE)



###############################################################################
# Step 6. Inspect time-series plots of the three variables.
###############################################################################

   PlotTemp <- ggplot(UnixTimeSeries, aes(x = unixtime, y = temp)) + geom_line()
   
   PlotTemp
   
   PlotDO <- ggplot(UnixTimeSeries, aes(x = unixtime, y = oxy)) + geom_line()
   
   PlotDO
   
   PlotDisc <- ggplot(UnixTimeSeries, aes(x = unixtime, y = disc)) + geom_line()
   
   PlotDisc



###############################################################################
# Step 7. Use random forest imputation to fill in NA values.
###############################################################################

### Only run Step 6 if necessary!

# Check for 'NA' values in the Temp, DO, and Q time series.

   summary(UnixTimeSeries)



# Convert 'unixtime' to 'Date'.

   UnixTimeSeries$Date <- as_datetime(UnixTimeSeries$unixtime)
   


# Break out new 'Month', 'Day', and 'Time' columns to aid in random forest interpolation.

   UnixTimeSeries$Month <- as.numeric(substr(UnixTimeSeries$Date, 6, 7))
   
   UnixTimeSeries$Day <- as.numeric(substr(UnixTimeSeries$Date, 9, 10))
   
   UnixTimeSeries$Time <- substr(UnixTimeSeries$Date, 12, 16)
   
   UnixTimeSeries$Time <- as.numeric(hm(UnixTimeSeries$Time))



# Use 'missForest' function ('missForest' library) to impute NA values. 

   UnixTimeSeriesImpNA <- UnixTimeSeries[, c('unixtime', 'temp', 'oxy', 'disc', 'Month', 'Day', 'Time')]
   
   UnixTimeSeriesImpNA <- missForest(UnixTimeSeriesImpNA)

   UnixTimeSeriesImpNA$OOBerror
   

# Clean up and save the new imputed data.

   UnixTimeSeriesImpNA2 <- UnixTimeSeriesImpNA$ximp
   
   UnixTimeSeries <- UnixTimeSeriesImpNA2[, 1:4]
   
   summary(UnixTimeSeries)
   
   write.csv(UnixTimeSeries, './RandomForest/Temp_DO_Discharge.csv', row.names = FALSE)



### End of code.