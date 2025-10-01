## This is an R script to practice some basic Pivot Table functions.

# One R package is needed: 'tidyverse'.

  install.packages("tidyverse")

  library(tidyverse)

  rm(list=ls())
  

#-----------------------------------------------------------------
# Step 1. Set the R working directory then load the [Fish_TestData.csv] table.
#-----------------------------------------------------------------

  # setwd("D:/Dropbox/NEON_DataViz_2023/Data")	# Update this path.


# The 'Fish_TestData.csv' data are 3-pass depletion electrofishing samples from 23 of the NEON streams.

  TestData <- read.csv("./JoinsPivots/Fish_TestData.csv", na.strings = "", stringsAsFactors = F)



#-----------------------------------------------------------------
# Step 2. Get some practice using basic Pivot Table functions to change data dimensions.
#-----------------------------------------------------------------


# Calculate the average of fishTotalLength for each Species at each Site.
# Use the 'group_by' and 'summarize' functions from the 'tidyverse' package.
# NOTE: there are lots of summary stats (median, max, sd, IQR, count, etc.) that can be called within the 'summarize' function.

  CoolStuff1 <- TestData %>% group_by(scientificName,siteID) %>% summarize(MeanTotalLength = mean(fishTotalLength))



# Convert the stacked data to a traditional site/sample x taxa matrix, where cell values are mean total lengths.
# First delete unnecessary columns from [TestData] and save as a new [TestData2] dataframe.
# Then use the 'group_by' and 'pivot_wider' functions with 'values_fn = mean'.
# Finally, use nested 'select' and 'sort' functions ('tidyverse') to arrange columns in alphabetical order.

  TestData2 <- TestData %>% select('siteID', 'scientificName', 'fishTotalLength')

  CoolStuff2 <- TestData2 %>% group_by(siteID) %>% pivot_wider(names_from = scientificName, values_from = fishTotalLength, values_fn = mean)

  CoolStuff2 <- CoolStuff2 %>% select('siteID', sort(colnames(.)))



# Rebuild the [CoolStuff2] dataframe (save as [CoolStuff3]), but specifying species' counts instead of mean lengths.

  TestData3 <- TestData %>% select('siteID', 'scientificName')
  
  CoolStuff3 <- TestData3 %>% group_by(siteID) %>% pivot_wider(names_from = scientificName, values_from = scientificName, values_fn = list(scientificName = length))

  CoolStuff3 <- CoolStuff3 %>% select('siteID', sort(colnames(.)))



# Filter the data to include only the first electrofishing pass collected during each sampling event.
# Start by going back to the original data and first subsetting ('filter' function) only the first pass.
# Then repeat the steps above for [CoolStuff3].
  
  CoolStuff4 <- filter(TestData, passNumber == 1)
  
  CoolStuff4 <- CoolStuff4 %>% select('siteID', 'scientificName')
  
  CoolStuff4 <- CoolStuff4 %>% group_by(siteID) %>% pivot_wider(names_from = scientificName, values_from = scientificName, values_fn = list(scientificName = length))
  
  CoolStuff4 <- CoolStuff4 %>% select('siteID', sort(colnames(.)))



#-----------------------------------------------------------------
# End of code.
#-----------------------------------------------------------------