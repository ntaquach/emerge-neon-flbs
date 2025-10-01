## This is an R script to:
## 1) Download NEON BENTHIC INVERTEBRATE data for MULTIPLE study streams.
## 2) Convert the raw NEON invertebrate data to a Genus x Site Assemblage Matrix of INSECT data (note: only class Insecta specimens are included).
## 3) Perform Nonmetric Multidimensional Scaling (NMDS) for the Insect Assemblage Matrix.

# 5 R packages are needed from CRAN: 'neonUtilities', 'remotes', 'tidyverse', 'vegan', 'shipunov', and 'geometry'.


  
#-----------------------------------------------------------------
# Step 0. Install and load the necessary R packages.
#-----------------------------------------------------------------

# Now install and/or load the 5 R packages from CRAN.

  install.packages(c('neonUtilities', 'remotes', 'tidyverse', 'vegan', 'ggrepel'))

  library(neonUtilities)
  library(remotes)
  library(tidyverse)
  library(vegan)
  library(ggrepel)

rm(list=ls())


#-----------------------------------------------------------------
# Step 1. Create a new R working directory.
#-----------------------------------------------------------------

# First set the initial working directory.

  #setwd({'paste your working directory path here & delete curly brackets'})	# Update this path.


# Assign your NEON token value to a new 'NEONtoken' variable.

  NEONtoken <- 'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJ0aWVuYW5ocXVhY2huZ3V5ZW5AZ21haWwuY29tIiwic2NvcGUiOiJyYXRlOnB1YmxpYyIsImlzcyI6Imh0dHBzOi8vZGF0YS5uZW9uc2NpZW5jZS5vcmcvIiwiZXhwIjoxOTE1NjMwOTc3LCJpYXQiOjE3NTc5NTA5NzcsImVtYWlsIjoidGllbmFuaHF1YWNobmd1eWVuQGdtYWlsLmNvbSJ9.jypPyKBBBuWIVDH4YGv3UlZeIUtMmFOkebSWYAcEOFHyEe37bXfj42UmuE0NRMKy4DVBl5YFF1YJQ5ezFZJVvw'	# Update this line.


# Specify the appropriate 4-letter NEON site code(s) (e.g.,'ARIK') to download and assign it to a new 'SiteCode' variable.
# All NEON sites are listed on the website:
# https://www.neonscience.org/field-sites/explore-field-sites
# For this exercise, we'll choose ALL sites (SiteCode <- 'all').
# To download all sites at once, use 'all'.

  SiteCode <- 'all'



#-----------------------------------------------------------------
# Step 2. Download NEON invert data for a single site, including multiple sampling events.
#-----------------------------------------------------------------

# Specify the YEAR and MONTH of the Start Date and End Date for the data records you will download.
# Use YYYY-MM format (e.g., '2018-10').
# To download ALL dates, set StartDate and EndDate to 'NA'.
  
  StartDate <- '2023-09'
  
  EndDate <- '2023-12'


# Download the NEON benthic invertebrate data.
# Benthic invertebrate samples are NEON data product 'DP1.20120.001' {i.e., dpID = 'DP1.20120.001'}.
# Multiple optional parmaters (e.g., specify years, months, etc.) can be specified. Check instructions at:
# https://www.neonscience.org/neonDataStackR

#   zipsByProduct(dpID = 'DP1.20120.001', site = SiteCode, startdate = StartDate, enddate = EndDate, 
#   
# 				package = 'basic', check.size = F, token = NEONtoken, include.provisional = T)
# 
# 
#   stackByTable(paste0(getwd(), '/filesToStack20120'), folder = T, saveUnzippedFiles = F, nCores = parallel::detectCores())


invertdata <- loadByProduct(dpID = "DP1.20120.001", site = SiteCode,
                            startdate = StartDate, enddate = EndDate,
                            package = "expanded", check.size = F,
                            token = NEONtoken, include.provisional = T)

InvertData <- invertdata$inv_taxonomyProcessed

write.csv(InvertData, "./MultidimensionalScaling/InvertData.csv")
#-----------------------------------------------------------------
# Step 3. Load the NEON invertebrate taxonomy/count data, then clean up and re-format the invertebrate data.
#-----------------------------------------------------------------

# Load the NEON [inv_taxonomyProcessed] table from the [NEONdata] file.

  # InvertData <- read.csv("filesToStack20120/stackedFiles/inv_taxonomyProcessed.csv", na.strings = "", stringsAsFactors = F)


# SELECT ONLY ENTRIES FOR TRUE INSECTS (class = "Insecta").#############################

  InsectData <- subset(InvertData, class == "Insecta")


# Create a new column of concatenated taxonomic information ("TaxaCode") in [InsectData].

  InsectData$TaxaCode <- paste(InsectData$order, "_", InsectData$family, "_", InsectData$genus, sep = "")
  
# Split the 'sampleID' column of [InsectData] into separate 'Site', 'Year', 'Month', 'Day', and 'Device' columns.
# Then remove the original 'collectDate' column.

  # Use the 'as.character' function to make sure the 'sampleID' values are recognized as the character data type.

  InsectData$sampleID <- as.character(InsectData$sampleID)
  
  # Assign the 'siteID' value to a new 'Site' variable.
  
  InsectData$Site <- InsectData$siteID

  # Use 'substring' function to select the 1st-4th characters of the 'collectDate' value and assign them to a new 'Year' variable.

  InsectData$Year <- substring(InsectData$collectDate, 1, 4)
  
  # Repeat the above for a new 'Month' variable.
  
  InsectData$Month <- substring(InsectData$collectDate, 6, 7)
  
  # Repeat the above for a new 'Day' variable.
  
  InsectData$Day <- substring(InsectData$collectDate, 9, 10)
  
  # Repeat the above for a new 'Replicate' variable.
  
  InsectData$Replicate <- substring(InsectData$sampleID, 15)
  
  # Use the 'select' function from 'dplyr' to delete the 'sampleID' column from [InsectData].
  
  InsectData <- select(InsectData, -c(sampleID))


# Create a new 'Gear' column that separates Gear type from 'Replicate'.
# The '[[:digit:]]' parameter selects all numbers within the specified character string, then removes them.
  
  InsectData$Gear <- gsub("[[:digit:]]", "", InsectData$Replicate)

    InsectData$Gear <- gsub(".", "", InsectData$Gear, fixed = T)


# Use 'subset' function with '!=' parameter to remove "DNA" entries from the 'Gear' column.

  InsectData <- subset(InsectData, Gear != "DNA")


# Create a new 'SampleDate' column that combines 'Year' and 'Month'.

  InsectData$SampleDate <- paste(InsectData$Year, InsectData$Month, sep = "_")


# Rename the 'invertebrateLifeStage' and 'estimatedTotalCount' columns.

  InsectData <- InsectData %>% rename(LifeStage = invertebrateLifeStage)
  
  InsectData <- InsectData %>% rename(Count = estimatedTotalCount)


# Delete all individuals that are not identified to AT LEAST genus-level.

# Note: as of 20 August 2023, there are 2,898,402 individual INSECT specimens included in the complete NEON benthic inverts database.
# Of these, 2,390,618 individual specimens, or 82.5% of all individuals, are identified to genus-level or higher.
# Deleting individuals identified below genus-level therefore eliminates ~17% of the overall data.
# By retaining ONLY genus-level ID, we also lose the additional resolution provided by 608,322 individuals (~21% of all data) identified to species-level.

  InsectData <- subset(InsectData, genus != "NA")


# Clean up the dataframe by subsetting and reordering columns.

  InsectData <- InsectData[, c("Site", "SampleDate", "Gear", "Replicate", "TaxaCode", "LifeStage", "Count")]


# Make sure the 'Count' column is recognized as numeric data.

  InsectData$Count <- as.numeric(InsectData$Count)

  InsectData


#-----------------------------------------------------------------
# Step 4. Build an Assemblage Matrix (counts of genera in each sample) that consolidates samples by sampling event, with all replicates and gear types combined.
#-----------------------------------------------------------------

# Consolidate the 'Count' values by 'Site', 'SampleDate', and 'TaxaCode'.
# Use the 'group_by' and 'summarize' functions from the 'dplyr' package.
# Save the result as new dataframe [CountSums].

  CountSums <- InsectData %>% group_by(Site, SampleDate, TaxaCode) %>% summarize(Count = sum(Count))


# Create a new 'SiteSample' variable in [CountSums] that combines the 'Site' and 'SampleDate' values.

  CountSums$SiteSample <- paste(CountSums$Site, CountSums$SampleDate, sep = "_")


# Clean up the dataframe by subsetting and reordering columns.

  CountSums <- subset(CountSums, select = c("SiteSample", "TaxaCode", "Count"))


# Delete any rows with NA entries for 'Count'.

  CountSums <- subset(CountSums, Count != 'NA')


# Use the 'pivot_wider' function from 'tidyr' to convert the stacked abundance data to a traditional site/sample x taxa matrix.

  AssemblageMatrix <- CountSums %>% pivot_wider(names_from = TaxaCode, values_from = Count, values_fill = 0)


# Use the 'as.data.frame' function to ensure [AssemblageMatrix] is recognized as a dataframe (vs. a Tidyr tibble).

  AssemblageMatrix <- as.data.frame(AssemblageMatrix)


# Use the 'rownames' function to convert the 'SiteSample' column values from [AssemblageMatrix] to row names.
# Then remove the 'SiteSample' column.

  rownames(AssemblageMatrix) <- AssemblageMatrix[, 1]

  AssemblageMatrix <- AssemblageMatrix[, -c(1)]


# Save the [AssemblageMatrix] dataframe to a .csv file for later use.

  write.csv(AssemblageMatrix, "./MultidimensionalScaling/Insects_AssemblageMatrix.csv", row.names = T)



#-----------------------------------------------------------------
# Step 5. Perform Non-Metric Multidimensional Scaling (NMDS; 'metaMDS' function in 'vegan') with the Assemblage Matrix [AssemblageMatrix].
#-----------------------------------------------------------------

# First calculate the Distance Matrix, using the Horn–Morisita dissimilarity index, then save Distance Matrix as .csv.
# Use function 'vegdist' from R package 'vegan', with 'method = "horn"'.

  DistMat <- as.matrix(vegdist(AssemblageMatrix, method = "horn", binary = F))
 
  write.csv(DistMat, "./MultidimensionalScaling/Insects_DistanceMatrix_HornMorisita.csv")


# Then perform NMDS with the 'metaMDS' function in 'vegan'.
# Use the 'k' parameter to set the number of ordination axes.

  set.seed(1)
  NMDSresult <- metaMDS(DistMat, k = 2)


# Save MDS axis scores as a data frame.

  NMDSscores <- as.data.frame(NMDSresult$points)



#-----------------------------------------------------------------
# Step 6. Build basic NMDS ordination plot with GGplot.
#-----------------------------------------------------------------

# Build the basic ordination scatter plot.

  NMDSplot <- ggplot(NMDSscores, aes(x = MDS1, y = MDS2)) + geom_point(color = 'blue', size = 3) + theme_classic()

  NMDSplot


# Rebuild the ordination scatterplot when the x and y axis limits are set from -0.6 to 0.6.

  NMDSplot <- NMDSplot + xlim(-0.6, 0.6) + ylim(-0.6, 0.6)

  NMDSplot



#-----------------------------------------------------------------
# Step 7. Re-build  NMDS ordination plot with site labels and improved aesthetics.
#-----------------------------------------------------------------

# Add a new 'Site'column to [NMDSscores] for use in plotting.

  NMDSscores$Site <- row.names(NMDSscores)
  
  NMDSscores$Site <- substring(NMDSscores$Site, 1, 4)


# Rebuild the ordination scatterplot with site labels replacing the points.

  NMDSplot2 <- NMDSplot + geom_label(label = NMDSscores$Site, size = 2)
  NMDSplot2
# Rebuild the ordination scatterplot with offset labels.

#   NMDSplot3 <- NMDSplot + geom_label_repel(aes(label = Site), size = 2,
#   
# 										   box.padding = 0.5, segment.color = 'grey50')

  NMDSplot3 <- ggplot(NMDSscores, aes(x = MDS1, y = MDS2)) + 
    geom_point(color = 'blue', size = 4) + xlim(-0.6, 0.6) + ylim(-0.6, 0.6) +
    geom_label_repel(aes(label = Site), size = 2,
                     box.padding = 0.5, segment.color = 'grey50')
  NMDSplot3
                     



# Export the ordination scatterplot.

  ggsave("./MultidimensionalScaling/Insects_NMDSplot.pdf", width = 7, height = 7, units = "in")

  ggsave("./MultidimensionalScaling/Insects_NMDSplot.png", width = 7, height = 7, units = "in")



#-----------------------------------------------------------------
# End of code.
#-----------------------------------------------------------------