## This is an R script to:
## 1) Download NEON BENTHIC INVERTEBRATE data for ONE study stream.
## 2) Build a Gantt chart of available invertebrate samples.
## 3) Convert the raw NEON invertebrate data to a Genus x Site Assemblage Matrix of INSECT data (note: only class Insecta specimens are included).
## 4) Perform Nonmetric Multidimensional Scaling (NMDS) for the Insect Assemblage Matrix.
## 5) Add seasonal convex hulls to the NMDS ordination plot.

# 7 R packages are needed from CRAN: 'neonUtilities', 'remotes', 'tidyverse', 'vegan', 'ggrepel', 'shipunov', and 'geometry'.
# 1 R package is needed from GitHub: 'ganttrify'.	


  
#-----------------------------------------------------------------
# Step 0. Install and load the necessary R packages.
#-----------------------------------------------------------------


# FIRST MAKE SURE RTOOLS IS INSTALLED ON YOUR COMPUTER (if you have a PC)!

# Rtools is standalone software. It's installation will run EXTERNAL to R and Rstudio.
# Download from: https://cran.r-project.org/bin/windows/Rtools/


# Now install and/or load the 7 R packages from CRAN.

  install.packages(c('neonUtilities', 'remotes', 'tidyverse', 'vegan', 'ggrepel', 'shipunov', 'geometry'))

  library(neonUtilities)
  library(remotes)
  library(tidyverse)
  library(vegan)
  library(ggrepel)
  library(shipunov)
  library(geometry)


# Must also download R package 'ganttrify' directly from GitHub.
# Lots of info on 'ganttrify' available at https://github.com/giocomai/ganttrify.

  remotes::install_github("giocomai/ganttrify", dependencies = TRUE, force = TRUE)
  
  library(ganttrify)



#-----------------------------------------------------------------
# Step 1. Create a new R working directory.
#-----------------------------------------------------------------

# First set the initial working directory.

  setwd({'paste your working directory path here & delete curly brackets'})	# Update this path.


# Assign your NEON token value to a new 'NEONtoken' variable.

  NEONtoken <- {'paste your NEON token here & delete curly brackets'}	# Update this line.

  NEONtoken <-	'eyJ0eXAiOiJKV1QiLCJhbGciOiJFUzI1NiJ9.eyJhdWQiOiJodHRwczovL2RhdGEubmVvbnNjaWVuY2Uub3JnL2FwaS92MC8iLCJzdWIiOiJkam1jZ2FydmV5QHZjdS5lZHUiLCJzY29wZSI6InJhdGU6cHVibGljIiwiaXNzIjoiaHR0cHM6Ly9kYXRhLm5lb25zY2llbmNlLm9yZy8iLCJleHAiOjE4MjI3MzI2NjksImlhdCI6MTY2NTA1MjY2OSwiZW1haWwiOiJkam1jZ2FydmV5QHZjdS5lZHUifQ.dabCQWA6803aNxOD_NGO0IPTrKFAuZ0rO_siuQ_msxJFUN7lGzmm_d4hRwuo8IKrj0XZwJBJCFDbxZ8EsfS0YA'


# Specify the appropriate 4-letter NEON site code (e.g.,'ARIK') to download and assign it to a new 'SiteCode' variable.
# All NEON sites are listed on the website:
# https://www.neonscience.org/field-sites/explore-field-sites
# To download all sites at once, use 'all' (i.e., site = 'all').

  SiteCode <- {'paste your NEON site code here & delete curly brackets'}	# Update this line.


# Create a new folder/directory to save results, with the SiteCode and "InvertData" in the folder name.

 # The 'paste' function creates a character string that combines (concatenates) all the included parameters.
 # We can paste together path names (e.g., getwd()), variable values (e.g., SiteCode), and new text in quotation marks.
 # The 'sep =' parameter determines what (if any) character(s) will be inserted between paste parameters.
 # Here the lack of an entry between the quotation marks for 'sep =' means there will be no separator.

  FolderName <- paste(getwd(), "/", SiteCode, "_InvertData", sep = "")
 
 # This 'ifelse' statment checks to see if a folder with the name we want already exists.
 # If no, it will create the named folder. If yes, the code will stop.
 
  ifelse(dir.exists(FolderName), stop("***** This Site folder already exists *****"), dir.create(FolderName))
  
  setwd(FolderName)



#-----------------------------------------------------------------
# Step 2. Download NEON invert data for a single site, including multiple sampling events.
#-----------------------------------------------------------------

# Specify the YEAR and MONTH of the Start Date and End Date for the data records you will download.
# Use YYYY-MM format (e.g., '2018-10').
# To download ALL dates, set StartDate and EndDate to 'NA'.
  
  StartDate <- '2016-01'
  
  EndDate <- '2024-12'


# Download the NEON benthic invertebrate data.
# Benthic invertebrate samples are NEON data product 'DP1.20120.001' {i.e., dpID = 'DP1.20120.001'}.
# Multiple optional parmaters (e.g., specify years, months, etc.) can be specified. Check instructions at:
# https://www.neonscience.org/neonDataStackR

  zipsByProduct(dpID = 'DP1.20120.001', site = SiteCode, startdate = StartDate, enddate = EndDate, 
  
				package = 'basic', check.size = F, token = NEONtoken, include.provisional = T)


  stackByTable(paste0(getwd(), '/filesToStack20120'), folder = T, saveUnzippedFiles = F, nCores = parallel::detectCores())


# Load the NEON invertebrate data file ("inv_persample.csv").

  InvertData <- read.csv("filesToStack20120/stackedFiles/inv_persample.csv", na.strings = "", stringsAsFactors = F)



#-----------------------------------------------------------------
# Step 3. Prepare a new table of sampling dates that are formatted for R package 'ganttrify'.
#-----------------------------------------------------------------

# Note: The Gantt chart data used in 'ganttrify' must be formatted in a very specific way.
# We must have columns named "wp", "activity", "start_date", and "end_date".

# For the NEON invertebrate data, it will make sense to specify "wp", "activity", "start_date", and "end_date" as follows:
#   wp = type of sample (Benthic sample)
#   activity = sampling site, sampler type, and sample replicate (e.g., "HOPB_surber_1")
#   start_date = "collectDate"
#   end_date = "collectDate"

# Now we'll pull the columns we need from [InvertData] and do a bit of reformatting.
# First, select the 'siteID', 'collectDate' and 'sampleID' columns and get rid of the rest, then save as a new [GanttData] dataframe.
# Use square brackets [] to specify that you will only retain the identified variables (i.e., columns).
# The 'c' before parentheses tells R you are submitting a VECTOR of information.

  GanttData <- InvertData[ , c('siteID', 'collectDate', 'sampleID')]


# Next we'll create a new "wp" column and auto-fill it with "Benthic sample" entries.

  GanttData$wp <- c("Benthic sample")


# Then create a new 'Sample' column with sampler type from the 'sampleID' column.

 # Here the 'str_sub' function is used to select text the 15th-22nd text characters in 'sampleID'.

  GanttData$Sample <- str_sub(GanttData$sampleID, 15, 22)


 # The 'gsub' function replaces one value (periods, here) with another (a deleted space).
  
  GanttData$Sample <- gsub(".", "", GanttData$Sample, fixed = T)


 # The '[[:digit:]]' parameter selects all numbers within the specified character string, then removes them.
	
  GanttData$Sample <- gsub("[[:digit:]]", "", GanttData$Sample)


 # The 'tolower' function changes all characters in "Sample" to lower case.
  
  GanttData$Sample <- tolower(GanttData$Sample)


# Combine the "siteID" and "Sample" columns into a new "activity" column.

  GanttData$activity <- paste(GanttData$siteID, GanttData$Sample, sep = "_")


# Create new "start_date" and "end_date" columns to work with 'ganttrify'.

 # Use the 'str_sub' function to select the 1st-10th characters in "collectDate", then assign them to "start_date".

  GanttData$start_date <- str_sub(GanttData$collectDate, 1, 10)

  GanttData$end_date <- str_sub(GanttData$collectDate, 1, 10)


# Get rid of unnecessary columns by selecting just the 4 we want to keep.

  GanttData <- GanttData[ , c('wp', 'activity', 'start_date', 'end_date')]


# Finally, use the 'order' function to sort alpha-numerically by "activity" (i.e., site ID and sample type).

  GanttData <- GanttData[order(GanttData$activity),]


# Save the [GanttData] file as .csv.

  OutputName <- paste(SiteCode, "_Invertebrates_GanttData.csv", sep = "")
  
  write.csv(GanttData, OutputName, row.names = F)



#-----------------------------------------------------------------
# Step 4. Create a new dataframe with the number of replicate samples for each sampling event.
#-----------------------------------------------------------------

# Copy [GanttChart] to the new data frame [Replicates].

  Replicates <- GanttData


# Create a new 'YearMonth' column that removes days from the dates.

  Replicates$YearMonth <- str_sub(Replicates$start_date, 1, 7)


# Get rid of unnecessary columns (i.e., keep only 'activity' and 'YearMonth').

  Replicates <- Replicates[ , c('activity', 'YearMonth')]


# Use the 'group_by_all' function from 'dplyr' ('tidyr') to count duplicate rows.
# This will tally the number of gear type replicates collected on a given sampling date, at a given site.

  Replicates <- Replicates %>% group_by_all() %>% count


# Use the 'rename' function from 'dplyr' ('tidyr') to rename the new 'n' column (i.e., number of replicates) as 'spot_type' for 'ganttrify'.

  Replicates <- Replicates %>% rename(spot_type = n)


# Copy the 'start_date' column to a new 'spot_date' column.

  Replicates$spot_date <- Replicates$YearMonth


# Get rid of unnecessary columns (i.e., keep only 'activity', 'spot_type', and 'spot_date').

  Replicates <- Replicates[ , c('activity', 'spot_type', 'spot_date')]


# Save the [Replicates] file as .csv.

  OutputName <- paste(SiteCode, "_Invertebrates_Replicates.csv", sep = "")
  
  write.csv(Replicates, OutputName, row.names = F)



#-----------------------------------------------------------------
# Step 5. Build the Gantt chart.
#-----------------------------------------------------------------

  GanttChart <- ganttrify(project = GanttData,
						  spots = Replicates,
						  by_date = TRUE,
						  month_breaks = 3,
						  show_vertical_lines = FALSE,
						  mark_quarters = FALSE,
						  month_number_label = FALSE,
						  alpha_wp = 0,
						  spot_fill = NA,
						  spot_border = NA,
						  size_text_relative = 0.8
						  )

  GanttChart


# Now export as .pdf.

  OutputName <- paste(SiteCode, "_Invertebrates_GanttChart.pdf", sep = "")
  
  ggsave(GanttChart, filename = OutputName, width = 15, height = 1.5)



#-----------------------------------------------------------------
# Step 6. Load the NEON invertebrate taxonomy/count data, then clean up and re-format the invertebrate data.
#-----------------------------------------------------------------

# Load the NEON [inv_taxonomyProcessed] table from the [NEONdata] file.

  InvertData <- read.csv("filesToStack20120/stackedFiles/inv_taxonomyProcessed.csv", na.strings = "", stringsAsFactors = F)


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



#-----------------------------------------------------------------
# Step 7. Build an Assemblage Matrix (counts of genera in each sample) that consolidates samples by sampling event, with all replicates and gear types combined.
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

  OutputName <- paste(SiteCode, "_Insects_AssemblageMatrix.csv", sep = "")

  write.csv(AssemblageMatrix, OutputName, row.names = T)



#-----------------------------------------------------------------
# Step 8. Perform Non-Metric Multidimensional Scaling (NMDS) with the Assemblage Matrix [AssemblageMatrix].
#-----------------------------------------------------------------

# First calculate the Distance Matrix, using the Horn–Morisita dissimilarity index, then save Distance Matrix as .csv.
# Use function 'vegdist' from R package 'vegan', with 'method = "horn"'.

  DistMat <- as.matrix(vegdist(AssemblageMatrix, method = "horn", binary = F))

  OutputName <- paste(SiteCode, "_Insects_DistanceMatrix_HornMorisita.csv", sep = "")
 
  write.csv(DistMat, OutputName)


# Then perform NMDS with the 'metaMDS' function in 'vegan'.
# Use the 'k' parameter to set the number of ordination axes.

  NMDSresult <- metaMDS(DistMat, k = 2)


# Save MDS axis scores as a data frame.

  NMDSscores <- as.data.frame(NMDSresult$points)



#-----------------------------------------------------------------
# Step 9. Build NMDS ordination plot with GGplot.
#-----------------------------------------------------------------

# Add new 'YearMonth', 'Year' and 'Month' columns to [NMDSscores], for use in plotting.

  NMDSscores$Date <- row.names(NMDSscores)
  
  NMDSscores$YearMonth <- substring(NMDSscores$Date, 6, 12)
  
  NMDSscores$Year <- substring(NMDSscores$Date, 6, 9)
  
  NMDSscores$Month <- substring(NMDSscores$Date, 11, 12)
  
  NMDSscores <- subset(NMDSscores, select = -c(Date))


# Build the basic ordination (scatter) plot.

  NMDSplot <- ggplot(NMDSscores, aes(x = MDS1, y = MDS2)) + geom_point(color = 'blue', size = 3) + 
  
					 theme_classic()
  
  NMDSplot


# Add 'SiteCode' and Stress value from NMDS ordination to plot.

  Stress <- round(NMDSresult$stress, digits = 3)
  
  StressOut <- paste("Stress = ", Stress, sep = "")
  
  Title <- paste(SiteCode, " (", StressOut, ")", sep = "")
  
  NMDSplot <- NMDSplot + ggtitle(Title)
  
  NMDSplot


# Add sampling dates as labels to ordination plot points.
# Use the 'geom_label_repel' function from 'ggrepel' library to offset labels.

  NMDSplot <- NMDSplot + geom_label_repel(aes(label = YearMonth), size = 2, box.padding = 0.5,
  
										  segment.color = 'grey50')
  
  NMDSplot


# Add arrows connecting points in sequential order (oldest to newest).

  NMDSarrow <- arrow(angle = 25, length = unit(0.1, "inches"), ends = "last", type = "closed")

  NMDSplot <- NMDSplot + geom_path(arrow = NMDSarrow)
  
  NMDSplot
	

# Rebuild the ordination scatterplot when the x and y axis limits are set from -0.8 to 0.8, then save.

  NMDSplot <- NMDSplot + xlim(-0.8, 0.8) + ylim(-0.8, 0.8)

  NMDSplot

  OutputName <- paste(SiteCode, "_NMDS_Plot_Insects.pdf", sep = "")

    ggsave(OutputName, width = 10, height = 10, units = "in")

  OutputName <- paste(SiteCode, "_NMDS_Plot_Insects.png", sep = "")

    ggsave(OutputName, width = 10, height = 10, units = "in")


# Recreate and save the ordination plot (standardized axes) when only the arrows are included, without the sample points.

  NMDSplot <- ggplot(NMDSscores, aes(x = MDS1, y = MDS2)) + theme_classic() +
  
				    geom_path(arrow = NMDSarrow) + ggtitle(Title) +
					
					xlim(-0.8, 0.8) + ylim(-0.8, 0.8)

  NMDSplot
  
  OutputName <- paste(SiteCode, "_NMDS_Plot_NoPoints_Insects.pdf", sep = "")

    ggsave(OutputName, width = 10, height = 10, units = "in")

  OutputName <- paste(SiteCode, "_NMDS_Plot_NoPoints_Insects.png", sep = "")

    ggsave(OutputName, width = 10, height = 10, units = "in")



#-----------------------------------------------------------------
# Step 10. Add NEON season values to [NMDSscores].
#-----------------------------------------------------------------

# Load the NEON field data file ("inv_fieldData.csv") with seasonal ('boutNumber'; 1 = spring, 2 = summer, 3 = fall) codes .

  SeasonData <- read.csv("filesToStack20120/stackedFiles/inv_fieldData.csv", na.strings = "", stringsAsFactors = F)


# Keep only the 'sampleID' and 'boutNumber' columns from [SeasonData].

  SeasonData <- SeasonData[, c('sampleID', 'boutNumber')]


# Rename the 'boutNumber' column as 'Season'.

  SeasonData <- SeasonData %>% rename(Season = boutNumber)


# Query NEON site code from 'sampleID' to a new 'Site' column.

  SeasonData$Site <- str_sub(SeasonData$sampleID, 1, 4)


# Query sample year from 'sampleID' to a new 'Year' column.

  SeasonData$Year <- str_sub(SeasonData$sampleID, 6, 9)


# Query sample month from 'sampleID' to a new 'Month' column.

  SeasonData$Month <- str_sub(SeasonData$sampleID, 10, 11)


# Combine 'Site', 'Year', and 'Month' into new 'Sample' column.

  SeasonData$Sample <- paste(SeasonData$Site, SeasonData$Year, SeasonData$Month, sep = "_")


# Remove 'sampleID', 'Site', 'Year', and 'Month' columns.

  SeasonData <- SeasonData[, c('Sample', 'Season')]


# Create new 'Sample' column for [NMDSscores].

  NMDSscores$Sample <- row.names(NMDSscores)


# Join [NMDSscores] with [SeasonData].

  NMDSscores <- merge(x= NMDSscores, y = SeasonData, all.x = T, by = 'Sample')
  
  NMDSscores <- NMDSscores %>% distinct(.keep_all = T)


# Clean up [NMDSscores].

  NMDSscores <- NMDSscores[, c('Sample', 'MDS1', 'MDS2', 'Season')]



#-----------------------------------------------------------------
# Step 11. Build a new ordination plot where sample points are color coded by sample season.
#-----------------------------------------------------------------

# Change Season labels as follows: 1 = "Spring", 2 = "Summer", 3 = "Fall".

  NMDSscores$SeasonName <- ifelse(NMDSscores$Season == 1, "Spring",
						   ifelse(NMDSscores$Season == 2, "Summer",
						   ifelse(NMDSscores$Season == 3, "Fall", "Winter"
						   )))


# Change 'Season' values to ordered (Spring to Winter) factor levels.

  NMDSscores <- NMDSscores %>% mutate(SeasonName = factor(SeasonName, levels = c('Spring', 'Summer', 'Fall', 'Winter')))


# Save the edited .csv file.

  OutputName <- paste(SiteCode, "_NMDS_AxisScores_Insects.csv", sep = "")
  
  write.csv(NMDSscores, OutputName, row.names = T)
  

# Create a custom color palette to represent seasons.
# Each season is assigned a hexidecimal code for it's color.

  ColorPal <- c('Spring' = '#009E73', 'Summer' = '#E69F00', 'Fall' = '#56B4E9', 'Winter' = '#0072B2')
  

# Plot the seasonal (colored points) ordination with arrows.

  NMDSplot <- ggplot(NMDSscores, aes(x = MDS1, y = MDS2)) + theme_classic() +
  
				    geom_path(arrow = NMDSarrow) + ggtitle(Title) +
					
					geom_point(aes(x = MDS1, y = MDS2, color = SeasonName), size = 4) +
					
					scale_x_continuous(name = 'Axis 1', breaks = c(-0.8, -0.4, 0, 0.4, 0.8), labels = c('-0.8', '', '0', '', '0.8'), limits = c(-0.8, 0.8)) +
					  
				    scale_y_continuous(name = 'Axis 2', breaks = c(-0.8, -0.4, 0, 0.4, 0.8), labels = c('-0.8', '', '0', '', '0.8'), limits = c(-0.8, 0.8)) +
					
					scale_color_manual(values = ColorPal)

  NMDSplot

  OutputName <- paste(SiteCode, "_NMDS_PlotSeason_Insects.pdf", sep = "")

    ggsave(OutputName, width = 8, height = 7, units = "in")

  OutputName <- paste(SiteCode, "_NMDS_PlotSeason_Insects.png", sep = "")

    ggsave(OutputName, width = 8, height = 7, units = "in")



#-----------------------------------------------------------------
# Step 12. Calculate seasonal convex hulls, then re-create 2D scatterplot of MDS axes 1 and 2 with seasonal convex hulls.
#-----------------------------------------------------------------
  
# Add convex hulls to the seasonal ordination points.
# Use the 'dplyr' functions 'group_by', 'slice', and 'chull' to query rows from [NMDSscores] based on their 'Season'.
# This creates a new dataframe [ConHul] where X and Y coordinates from the MDS ordination are neatly organized according to Season.

  ConHul <- NMDSscores %>% group_by(SeasonName) %>% slice(chull(x = MDS1, y = MDS2))

  
  PlotTitle <- paste0(SiteCode, "_Insects_ConvexHullOverlap (Stress = ", Stress,")")
  
  NMDSplot <- ggplot(ConHul, aes(x = MDS1, y = MDS2, color = SeasonName, fill = SeasonName)) +
  
					theme_classic() + guides(alpha = 'none') + ggtitle(PlotTitle) +
					
					geom_polygon(alpha = 0.4) + geom_point(data = NMDSscores, aes(x = MDS1, y = MDS2), size = 2.5, alpha = 0.8) +
									
				    scale_x_continuous(name = 'Axis 1', breaks = c(-0.8, -0.4, 0, 0.4, 0.8), labels = c('-0.8', '', '0', '', '0.8'), limits = c(-0.8, 0.8)) +
					  
				    scale_y_continuous(name = 'Axis 2', breaks = c(-0.8, -0.4, 0, 0.4, 0.8), labels = c('-0.8', '', '0', '', '0.8'), limits = c(-0.8, 0.8)) +
					  
				    scale_color_manual(values = ColorPal) + scale_fill_manual(values = ColorPal)

  NMDSplot
  
  OutputName <- paste(SiteCode, "_ConvexHullOverlap_Insects.pdf", sep = "")

    ggsave(OutputName, width = 8, height = 7, units = "in")

  OutputName <- paste(SiteCode, "_ConvexHullOverlap_Insects.png", sep = "")

    ggsave(OutputName, width = 8, height = 7, units = "in")



#-----------------------------------------------------------------
# Step 13. Use a permuation/simulation test to detect 'significantly small' seasonal hulls and 'significant separation' among hulls.
#-----------------------------------------------------------------

# First calculate areas of seasonal convex hulls; repeat 999 permutations.
# Then calculate overlap among hulls; repeat 999 permutations.

# Create new data frame [ConHulTest] to store results.

  SpringArea <- 0
  
  SummerArea <- 0
  
  FallArea <- 0
  
  WinterArea <- 0
  
  Overlap <- 0
  
  ConHulTest <- data.frame(SpringArea, SummerArea, FallArea, WinterArea, Overlap)


# Calculate empirical areas (volumes) of seasonal convex hulls, then append empirical results as SECOND row of [ConHulTest]. 
# Use the 'split' function (from 'dplyr'/'tidyverse') to break the [NMDSscores] data into Seasonal groups.
# Use the 'map' function (from 'purrr'/'tidyverse') to convert the NMDS axis values to matrix format (for use in building convex hulls).
# Use another 'map' function with the 'convhulln' function (from 'geometry' library) with 'FA' option to calculate convex hull areas.
# Use the 'map_dbl' function (from 'purrr'/'tidyverse') with 'vol' parameter to extract the hull volumes (i.e., 2-D areas).

  ConHulResult <- NMDSscores %>% split(.$Season) %>% map(~ as.matrix(.[ , c('MDS1', 'MDS2')])) %>%
  
													 map(~ convhulln(., option = 'FA')) %>%
									 
													 map_dbl('vol')


# Append the seasonal convex hull areas to [ConHulTest].

  ConHulTest[2, 1] <- ConHulResult[1]
  
  ConHulTest[2, 2] <- ConHulResult[2]

  ConHulTest[2, 3] <- ConHulResult[3]

  ConHulTest[2, 4] <- ConHulResult[4]


# Use the 'Overlap' function ('Shipunov' library) to calculate empirical overlap among seasonal convex hulls.
# Then append empirical results as SECOND row of [ConHulTest].
# Subtract 3 from the sum of the overlap matrix ('HullOverlap') to remove diagonal values (all unity values).

  plot.new()

  HullOverlap <- Overlap(Hulls(NMDSscores[, 2:3], NMDSscores[, 4]))

  ConHulTest[2, 5] <- sum(HullOverlap) - 3


# Change name of FIRST ROW in [ConHulTest] to 'p_value'.

  row.names(ConHulTest)[row.names(ConHulTest) == '1'] <- 'p_value'


# Change name of SECOND ROW in [ConHulTest] to 'empirical'.

  row.names(ConHulTest)[row.names(ConHulTest) == '2'] <- 'empirical'


# Make a copy of [NMDSscores] named [NMDSscoresRand] and delete unnecessary columns.

  NMDSscoresRand <- NMDSscores
  
  NMDSscoresRand <- NMDSscoresRand %>% select('Sample', 'MDS1', 'MDS2', 'Season')



### Start the simulation loop here --------------------------------------------------------------------------

  for (i in 1:99) {


# Randomize seasonal group membership in [NMDSscoresRand].

  NMDSscoresRand <- transform(NMDSscoresRand, Season = sample(Season))


# Calculate area of each seasonal hull, then append random results to [ConHulTest].

  ConHulResult <- NMDSscoresRand %>% split(.$Season) %>% map(~ as.matrix(.[ , c('MDS1', 'MDS2')])) %>%
  
														 map(~ convhulln(., option = 'FA')) %>%
									 
														 map_dbl('vol')

  ConHulTest[i + 2, 1] <- ConHulResult[1]
  
  ConHulTest[i + 2, 2] <- ConHulResult[2]

  ConHulTest[i + 2, 3] <- ConHulResult[3]

  ConHulTest[i + 2, 4] <- ConHulResult[4]
  
  row.names(ConHulTest)[row.names(ConHulTest) == i + 2] <- paste0('sim', i)


# Calculate summed area of overlap among convex hulls, then append random results to [ConHulTest].
# Subtract 3 from the sum of the overlap matrix to remove diagonal values (all unity values).

  plot.new()

  HullOverlap <- Overlap(Hulls(NMDSscoresRand[, 2:3], NMDSscoresRand[, 4]))

  ConHulTest[i + 2, 5] <- sum(HullOverlap) - 3


# Update 'i'.

	i = i + 1


  }


### End the simulation loop here --------------------------------------------------------------------------



# Calculate p-values for seasonal hull sizes.
# Each p-value is the fraction of simulated hull sizes that is SMALLER THAN OR EQUAL TO the size of the empirical hull.

  sim_all <- ConHulTest[-(1:2), ]
  
  emp_spring <- ConHulTest[2, 1]

    p_spring <- (sum(sim_all[, 1] <= emp_spring, na.rm = T)) / 100
	
	ConHulTest[1, 1] <- p_spring

  emp_summer <- ConHulTest[2, 2]
  
    p_summer <- (sum(sim_all[, 2] <= emp_summer, na.rm = T)) / 100
  
	ConHulTest[1, 2] <- p_summer

  emp_fall <- ConHulTest[2, 3]
  
    p_fall <- (sum(sim_all[, 3] <= emp_fall, na.rm = T)) / 100
  
	ConHulTest[1, 3] <- p_fall

  emp_winter <- ConHulTest[2, 4]
  
    p_winter <- (sum(sim_all[, 4] <= emp_winter, na.rm = T)) / 100
  
	ConHulTest[1, 4] <- p_winter


# Calculate p-value for hull overlap.
# The p-value is the fraction of simulated overlap values that is LESS THAN OR EQUAL TO the empirical overlap.
  
  emp_overlap <- ConHulTest[2, 5]

    p_overlap <- (sum(sim_all[, 5] <= emp_overlap, na.rm = T)) / 100
	
	ConHulTest[1, 5] <- p_overlap


# Export [ConHulTest] to a .csv file.

  OutputName <- paste(SiteCode, "_ConvexHull_Area_Overlap.csv", sep = "")
  
  write.csv(ConHulTest, OutputName, row.names = T)



#-----------------------------------------------------------------
# End of code.
#-----------------------------------------------------------------