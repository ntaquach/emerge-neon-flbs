## This is an R script to:
## 1) Build a linear regression model of fish species richess, using river discharge as the predictor and data from McGarvey & Terra (2016).
## 2) Create a custom loop in R that will iterate steps 2-5 below.
## 3) Randomly subsample the complete dataset.
## 4) Re-build the linear regression model of fish species richness, fit to the subsampled data.
## 5) Store the modelign results to a master list of results.
## 6) Repeat the process a specified number of times.

# The full citation for McGarvey & Terra (2016) is:
# 'Using river discharge to model and deconstruct the latitudinal diversity gradient for fishes of the Western Hemisphere',
# Journal of Biogeography 43(7):1436-1449 [https://doi.org/10.1111/jbi.12618].

# 1 R package is needed from CRAN: 'tidyverse'.



#-----------------------------------------------------------------
# Step 0. Install and load the necessary R packages.
#-----------------------------------------------------------------

# Now install and/or load 'tidyverse'.

  install.packages('tidyverse')

  library(tidyverse)

rm(list=ls())


#-----------------------------------------------------------------
# Step 1. Load the fish species richness data from McGarvey & Terra (2016).
#-----------------------------------------------------------------

# First navigate to the 'LoopExercise' folder and make it the working directory.


# Load the NEON invertebrate data file ("inv_persample.csv").

  FishData <- read.csv("./CustomLoop/McGarvey_Terra_2016_FishData.csv")



#-----------------------------------------------------------------
# Step 2. Build a linear regression model of fish species richness.
#-----------------------------------------------------------------

# First examine the correlation between discharge and richness.

  plot(FishData$Log_Richness ~ FishData$Log_HighFlowQ)


# Build the linear regression model (response = 'Log_Richness', predictor = 'Log_MeanAnnualQ').

  Model1 <- lm(FishData$Log_Richness ~ FishData$Log_MeanAnnualQ)

summary(Model1)

#-----------------------------------------------------------------
# Step 3. Create an empty data frame to store key results/statistics from the linear regression models.
#-----------------------------------------------------------------

# Create new variables to store relevant summary statistics from the linear regression models.

  Intercept <- 0
  
  InterceptStdErr <- 0
  
  Slope <- 0
  
  SlopeStdErr <- 0
  
  ResidStdErr <- 0
  
  Rsq <- 0
  
  AdjRsq <- 0
  
  DegFree <- 0
  
  Fstat <- 0
  
  Pvalue <- 0


# Combine the new, empty variables into a new data frame [RegressionResults] to store results.

  RegressionResults <- data.frame(Intercept, InterceptStdErr, Slope, SlopeStdErr, ResidStdErr, Rsq, AdjRsq, DegFree, Fstat, Pvalue)


# Update each of the summary statistics from the linear regression model.

  Intercept <- summary(Model1)$coefficients[1, 1]
  
  InterceptStdErr <- summary(Model1)$coefficients[1, 2]
  
  Slope <- summary(Model1)$coefficients[2, 1]
  
  SlopeStdErr <- summary(Model1)$coefficients[2, 2]
  
  ResidStdErr <- summary(Model1)$sigma
  
  Rsq <- summary(Model1)$r.squared
  
  AdjRsq <- summary(Model1)$adj.r.squared
  
  DegFree <- as.numeric(summary(Model1)$fstatistic[3])
  
  Fstat <- as.numeric(summary(Model1)$fstatistic[1])
  
  Pvalue <- summary(Model1)$coefficients[2, 4]


# Store the summary statistics in the first row of the [RegressionResults] data frame.

  RegressionResults[1, 1] <- Intercept

  RegressionResults[1, 2] <- InterceptStdErr
  
  RegressionResults[1, 3] <- Slope
  
  RegressionResults[1, 4] <- SlopeStdErr
  
  RegressionResults[1, 5] <- ResidStdErr
  
  RegressionResults[1, 6] <- Rsq
  
  RegressionResults[1, 7] <- AdjRsq
  
  RegressionResults[1, 8] <- DegFree
  
  RegressionResults[1, 9] <- Fstat
  
  RegressionResults[1, 10] <- Pvalue


# Change first row label in [RegressionResults] to 'Complete'.

  row.names(RegressionResults)[row.names(RegressionResults) == '1'] <- 'Complete'



#-----------------------------------------------------------------
# Step 4. Create a loop to subsample the complete dataset and re-run the linear regression model 99 times.
#-----------------------------------------------------------------

### Start the simulation loop here --------------------------------------------------------------------------

# Create a 'for' loop and use a counter ('i') to keep tabs of the number of subsamples run.
# Here, we state 'i in 1:99' to specify that we're starting with i = 1 and continuing through i = 99.
# After the 'for' statement, use the left curly bracket { to indicate the start of the looping code.

  for (i in 1:99) {


# Use the 'sample_n' function from 'tidyverse' ('dplyr') to randomly subsample 33 river basins (rows) from the complete [FishData] dataframe.

  FishDataSub <- FishData %>% sample_n(size = 33, replace = FALSE)


# Rebuild the linear regression model, using the subsample data [FishDataSub].

  Model1 <- lm(FishDataSub$Log_Richness ~ FishDataSub$Log_MeanAnnualQ)


# Update each of the summary statistics from the linear regression model.

  Intercept <- summary(Model1)$coefficients[1, 1]
  
  InterceptStdErr <- summary(Model1)$coefficients[1, 2]
  
  Slope <- summary(Model1)$coefficients[2, 1]
  
  SlopeStdErr <- summary(Model1)$coefficients[2, 2]
  
  ResidStdErr <- summary(Model1)$sigma
  
  Rsq <- summary(Model1)$r.squared
  
  AdjRsq <- summary(Model1)$adj.r.squared
  
  DegFree <- as.numeric(summary(Model1)$fstatistic[3])
  
  Fstat <- as.numeric(summary(Model1)$fstatistic[1])
  
  Pvalue <- summary(Model1)$coefficients[2, 4]


# Store the summary statistics in the 'i-th' row of the [RegressionResults] data frame.

  RegressionResults[i + 1, 1] <- Intercept

  RegressionResults[i + 1, 2] <- InterceptStdErr
  
  RegressionResults[i + 1, 3] <- Slope
  
  RegressionResults[i + 1, 4] <- SlopeStdErr
  
  RegressionResults[i + 1, 5] <- ResidStdErr
  
  RegressionResults[i + 1, 6] <- Rsq
  
  RegressionResults[i + 1, 7] <- AdjRsq
  
  RegressionResults[i + 1, 8] <- DegFree
  
  RegressionResults[i + 1, 9] <- Fstat
  
  RegressionResults[i + 1, 10] <- Pvalue


# Update the row label in [RegressionResults] to 'Subsample i'.

  row.names(RegressionResults)[row.names(RegressionResults) == i + 1] <- paste0('Subsample', i)


# Increase 'i' by 1 unit.

	# i = i + 1


# Use the right curly bracket } to end the looping code.

  }


### The simulation loop has ended here --------------------------------------------------------------------------


# Export [RegressionResults] to a .csv file.
  
  write.csv(RegressionResults, './CustomLoop/LinearRegressionResults_FishRichnessModel.csv', row.names = T)



#-----------------------------------------------------------------
# End of code.
#-----------------------------------------------------------------