## This is an R script to:
## 1) Build a Classification Tree (categorical response variable) model.
## 2) Compare the Classification Tree model with a Random Forest model.


#-----------------------------------------------------------------
# Step 0. Load R libraries and set the working directory.
#-----------------------------------------------------------------

# Start by loading (and/or installing) the 6 required R packages: 'tidyverse', 'rpart', 'rpart.plot', 'ggpmisc', 'pROC', and 'randomForest'.

	install.packages('tidyverse')
	install.packages('rpart')
	install.packages('rpart.plot')
	install.packages('ggpmisc')
	install.packages('pROC')
	install.packages("randomForest")
  
	library(tidyverse)
    library(rpart)
	library(rpart.plot)
	library(ggpmisc)
	library(pROC)
	library(randomForest)


# Next, set the R working directory. 

  setwd({'paste your working directory path here & delete curly brackets'})	# Update this path.

  rm(list=ls())
  

#-----------------------------------------------------------------
# Step 1. Build a Classification Tree model of Etheostoma_olmstedi presence using the 'Etheostoma_olmstedi_Train' dataset.
#-----------------------------------------------------------------

# Load the 'Etheostoma_olmstedi_Train' data.

	Etheostoma_olmstedi <- read.csv("./RandomForest/Etheostoma_olmstedi_Train.csv")


# Be sure the 'Etheostoma_olmstedi_presence' column (1 = presence, 0 = absence) is recognized as FACTOR data.

	Etheostoma_olmstedi$Etheostoma_olmstedi_presence <- as.factor (Etheostoma_olmstedi$Etheostoma_olmstedi_presence)  ## This line makes sure the 1,0 PA data will be treated as binary


# Delete any rows (replicates) with missing data.

	Etheostoma_olmstedi <- Etheostoma_olmstedi[complete.cases(Etheostoma_olmstedi), ]


# Use the 'rpart' function to build a Classification Tree model of Etheostoma_olmstedi presence.
# Because Etheostoma_olmstedi presence is a binary response variable, set the 'method' parameter as 'class'.

    EthOlm_Pres <- rpart(Etheostoma_olmstedi_presence ~ ., data = Etheostoma_olmstedi, method = 'class')


# Use the 'rpart.plot' function to view the Classification Tree.
# Note: 'rpart.plot' is the simplified option. Many more custom plot options are available with the 'prp' function.
# For more info on 'prp', go to: https://cran.r-universe.dev/rpart.plot/doc/manual.html

    rpart.plot(EthOlm_Pres)


# Use the 'rpart.rules' function to write out the cutpoints from the Classification Tree.

	rpart.rules(EthOlm_Pres)
	

# Use the 'sink' function to write the Classification Tree cutpoints to a .txt file.

	sink('Etheostoma_olmstedi_ClassificationModel.txt')
	
	rpart.rules(EthOlm_Pres)
	
	sink()


# Add a new 'PredictPres' column to the [Etheostoma_olmstedi] data.
# Populate the 'PredictPres' column with model-predicted values.
# Note: 2 columns of 'PredictPres' will be created: a '0' column (probability of absence) and a '1' column (probability of presence).
# These 2 columns are actually two vectors of a list. Totally confusing. Don't sweat it.

	Etheostoma_olmstedi$PredictPres <- predict(EthOlm_Pres, newdata = Etheostoma_olmstedi)


# Add a new 'PredictedPresence' column to the [Etheostoma_olmstedi] data.
# Convert the 'PredictedPresence[,"1"]' column to binary 1 (predicted present) vs. 0 (predicted absent) values.
# Use the 'ifelse' function and a 'PredictedPresence[,"0"]' threshold of <0.5 to populate the 'PredictedPresence' column with '1' (predicted present) and '0' (predicted absent) values.

	Etheostoma_olmstedi$PredictedPresence <- ifelse(Etheostoma_olmstedi$PredictPres[,"0"] < 0.5, 1, 0)

# Remove the 'PredictPres' columns.

	Etheostoma_olmstedi <- subset(Etheostoma_olmstedi, select = -c(PredictPres))


# Now use the 'table' function to build the 2x2 confusion matrix.

	table(Etheostoma_olmstedi$PredictedPresence, Etheostoma_olmstedi$Etheostoma_olmstedi_presence)


# Build the ROC (Receiver Operating Characteristic) curve using the 'roc' function from the 'pROC' package.
# Specificity = probability of correctly predicting absence (true negative rate).
# Sensitivity = probability of correctly predicting presence (true positive rate).

	ROCobject1 <- roc(Etheostoma_olmstedi$Etheostoma_olmstedi_presence, Etheostoma_olmstedi$PredictedPresence)
	
	ggroc(ROCobject1)
	

# Calculate AUC (Area Under the Curve) using the 'auc' function from the 'pROC' package.

	auc(ROCobject1)



#-----------------------------------------------------------------
# Step 2. Use the Classification Tree model to predict Etheostoma_olmstedi presence for a new dataset ('Etheostoma_olmstedi_Test').
#-----------------------------------------------------------------

# Load the 'Etheostoma_olmstedi_Test' data.

	TestData <- read.csv("./RandomForest/Etheostoma_olmstedi_Test.csv")


# Be sure the 'Etheostoma_olmstedi_presence' column (1 = presence, 0 = absence) is recognized as FACTOR data.

	TestData$Etheostoma_olmstedi_presence <- as.factor (TestData$Etheostoma_olmstedi_presence)  ## This line makes sure the 1,0 PA data will be treated as binary


# Delete any rows (replicates) with missing data.

	TestData <- TestData[complete.cases(TestData), ]


# Use the 'EthOlm_Pres' model to predict Etheostoma_olmstedi presence for the Test data.

	TestData$PredictPres <- predict(EthOlm_Pres, newdata = TestData)


# Add a new 'PredictedPresence' column to the [TestData] data.
# Convert the 'PredictedPresence[,"1"]' column to binary 1 (predicted present) vs. 0 (predicted absent) values.
# Use the 'ifelse' function and a 'PredictedPresence[,"0"]' threshold of <0.5 to populate the 'PredictedPresence' column with '1' (predicted present) and '0' (predicted absent) values.

	TestData$PredictedPresence <- ifelse(TestData$PredictPres[,"0"] < 0.5, 1, 0)


# Remove the 'PredictPres' columns.

	TestData <- subset(TestData, select = -c(PredictPres))


# Now use the 'table' function to build the 2x2 confusion matrix for the Test Data.

	table(TestData$PredictedPresence, TestData$Etheostoma_olmstedi_presence)


# Build the ROC (Receiver Operating Characteristic) curve using the 'roc' function from the 'pROC' package.
# Specificity = probability of correctly predicting absence (true negative rate).
# Sensitivity = probability of correctly predicting presence (true positive rate).

	ROCobject2 <- roc(TestData$Etheostoma_olmstedi_presence, TestData$PredictedPresence)
	
	ggroc(ROCobject2)
	

# Calculate AUC (Area Under the Curve) using the 'auc' function from the 'pROC' package.

	auc(ROCobject2)



#-----------------------------------------------------------------
# Step 3. Build a Random Forest model of Etheostoma_olmstedi presence, using the original 'Etheostoma_olmstedi_Train.csv' dataset.
#-----------------------------------------------------------------

# Load the 'Etheostoma_olmstedi_Train' data.

	Etheostoma_olmstedi <- read.csv("./RandomForest/Etheostoma_olmstedi_Train.csv")


# Be sure the 'Etheostoma_olmstedi_presence' column (1 = presence, 0 = absence) is recognized as FACTOR data.

	Etheostoma_olmstedi$Etheostoma_olmstedi_presence <- as.factor (Etheostoma_olmstedi$Etheostoma_olmstedi_presence)  ## This line makes sure the 1,0 PA data will be treated as binary


# Delete any rows (replicates) with missing data.

	Etheostoma_olmstedi <- Etheostoma_olmstedi[complete.cases(Etheostoma_olmstedi), ]


# Build a Random Forest model using the 'randomForest' function and all predictor variables.

	RFmodel1 <- randomForest(Etheostoma_olmstedi_presence ~ ., data = Etheostoma_olmstedi, importance = TRUE)


# Examine the results, depicted as a 2x2 confusion matrix.

	RFmodel1



#-----------------------------------------------------------------
# End of code.
#-----------------------------------------------------------------