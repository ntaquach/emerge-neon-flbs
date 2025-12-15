## This is an R script to:
## 1) Build a Regression Tree (continuous response variable) model.
## 2) Compare the Regression Tree model with a Random Forest model.


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

  # setwd({'paste your working directory path here & delete curly brackets'})	# Update this path.



#-----------------------------------------------------------------
# Step 1. Build a Regression Tree of the VA County level socioeconomic data.
#-----------------------------------------------------------------

# Import the VA County level socioeconomic data.

    VAdata <- read.csv("./RandomForest/CountyData_VA.csv", header = T)


# Use the 'rpart' function to build a Regression Tree of the % of county residents with a college degree.
# Because % residents with college degree is a numerical/continuous response variable, set the 'method' parameter as 'anova'.

    VAcollegeTree <- rpart(College_degree ~ Lack_HealthIns + Renter_Occ + Smokers + Physically_Inactive +
	
						   Severe_Housing_Problems + Food_Insecure + Rural,
						 
						   data = VAdata, method = 'anova')


# Use the 'rpart.plot' function to view the Regression Tree.

    rpart.plot(VAcollegeTree)


# Add a new 'PredictedCollege' column to the [VAdata] data.
# Populate the 'PredictedCollege' column with model-predicted values.
# The predicted values will hopefully be similar to the observed values, but will never be identical.

	VAdata$PredictedCollege <- predict(VAcollegeTree, newdata = VAdata)


# Create a scatterplot of the OBSERVED vs. PREDICTED values to see how well the model performs.

	CountyVA_CollegePlot <- ggplot(VAdata, aes(x = College_degree, y = PredictedCollege)) +
	
								 stat_poly_line() + stat_poly_eq() + geom_point() +
								 
								 xlab('Observed college degrees (%)') + ylab('Predicted college degrees (%)')

	CountyVA_CollegePlot



#-----------------------------------------------------------------
# Step 2. Use the 'VAcollegeTree' Regression Tree model to predict college degrees (%) for new data from California and Wisconsin.
#-----------------------------------------------------------------

# Start by importing the same data, but for counties in CA & WI.

    CA_WIdata <- read.csv("./RandomForest/CountyData_CA_WI.csv", header = T)


# Add a new 'PredictedCollege' column to the [CA_WIdata] data.
# Populate the 'PredictedCollege' column with model-predicted (from the VA model) values.

	CA_WIdata$PredictedCollege <- predict(VAcollegeTree, newdata = CA_WIdata)


# Create a scatterplot of the OBSERVED vs. PREDICTED values to see how well the model performs for the new counties.

	County_CA_WI_CollegePlot <- ggplot(CA_WIdata, aes(x = College_degree, y = PredictedCollege)) +
	
								 stat_poly_line() + stat_poly_eq() + geom_point() +
								 
								 xlab('Observed college degrees (%)') + ylab('Predicted college degrees (%)')

	County_CA_WI_CollegePlot



#-----------------------------------------------------------------
# Step 3. Build a Random Forest model 'College_degree', instead of Regression Tree.
#-----------------------------------------------------------------

# Import a fresh copy of "CountyData_VA.csv".

	VAdata <- read.csv("./RandomForest/CountyData_VA.csv", header = T)


# Build the Random Forest model using the same 7 predictor variables.
  
	VAcollegeRF <- randomForest(College_degree ~ Lack_HealthIns + Renter_Occ + Smokers + Physically_Inactive +
	
								Severe_Housing_Problems + Food_Insecure + Rural, data = VAdata, mtry=2)

	VAcollegeRF


# Create a scatterplot of OBSERVED vs. PREDICTED values to see how well the 'VAcollegeRF' model performs.

	VAdata$Predict <- predict(VAcollegeRF, type = "response")

	VAcollegeRFplot <- ggplot(VAdata, aes(x = College_degree, y = Predict)) +
	
							  stat_poly_line() + stat_poly_eq() + geom_point() +
							  
							  xlab('Observed college degrees (%)') + ylab('Predicted college degrees (%)')

	VAcollegeRFplot


# Use the 'VAcollegeRF' Random Forest model to predict 'College_degree' for the independent 'CountyData_CA_WI.csv' data.

    CA_WIdata <- read.csv("./RandomForest/CountyData_CA_WI.csv", header = T)
	
	CA_WIdata$Predict <- predict(VAcollegeRF, newdata = CA_WIdata, type = "response")


# Create a scatterplot of the OBSERVED vs. PREDICTED values to see how well the model performs for the new test data.

	County_CA_WI_CollegePlotRF <- ggplot(CA_WIdata, aes(x = College_degree, y = Predict)) +
	
										 stat_poly_line() + stat_poly_eq() + geom_point() +
										 
										 xlab('Observed college degrees (%)') + ylab('Predicted college degrees (%)')

	County_CA_WI_CollegePlotRF



#-----------------------------------------------------------------
# Step 4. Examine some random forest diagnostic plots.
#-----------------------------------------------------------------

# Use the Variable Importance plot to rank the predictor variables.

	varImpPlot(VAcollegeRF)

# Use Partial Dependence Plots to examine the functional relationship between predictor and response variables.

	partialPlot(VAcollegeRF, pred.data = VAdata, x.var = 'Physically_Inactive', ylab = 'County residents w/ college degree (%)')

	partialPlot(VAcollegeRF, pred.data = VAdata, x.var = 'Renter_Occ', ylab = 'County residents w/ college degree (%)')



#-----------------------------------------------------------------
# End of script.
#-----------------------------------------------------------------