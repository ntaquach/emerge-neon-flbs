## This is an R script to calculate Gross Primary Production and Ecosystem Respiration using 'streamMetabolizer'.
## Formatted temperature, dissolved oxygen, and discharge data will be used, after running the "+Rscript_NEON_Q_Temp_DO" script.



###############################################################################
# Step 0. Load R libraries and set the working directory.
###############################################################################

# Manually install rtools (https://cran.r-project.org/bin/windows/Rtools/)



# Install and load 'remotes' library.
# Must do this to install 'streamMetabolizer' from GitHub.

   install.packages("remotes")

   library(remotes)



# Install 'streamMetabolizer' from GitHub, then load.

   Sys.setenv(GITHUB_PAT = 'copy your GitHub token here')  # Update this line.

   remotes::install_github('appling/unitted')

   remotes::install_github("USGS-R/streamMetabolizer", build_vignettes = FALSE)
   
   library(streamMetabolizer)



# Install and load remaining R libraries.

   install.packages("deSolve")
   install.packages("rstan")
   
   library(tidyverse)
   library(deSolve)
   library(rstan)



# Set the R working directory. 

   # setwd('copy your working directory path here')	# Update this path.




###############################################################################
# Step 1. Specify constants for use in streamMetabolizer.
###############################################################################

# These are the constants you will need. Enter your own values.

   lat<- 35.690428 # Latitude in units of decimal degrees.

   long<- -83.50379 # Longitude in units of decimal degrees. Note the negative sign '-' for folks in the western hemisphere.

   altitude<- 579  # Altitude in units of meters above mean sea level (ASL).

   bpstd <- 1013  # Barometric pressure. Do not change this 'bpstd' value. Ignoring weather related changes in bp for this minimal code (1013 mb = 760 mm Hg).  

   K600_guess <- 275.7  # Gas exchange rate in units of 1/day. Enter your best guess?
					 # Big slow rivers = 0.5 to 2. Medium slow rivers = 2-10. 
					 # Fun rivers to float = 10-20. Non-bubbly streams = 20-40.
					 # Bubbly streams = 50-200.
					 
   mean_depth <- 0.15  # Mean river depth in units of meters.



# Use this function to obtain barometric pressure in units of millibars (for streamMetabolizer).

   bpcalc_atm<- function(bpst, alt) {
   
   bpst * exp((-9.80665  *0.0289644 * alt) / (8.31447 * (273.15 + 15)))

   }

   bpcalc_atm(bpst = bpstd, alt = altitude)




###############################################################################
# Step 2. Import the formatted Temperature and Dissolved Oxygen time-series data.
###############################################################################

# The time-series should range from 5 to 50 days in length.

# The imported .csv file must be formatted exactly like this,
# where unixtime is "Unix" , temp is  "Temperature_degC", and oxy is  "Dissolved Oxygen_mgL".
# NEVER USE PME % saturation data.

# unixtime   temp   oxy
# 1721418480 18.048 8.467
# 1721419080 16.720 8.869
# 1721419680 16.490 8.965

   oxy <- read.csv('./RandomForest/Temp_DO_Discharge.csv')
   
   head(oxy)  # Do the data look ok?




###############################################################################
# Step 3. Use 'streamMetabolizer' to estimate Gross Primary Production and Ecosystem Respiration.
###############################################################################

# Run the following code without making any changes!


# Convert unix time to a R time object in UTC.

   oxy$time <- as_datetime(oxy$unixtime)

   oxy$solar.time <- convert_UTC_to_solartime(oxy$time, longitude = long, time.type = "mean solar")

   oxy$light <- calc_light(oxy$solar.time, latitude = lat, longitude = long, max.PAR = 2326, attach.units = FALSE)

   oxy$osat <- calc_DO_sat(temp.water = oxy$temp, pressure.air = bpcalc_atm(bpst = bpstd, alt = altitude))



# Set up the streamMetabolizer model and specs. We're ignoring discharge variation here with a simpler pooling model.

   model_name <- mm_name(type = 'bayes', pool_K600 = 'normal', err_obs_iid = TRUE, err_proc_iid = TRUE)

   model_specs <- specs(model_name, K600_daily_meanlog_meanlog = log(K600_guess), K600_daily_meanlog_sdlog = 0.7,
   
						K600_daily_sdlog_sigma = 0.1, burnin_steps = 1000, saved_steps = 1000) 



# Create a [data_sm] dataframe to store model inputs.

   data_sm <- data.frame(DO.obs = oxy$oxy, DO.sat = oxy$osat,
   
						 temp.water = oxy$temp, depth = rep(mean_depth, length(oxy$oxy)),
                    
						 light = oxy$light, solar.time = oxy$solar.time)


   head(data_sm)  # Always check the dataframe!



# Fit the streamMetabolizer model.

   fit <- metab(model_specs, data = data_sm, info = c(site = 'your_site', source = 'HT Odum'))



# Plot model predictions.

   plot_DO_preds(predict_DO(fit))

   plot_metab_preds(predict_metab(fit))

   params<- get_params(fit , uncertainty = 'ci')

   plot(params$K600.daily, params$ER.daily)  # If these covary, then equifinality warning!



# Return model-estimated GPP and ER.

   mean(params$GPP.daily, na.rm = TRUE)

   mean(params$ER.daily, na.rm = TRUE)




### End of code.