## This is an R script to:

## 1) Install Rtools (if Windows).
## 2) Install Rstudio version 2025_05.
## 3) Install the 'installr' package directly from CRAN, then update your R version.
## 4) Install 14 additional R packages directly from CRAN.
## 5) Separately install 2 R packages from GitHub.



##############################################################################################
# Step 1. Install Rtools (for Windows machines).
##############################################################################################

# Rtools is standalone software. It's installation will run EXTERNAL to R and Rstudio.
# Download from: https://cran.r-project.org/bin/windows/Rtools/

# Or download and use the 'rtools45-6608-6492' install file provided in the Google Drive folder.



##############################################################################################
# Step 2. Install Rstudio version 2025_5.
##############################################################################################

# This is NOT the latest version of Rstudio!
# Unfortunately, the latest version (2025_09) is NOT compatible with the NEON Data Portal.
# Luckily, there is nothing complicated to re-install an older version.

# Just download and run the 'RStudio-2025.05.1-513' file (Windows) or the 'RStudio-2025.05.1-513.dmg' file (Mac).
# Both install files are provided in the Google Drive folder.



##############################################################################################
# Step 3. Install 'installr' from CRAN.
##############################################################################################

# Run the following lines to make sure you have the latest version of R (which is separate from Rstudio).

  install.packages('installr')
  
  library(installr)
  
  updateR()

# When you run the 'updateR' function, a prompt will tell you if you are using the latest version of R.
# If not, pop-up prompts will guide you through the updating process.



##############################################################################################
# Step 4. Install 14 additional R packages from CRAN.
##############################################################################################

  install.packages('neonUtilities')
  install.packages('remotes')
  install.packages('tidyverse')
  install.packages('vegan')
  install.packages('ggrepel')
  install.packages('randomForest')
  install.packages('missForest')
  install.packages('deSolve')
  install.packages('shipunov')
  install.packages('geometry')
  install.packages('rpart')
  install.packages('rpart.plot')
  install.packages('ggpmisc')
  install.packages('rstan')



##############################################################################################
# Step 5. Install the 'ganttrify' and 'streamMetabolizer' R packages from GitHub.
##############################################################################################

# Download R package 'ganttrify' directly from GitHub.
# Lots of info on 'ganttrify' is available at https://github.com/giocomai/ganttrify.

  library(remotes)

  remotes::install_github('giocomai/ganttrify', dependencies = TRUE, force = TRUE)
  
  library(ganttrify)



# Download R package 'streamMetabolizer' directly from GitHub.

   Sys.setenv(GITHUB_PAT = 'copy your GitHub token here')  # Update this line.

   remotes::install_github('appling/unitted', force = TRUE)

   remotes::install_github('USGS-R/streamMetabolizer', build_vignettes = FALSE, force = TRUE)
   
   library(streamMetabolizer)



### End of code.