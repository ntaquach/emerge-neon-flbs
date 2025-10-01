#install.packages("neonUtilities")
#install.packages("remotes")
#install.packages("tidyverse")
#install.packages("vegan")
#install.packages("shipunov")

Sys.setenv(GITHUB_PAT = "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")

remotes::install_github("giocomai/ganttrify", build_vignettes = F)

library(neonUtilities)
library(remotes)
library(tidyverse)
library(vegan)
library(shipunov)
library(ganttrify)
library(randomForest)
library(deSolve)
library(rstan)



#GitHub fine grained token ===============  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
