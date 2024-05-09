# CMAP | Alexis McAdams, Mary Weber | 8/11/2021

# Hey you! Make sure to run setup_control.R BEFORE running this script :)
#
# This script runs a loop to cycle through the Projection.R script over
# and over to get population projections in 5 year increments up to 2050.
# For each projection period, the Population projection and Net Migration
# are saved in lists (POPPROJ and NETMIGPROJ, respectively) and are used as
# input for the subsequent projection period.
#
# Formatting, export, and secondary projection products are handled in
# the script called "final_control.R"

# AB notes -- this file relies on another R file to loop through projection years to do the projection
# I think this set up makes sense, but the loop currently includes a lot that doesn't need to run each iteration,
# so part of my efforts are to pare the loop portion down as much as possible


# set params --------------------------------------------------------------

######## load libraries ----------------

library(dplyr)
library(tidyverse)
library(readxl)
library(ggplot2)
library(tidycensus)
library(janitor)

######## set-up projection start/end variables ---------------

baseyear <- 2022
startyear <- 2022
endyear <- 2050

projnums <- ceiling((endyear - startyear) / 5) #number of 5-year projection cycles to complete

series <- seq(from=(startyear - (startyear %% 5)),
              to=endyear, by= 5)
series[1] <- startyear

target_NM_Sex_check <- tibble()  ###################################### temporary!
base_year_NM_check <- tibble()   ###################################### temporary!


######## set up the population projection and migration projection lists

POPPROJ <- list() # total population
for(years in series){
  POPPROJ[[as.character(years)]] <- tibble()
}

NETMIGPROJ <- list() # Net migrants
for(years in series){
  NETMIGPROJ[[as.character(years)]] <- tibble()
}

COMPONENTS <- list() # components of change (births, deaths, migrants)
for(years in series){
  COMPONENTS[[as.character(years)]] <- tibble()
}

MIG_DETAIL <- list() # net migrants, detailed data
for(years in series){
  MIG_DETAIL[[as.character(years)]] <- tibble()
}


# load depedent data ------------------------------------------------------
labor_force_proj <- read_excel("Input/cc_labor_force.xlsx")
hh_pop_16_plus_proj <- read_excel("Input/cc_hh_gq_pop.xlsx")

load("Output/Mort_Proj.Rdata")    # Mort_Proj (from Mortality.R)
load("Output/BirthRatios.Rdata")  # F_Groups, bRatios (from Fertility.R)
load("Output/ASFR.Rdata")         # ASFR_projections (from setup_control.R or Fertility.R)
load("Output/POP_PEP.Rdata") # POP
load("Output/BirthRatios.Rdata") #gender_ratios (from Fertility)
load("Output/International_mig_proj.Rdata") # intl_imm_project_five_year (from international migration)
load("Output/GQData2.Rdata") # gq, gq_military, gq_ratios
load("Output/pums_worker_migration.Rdata") #labor_force_movers_demo, labor_force_multiplier, from domestic_migration.R


######## run the loop ---------------

i <- 1
while(i <= projnums){

#set up variables for Projection.R
projstart <- series[i]
projend <- series[i+1]
projmidpoint  <- (projstart + projend) / 2
projyears <- seq(from = projstart,
                 to = projend - 1)

print(paste("Creating forecast for the period",projstart, "to", projend, sep=" "))
#run the projection code
source("src/07a_projection_loop.R")

#save the MigrationProjections.R outputs in list format

POPPROJ[[as.character(projend)]] <- final_projection
i <- i+1
}

#save the Net Migration rates
NETMIGPROJ[[as.character(projend)]] <- Migration

#save the Components of Change
COMPONENTS[[as.character(projend)]] <- Components

#save the detailed Migration data
MIG_DETAIL[[as.character(projend)]] <- detailedMigs

#-------
  i <- i+1
}

#belatedly put the starting migration rates in the final list (see Projection.R 1st proj period loop, ~ line 47)
start_Base_Mig <- start_Base_Mig %>% select(Region, Age, Sex, NetRates) %>% rename(NMRs = NetRates)
NETMIGPROJ[[1]] <- start_Base_Mig


######## Final Steps

#upload the finished projection lists to GitHub

save(POPPROJ, file="Output/PopProj.Rdata")
save(NETMIGPROJ, file="Output/NMProj.Rdata")
save(COMPONENTS, file="Output/ComponentsOfChange.Rdata")
save(MIG_DETAIL, file = "Output/MigTesting.Rdata")

#Recordkeeping list ("SETTINGS")
projection_options <- c('External IL Area Adjustment to Base Pop' = EXTIL,
                        'COVID19 Deaths Adjustment to Base Pop' = c19deaths,
                        'Total Net Migration Target Values' = TNMfilename,
                        'Coarse Migration Characteristics Override' = override,
                        'Zero Migration Scenario Override' = zeromigrationoverride
)
load("Output/recordkeeping.Rdata") # SETTINGS
SETTINGS[[2]] <- projection_options
save(SETTINGS, file = "Output/recordkeeping.Rdata")
