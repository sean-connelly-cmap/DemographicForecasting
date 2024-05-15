library(tidyverse)
library(readxl)
library(janitor)

#the economic forecast gives us the number of jobs in the region for the horizon period. Our balancing assumption is that if there are fewer workers
#than jobs in the region then people will move here to fill those jobs. The economic forecast uses historical data to forecast the size of the labor force
#necessary to fill the jobs in the region. Note that many people in the labor force will be unemployed as there are not enough jobs for everyone who wants one.

# the number of people labor force is determined by the equation (civilian noninstitutional population age 16 and older)*(labor force participation rate)

#what is "civilian noninstitutional population age 16 and older"? https://www.bls.gov/cps/definitions.htm#population

    # The civilian noninstitutional population age 16 and older is the base population group, or universe, used for Current Population Survey (CPS) statistics published by BLS. (See also geographic scope and reference of the CPS.)
    #
    # The civilian noninstitutional population excludes the following:
        # active duty members of the U.S. Armed Forces
        # people confined to, or living in, institutions or facilities such as
        # prisons, jails, and other correctional institutions and detention centers
        # residential care facilities such as skilled nursing homes

# notably, all household members over 16 is in the labor force, regardless of their age.

#using the forecast data, we can determine the size of the "civilian noninstitutional population age 16 and older". To get the size of the labor force,
#we need to forecast the labor force participation rate.

# this part of the code does the following
  #1. Compare CMAP and national LFPR -- the LFPR has regional differences due to a number of factors -- we want to get the ratio of CMAP LFPR to national
  #2. Grab national LFPR projections


# 1. Compare CMAP/national LFPR ------------------------------------------------


# https://ides.illinois.gov/resources/labor-market-information/laus/chicago-metropolitan-area-unemployment-rates.html
# download file -- "Annual Averages"

cma_lfpr <- read_xls("Input/Chicago-Naperville-ArlingtonHeights_MD_annavg.xls", skip = 6) |>
  clean_names() |>
  filter(!is.na(year)) |>
  select(year, cma_lfpr = labor_force_participation_rate)


#https://www.cbo.gov/data/budget-economic-data#12
#long term economic projections -- this version is March 2024 but updated annually

national_lfrp_for_ratio <- read_xlsx("Input/57054-2024-03-LTBO-econ.xlsx",
                                     sheet = "1. Econ Vars_Annual Rates", skip = 7) |>
  clean_names() |>
  filter(x1 == "Labor force participation rated") |>
  t() |>
  as_tibble() |>
  row_to_names(row = 1) |>
  clean_names() |>
  cbind(year = c(seq(1994, 2054))) |>
  select(year, national_lfpr = labor_force_participation_rated) |>
  left_join(cma_lfpr) |>
  filter(!is.na(cma_lfpr)) |>
  mutate(cma_to_national = cma_lfpr/as.numeric(national_lfpr))

#using this makes the total pop a bit lower
cma_ratio_lfpr <- national_lfrp_for_ratio |> filter(year %in% seq(2014, 2018, 1)) |> mutate(mean = mean(cma_to_national)) |> distinct(mean) |> pull()


# 2. process CBO LFPR projections -----------------------------------------

#long term LFPR projections from CBO; see link below, "Long-Term Economic Projections", "Mar 2024"
#https://www.cbo.gov/data/budget-economic-data

cbo_lfrp_proj_raw <- read_excel("Input/57054-2024-03-LTBO-econ.xlsx",
                                sheet = "2. Actual LFP Rates",
                                skip = 7)

cbo_lfrp_proj_cbo_ages <- cbo_lfrp_proj_raw |>
  row_to_names(row_number = 1) |>
  clean_names() |>
  select(!c(na,all_people_age_16_or_older, na_2, age_16_or_older, age_16_or_older_2)) |>
  pivot_longer(cols = c(ages_16_to_17:ages_80_to_89_2),
               names_to = "age_temp") |>
  mutate(sex = case_when(
    str_sub(age_temp,-2,-1) == "_2" ~ "female",
    T ~ "male"
  ),
  age_cbo_buckets = case_when(
    str_sub(age_temp,-2,-1) == "_2" ~ str_remove_all(str_c(str_replace_all(str_remove_all(age_temp, "ages_"),"_"," "), "  years"), " 2 "),
    T ~ str_c(str_replace_all(str_remove_all(age_temp, "ages_"),"_"," "), " years")
  ),
  lfpr_proj = (as.numeric(value) / 100)
  ) |>
  select(year, lfpr_proj, sex, age_cbo_buckets) |>
  filter(!is.na(lfpr_proj))

cbo_proc_forecast_ages <- cbo_lfrp_proj_cbo_ages |>
  mutate(age = case_when(
    age_cbo_buckets %in% c("16 to 17 years","18 to 19 years") ~ "15 to 19 years",
    age_cbo_buckets %in% c("60 to 61 years","62 to 64 years") ~ "60 to 64 years",
    T ~ age_cbo_buckets
  ),
  year_group = case_when(floor(as.numeric(year)/5)*5 == 2020 ~ 2022,
                         T ~ floor(as.numeric(year)/5)*5)) |>
  filter(!is.na(year_group)) |>  #nas are just extra rows from excel sheet, nothing we need
  group_by(year_group, age, sex) |>
  summarize(average_lfpr = mean(lfpr_proj))

save(cbo_proc_forecast_ages, file="Output/Econ_Data.Rdata")

