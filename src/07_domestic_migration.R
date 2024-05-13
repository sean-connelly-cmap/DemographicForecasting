library(tidyverse)
library(tidycensus)
library(janitor)
library(readxl)

#list of (2010) PUMAs, mig pumas
pums_2010_il <- tigris::pumas(state = "IL", year = 2019) |>  #the 2021 PUMS data uses the 2010 PUMAs so don't want most recent
  clean_names()

pums_2010_region <- pums_2010_il |>
  filter(str_detect(namelsad10, paste(c(names(cmapgeo::county_fips_codes$cmap), "Chicago"), collapse = "|")) &
           !str_detect(namelsad10,"Williamson")) #note that Kendall and Grundy county are grouped in 2010 PUMAs

# some context -- PUMS data includes information about migration -- specifically it says if someone moved in the previous year and
# where they moved from. The data about where they currently live is at the PUMA level, but the data about where they used to live
# is aggregated to the "MIGPUMA" level due to privacy concerns. MIGPUMAs are groups of PUMAs. For this analysis we will need both

#downloaded from IPUMS: https://usa.ipums.org/usa/volii/10migpuma.shtml
mig_puma_xwalk <- read_excel("Input/puma_migpuma1_pwpuma00_2010.xls", skip = 1) |> clean_names()

mig_pumas_cmap <- mig_puma_xwalk |>
  filter(state_of_residence_st == "17", puma %in% pums_2010_region$pumace10) |>
  distinct(pwpuma00_or_migpuma1) |>
  pull()

#investigate migpumas
# mig_pumas_touch_region <- mig_puma_xwalk |>
#   filter(state_of_residence_st == "17",
#          pwpuma00_or_migpuma1 %in% mig_pumas_cmap) |>
#   mutate(in_region = case_when(
#     puma %in% pums_2010_region$pumace10 ~ 1,
#     T ~ 0
#   )) |>
#   group_by(pwpuma00_or_migpuma1) |>
#   mutate(percent_in_region = sum(in_region)/n())
#
# table(mig_pumas_touch_region$percent_in_region) #so includes grundy plus one additional puma seems fine
#


# pull PUMS data from API -------------------------------------------------

vars <- pums_variables |>
  filter(year == 2021,
         survey == "acs5")

vars_trim <- vars |> distinct(var_code, var_label)

pums_var_list <- c("SERIALNO", "PUMA", "WGTP", "PWGTP", "AGEP", "ESR", "MIG", "MIGPUMA", "MIGSP", "SEX")

#note this pull will take a min
pums_pull <- get_pums(variables = pums_var_list,
                      state = "IL",
                      year = 2021,
                      survey = "acs5",
                      puma = pums_2010_region$pumace10,
                      recode = T)

moved_into_region_work <- pums_pull |>
  mutate(recent_dom_mover_temp = case_when(
    MIG_label %in% c("Yes, same house (nonmovers)", "No, outside US and Puerto Rico") ~ 0, # internaitonal mig handled separately
    MIG_label == "No, different house in US or Puerto Rico" & (MIGPUMA %in% mig_pumas_cmap & MIGSP == "017")  ~ 0, #people who moved within the region
    MIG_label == "No, different house in US or Puerto Rico" & !(MIGPUMA %in% mig_pumas_cmap & MIGSP == "017") ~ 1,
    T ~ NA #babies
  ),
  in_lf = case_when(
    ESR_label %in% c("N/A (less than 16 years old)","Not in labor force","Armed forces, at work","Armed forces, with a job but not at work") ~ 0,
    T ~ 1
  )) |>
  group_by(SERIALNO) |>  #households
  mutate(percent_recent_mover = mean(recent_dom_mover_temp, na.rm = T),
         number_in_lf = sum(in_lf),
         hh_size = n()) |>
  filter(percent_recent_mover > 0.5,
         number_in_lf > 0) |>
  ungroup()

labor_force_multiplier <- moved_into_region_work |>
  summarize(number_workers = sum(in_lf*PWGTP), #pums data is weighted, this variable tells you the number of people the row represents
            number_people = sum(PWGTP),
            labor_force_multiplier = number_people / number_workers) |>
  pull(labor_force_multiplier)

labor_force_movers_demo <- moved_into_region_work |>
  mutate(sex = str_to_lower(SEX_label),
         age_temp = as.character(cut(AGEP, breaks = seq(0, 85,5), right = F)),
         age_bound_high = as.character(as.numeric(str_sub(age_temp, -3,-2)) - 1), #these create warning but
         age_bound_low = str_sub(age_temp, 2,3),
         age = case_when(
           is.na(age_temp) ~ "85 years and over",
           age_temp == "[0,5)" ~ "0 to 4 years",
           age_temp == "[5,10)" ~ "5 to 9 years",
           T ~ str_c(age_bound_low, " to ", age_bound_high, " years")
                        )
          )


labor_force_movers_demo <- moved_into_region_work |>
  mutate(sex = str_to_lower(SEX_label),
         age_temp = as.character(cut(AGEP, breaks = seq(0, 85,5), right = F)), #this creates some NAs, the
         age = case_when(
           is.na(age_temp) ~ "85 years and over",
           age_temp == "[0,5)" ~ "0 to 4 years",
           age_temp == "[5,10)" ~ "5 to 9 years",
           T ~ str_c(str_sub(age_temp, 2,3), " to ", as.character(as.numeric(str_sub(age_temp, -3,-2)) - 1), " years")
         )
  ) |>
  group_by(sex, age) |>
  summarize(population = sum(PWGTP)) |>
  ungroup() |>
  mutate(percentage = population/sum(population)) |>
  select(!population)


save(labor_force_movers_demo, labor_force_multiplier, file="Output/pums_worker_migration.Rdata")

