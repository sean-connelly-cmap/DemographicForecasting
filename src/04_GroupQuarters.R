# CMAP | Mary Weber, Alexis McAdams, Alex Bahls | 7/14/2023

##### GroupQuarters.R

# This script fetches detailed 2020 Group Quarters populations and calculates ratios of
# GQ type to Pop for each Age group and Sex by Region (with the exception of Military).
#

library(tidyverse)
library(tidycensus)

# Set parameters ----------------------------------------------------------

load("Output/POP_PEP.Rdata") # POP

CMAP_GEOIDS <- cmapgeo::county_fips_codes$cmap

COUNTIES <- list(
  IL = c(31, 43, 89, 93, 97, 111, 197,       # CMAP counties
         7, 37, 63, 91, 99, 103, 141, 201),  # Non-CMAP Illinois counties
  IN = c(89, 91, 127),                       # Indiana counties
  WI = c(59, 101, 127)                       # Wisconsin counties
)

#PCO07 is the sum of 8, 9, and 10 -- since we are pulling all the parts we dont need it
GQ_TABLES <- c("PCO10", "PCO9", "PCO8", "PCO6", "PCO5", "PCO4", "PCO3")

YEAR <- 2020

# Step 1: fetch historical GQ data ------------------------------------------

# Compile list of variables to download
DHC_VARS <- load_variables(YEAR, "dhc")
GQ_VARS <- DHC_VARS %>%
  filter(str_starts(name, paste0("^", paste(GQ_TABLES, collapse="|^")))) %>%
  mutate(
    Category = str_replace_all(label, ".*\\)!!", ""),
    Category = str_replace_all(Category, "!!", " ")
  )

# Download census GQ data for selected variables in all counties
GQ_DATA <- tibble()
for (STATE in names(COUNTIES)) {
  TEMP <- get_decennial(geography = "county", variables = GQ_VARS$name,
                        county = COUNTIES[[STATE]], state = STATE,
                        year = YEAR, sumfile = "dhc", cache_table = TRUE)
  GQ_DATA <- bind_rows(GQ_DATA, TEMP)
}


# Step 2 -- process Census pull -------------------------------------------

# Assemble final table of GQ population data, rename cols and reformat values
GQ <- GQ_DATA %>%
  left_join(GQ_VARS, by = c("variable" = "name")) %>%
  select(-label) %>%
  janitor::clean_names() %>%
  separate_wider_delim(name, delim = ", ",names = c("county", "state")) %>%
  separate_wider_delim(category, delim = ":", names = c(NA,"sex","age") ,too_few = "align_end") %>%  #might want to remove the last command
  mutate(age = case_when(is.na(age) | age == "" ~ "Total",
                         age == "Under 5 years" ~ "0 to 4 years",
                         T ~ str_trim(age)),
         sex = case_when(str_detect(sex,"Total") ~ "Total",
                         T ~ str_trim(sex)),
    Year = YEAR,
    Category = case_when(age == "Total" & sex == "Total" ~ "County Total",
                         age == "Total" & sex == "Male" ~ "County Male Total",
                         age == "Total" & sex == "Female"  ~ "County Female Total",
                         T ~ str_trim(paste0(sex," ",age))),
    Region_cc = case_when(geoid %in% CMAP_GEOIDS ~ geoid,
                       state == "Illinois" ~ "External IL",
                       state == "Indiana" ~ "External IN",
                       state == "Wisconsin" ~ "External WI",
                       T ~ NA)
  )

# Remove the rows for Population Totals, sum up the GQ populations by GQ type age and sex, fix 0-4 Age value
new_age_groups_to_remove <- c("Under 20 years","Under 25 years","25 years and over", "65 years and over")

GQlong <- GQ %>%
  filter(sex != "Total",
         age != "Total") %>%
  group_by(Region_cc, sex, age, concept) %>%
  summarize(Population = sum(value)) %>%
  filter(!age %in% new_age_groups_to_remove) %>%
  ungroup()

# Step 3: calculate GQ Ratios ------------------------------------------

# import the 2020 Census Population (these values are the all population, including GQ)
pop2020 <- POP[["2020"]] %>%
  group_by(Region_cc, Age, Sex) %>%
  summarize(totalpop = sum(Population)) |>
  rename(age = Age, sex = Sex)

# Join the GQ sums to 2020 population, calculate the GQ ratios for every GQ type
# GQ ratio = GQ pop / (totalpop)
GQratios <- left_join(GQlong, pop2020, by=c("Region_cc", "age", "sex")) %>%
  mutate(ratio = Population / totalpop,
         concept = word(concept, start = 5, end = 6)) %>% #shorten the Concept col values (types of GQs)
  select(!c("Population","totalpop")) %>%
  pivot_wider(names_from = concept, values_from = ratio) |>
  janitor::clean_names() |>
  select(!military_quarters) #remove military

# split out Military population total by age and sex - this value is held constant in future projections --
# note that everywhere outside of Lake County (naval base) has been 0 military in past few analyses
GQ_Military <- GQ %>%
  filter(concept == "GROUP QUARTERS POPULATION IN MILITARY QUARTERS BY SEX BY AGE",
         sex != "Total",
         age != "Total") |>
  group_by(Region_cc, sex, age) %>%
  mutate(value = sum(value)) %>%
  select(value, sex, age, Region_cc) |>
  unique() %>%
  ungroup()

save(GQ, GQ_Military, GQratios, file="Output/GQData2.Rdata")


