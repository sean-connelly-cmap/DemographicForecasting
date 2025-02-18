# CMAP | Noel Peterson, Alexis McAdams | 8/7/2021

# This script uses tidycensus to fetch PUMS data on 2019 headship rates by
# age (but not sex) for the 21-county modeling region


library(tidyverse)
library(tidycensus)

load("Output/PumaRegions.Rdata") #"puma_region" - key for identifying CMAP region PUMAs, created in Age_0_4_PUMS_Breakdown.R

# Get PUMS person-level age data ------------------------------------------

pums_il <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "17", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_in <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "18", year = 2019, survey = "acs5",
                   variables_filter = list(AGEP = 15:99), show_call = TRUE)
pums_wi <- get_pums(variables = c("PUMA", "AGEP", "SPORDER"), state = "55", year = 2019, survey = "acs5",
                    variables_filter = list(AGEP = 15:99), show_call = TRUE)

# Join PUMS data to PUMA region assignments -------------------------------

pums_21co <- bind_rows(pums_il, pums_in) %>%
  bind_rows(pums_wi) %>%
  rename(ExactAge = AGEP) %>%
  mutate(IsHouseholder = SPORDER == 1,
         AgeGroup = case_when(
           ExactAge < 85 ~ paste(floor(as.double(ExactAge)/5)*5, "to", floor(as.double(ExactAge)/5)*5+4, "years"),
           TRUE ~ "85 years and over"
         )) %>%
  select(SERIALNO, PUMA, ST, PWGTP, IsHouseholder, ExactAge, AgeGroup)

pums_21co <- pums_21co %>%
  right_join(puma_region, by=c("PUMA" = "PUMACE10", "ST" = "STATEFP10")) %>%
  select(-SERIALNO, -GEOID10)

# Calculate headship rates for each age group by PUMA ---------------------

total_pop <- pums_21co %>%
  group_by(ST, PUMA, AgeGroup, Region) %>% #only change here was adding in region
  summarize(TotalPop = sum(PWGTP), .groups = 'drop') %>% # Sum person weights
  select(-ST, -PUMA) %>%
  group_by(AgeGroup, Region) %>%
  summarise(TotalPop = sum(TotalPop))

householders <- pums_21co %>%
  filter(IsHouseholder == TRUE) %>%
  group_by(ST, PUMA, AgeGroup, Region) %>%
  summarize(NumHouseholders = sum(PWGTP), .groups='drop') %>%
  select(-ST, -PUMA) %>%
  group_by(AgeGroup, Region) %>%
  summarise(NumHouseholders = sum(NumHouseholders))# Sum person weights

HEADSHIP_RATES <- left_join(householders, total_pop) %>%
  mutate(HeadshipRate = NumHouseholders / TotalPop)

HEADSHIP_RATES <- HEADSHIP_RATES %>% rename(Age = AgeGroup) %>% select(-NumHouseholders, -TotalPop)

save(HEADSHIP_RATES, file="Output/PUMS_HeadshipRates.Rdata")
