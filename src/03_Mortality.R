# CMAP | Noel Peterson, Mary Weber, Alex Bahls | 7/12/2021

#need to update the input dataset

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)
library(janitor)

#looked into using CDC wonder data but it had too many supressed counts to use reliably
#i.e. in 2019 external WI there are 44/138 county/age/sex combinations with suppressed counts

# Parameters ---------------------------------------------------------

load("Output/POP_PEP.Rdata")
load("Output/Age_0_4_Freq.Rdata")

MORT_YEARS <- c(2014:2018)
DEATHS_XLSX <- "Input/CMAPMortality1990-2019.xlsx"

county_fips_index <- as_tibble(cbind(county_name = names(cmapgeo::county_fips_codes$cmap),
                                     county_code = cmapgeo::county_fips_codes$cmap))

# Create mortality age groups ---------------------------------------------------------

mort_pop <- tibble()

for (YEAR in MORT_YEARS) {
  mort_pop <- bind_rows(mort_pop, POP[[as.character(YEAR)]])
}

# Load deaths data
deaths <- read_excel(DEATHS_XLSX) %>%
  filter(Year %in% MORT_YEARS) %>%
  mutate(geoid = as.character(GEOID),
         age = case_when(Age %in% c("85 to 89 years", "90 to 94 years", "95 years and over") ~ "85 years and over",
                         TRUE ~ Age),
         region_cc = case_when(
           Region == "CMAP Region" ~ geoid,
           T ~ Region
         )) %>%
  clean_names() |>
  group_by(geoid = geoid_2, sex, age = age_2, year, region_cc) %>%
  summarize(mortality = sum(mortality), .groups = "drop")

# Join pop to deaths
mort_data <- mort_pop %>%
  mutate(age = ifelse(age == "85 years and older","85 years and over", age)) %>%
  full_join(deaths, by=c('geoid', 'age', 'sex', 'year', 'region_cc')) %>%
  group_by(age, sex, region_cc) %>%
  summarise(population = sum(population),
            mortality = sum(mortality),
            .groups = "drop") %>%
  arrange(region_cc, desc(sex))

# Use PUMA proportion estimates for 0-1 and 1-4 age group -------------------------------------

age_0_4_prop <- age_0_4_freq %>%
  mutate(age = case_when(age_group == 'Less than 1 year' ~ '0 to 1 years',
                         TRUE ~ age_group)) %>%
  left_join(county_fips_index, by = c("region_cc" = "county_name")) |>
  mutate(region_cc = case_when(
    str_detect(region_cc, "External") ~ region_cc,
    T ~ county_code
  )) |>
  select(-c(age_group,population, county_code))

mort_data_with_1_4 <- mort_data %>%
  left_join(age_0_4_prop, by=c('age','region_cc', 'sex')) |>
  mutate(population = case_when(age == '0 to 1 years' ~ lead(population)*age_0_4_share,
                                age == '1 to 4 years' ~ lag(population)*age_0_4_share,
                                TRUE ~ population)) %>%
  select(!age_0_4_share) %>%
  filter(age != '0 to 4 years')

# Life Tables Calculations ---------------------------------------------------------

#Life tables are a common means of constructing survival rates -- see forecast book p. 54

#ab note 4/29/2024 -- dont think we actually need these but keeping in for now just in case

LT <- tibble(age = unique(deaths$age)) %>%
  mutate(x = as.numeric(str_split_fixed(age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

#i think Ax is an assumption about when in the internal people will die;
#we assume the average person that dies in the five year interval dies halfway through
#but for babies, much more likely to die earlier in interval;
#we expect more deaths right after birth than 9 months after birth


#see page 54 of forecast book for variable definitions
life_table <- mort_data_with_1_4 %>%
  left_join(LT, by="age") %>%
  select(region_cc, sex, age, mortality, population, x, Ax) %>%
  arrange(region_cc, desc(sex), x) %>%
  group_by(region_cc, sex) %>%
  mutate(Mx = (mortality/as.numeric(population)), #Mortality rate (using mid-year population)
    n = case_when(age == '0 to 1 years' ~ 1, # n -- number of years in interval
                  age == '1 to 4 years' ~ 4,
                  age == '85 years and over' ~ 2/Mx, #I have thought about this calc a lot and don't have a perfect
                              #conceptual understanding of it; however the n value for 85+ isn't used in subsequent code
                  TRUE ~ 5),
    Qx = ifelse(age == '85 years and over', 1,  # 85+ should always be 1
                (n*Mx/(1+n*(1-Ax)*Mx))), #proportion that will die during that age internal; formula to adjust for mid-year population denom
    Px = (1-Qx), #probabilty of living through age period
    Ix = head(accumulate(Px, `*`, .init=100000), -1), # 0-1 should always be 100000 -- number of people who reach age interval; 100000*p
    Dx = (ifelse(age == '85 years and over', Ix, Ix -lead(Ix))), #number of people that will die during internal
    Lx = (ifelse(age == '85 years and over', Ix/Mx, n*(lead(Ix)+(Ax*Dx)))), # of person years lived
    temp = ifelse(age == '85 years and over', Lx, 0),
    Tx = (ifelse(age == '85 years and Over', Lx, accumulate(Lx, `+`, .dir = "backward"))), #cumulative number of person years lived after internval
    Ex = (Tx/Ix), #life expectancy, number of people over number of future person years
    Sx = case_when(age == '0 to 1 years' ~ Lx/Ix,
                   age == '1 to 4 years' ~ Lx/(lag(Lx)*4),
                   age == '5 to 9 years' ~ Lx/(lag(Lx) + lag(Lx, n = 2)),
                   age == '85 years and over' ~ Lx/(Lx +lag(Lx)),
                   TRUE ~ Lx/lag(Lx)) #survival rate in life years
  ) %>%
  select(-temp) %>%
  relocate(c(x, n, Ax), .before= mortality) %>%
  ungroup()

# Read in Census tables -----------------------------------------------------------

#need 2017 for baseline to yoke to using new projections
#https://www.census.gov/data/datasets/2017/demo/popproj/2017-popproj.html
#7. Projected Mortality Rates by Nativity, Age, Sex, Race, and Hispanic Origin for the United States: 2017 to 2060
census_mort_proj_17 <- read_csv("Input/np2017_a2.csv")

census_17_proc <- census_mort_proj_17 |>
  filter(group == 0,
         sex != 0,
         nativity == 0,
         year == 2017) |>
  mutate(Sex = ifelse(sex == 1, "Male", "Female"),
         Mort_0_1 = ASDR_0,
         Mort_1_4 = rowMeans(across(ASDR_1:ASDR_4)),
         Mort_5_9 = rowMeans(across(ASDR_5:ASDR_9)),
         Mort_10_14 = rowMeans(across(ASDR_10:ASDR_14)),
         Mort_15_19 = rowMeans(across(ASDR_15:ASDR_19)),
         Mort_20_24 = rowMeans(across(ASDR_20:ASDR_24)),
         Mort_25_29 = rowMeans(across(ASDR_25:ASDR_29)),
         Mort_30_34 = rowMeans(across(ASDR_30:ASDR_34)),
         Mort_35_39 = rowMeans(across(ASDR_35:ASDR_39)),
         Mort_40_44 = rowMeans(across(ASDR_40:ASDR_44)),
         Mort_45_49 = rowMeans(across(ASDR_45:ASDR_49)),
         Mort_50_54 = rowMeans(across(ASDR_50:ASDR_54)),
         Mort_55_59 = rowMeans(across(ASDR_55:ASDR_59)),
         Mort_60_64 = rowMeans(across(ASDR_60:ASDR_64)),
         Mort_65_69 = rowMeans(across(ASDR_65:ASDR_69)),
         Mort_70_74 = rowMeans(across(ASDR_70:ASDR_74)),
         Mort_75_79 = rowMeans(across(ASDR_75:ASDR_79)),
         Mort_80_84 = rowMeans(across(ASDR_80:ASDR_84)),
         Mort_85_over = rowMeans(across(ASDR_85:ASDR_95))) |>  #using 95 for now since there are fewer 100 y/os and don't want equal weight
  select(!c(starts_with("ASDR"), sex)) |>
  pivot_longer(cols = c(Mort_0_1:Mort_85_over), names_to = "Age", names_prefix = "Mort_", values_to = "mort_rate") |>
  group_by(Sex, Age) |>
  mutate(mort_rate_average = mean(mort_rate)) |>
  distinct(Sex, Age, mort_rate_average) |>
  mutate(Age = str_replace(Age, "_", " to "),
         Age = str_c(Age," years"),
         Age = ifelse(Age == "85 to over years","85 years and over",Age),
         year = 2017) |>
  pivot_wider(id_cols = c("Sex","Age"), names_from = year, values_from = mort_rate_average) |>
  clean_names()

# https://www.census.gov/newsroom/press-kits/2023/population-projections.html
# Projected Mortality Rates by Age, Sex, Race, and Hispanic Origin for the United States: 2023 to 2100 (NP2023_A2)

census_mort_proj <- read_csv("Input/np2023_a2.csv")

#previous versions of this used SSA data that only had the data for each 5-year interval and took the average
#this version has data for each year so I just average the 5 years in each interval so the midpoints are no longer needed

census_mort_proj_processed <- census_mort_proj |>
  filter(GROUP == 0,
         YEAR <= 2050,
         NATIVITY == 0,
         SEX != 0) |>
  select(!c(NATIVITY,GROUP)) |>
  mutate(Sex = ifelse(SEX == 1, "Male", "Female"),
         Mort_0_1 = ASDR_0,
         Mort_1_4 = rowMeans(across(ASDR_1:ASDR_4)),
         Mort_5_9 = rowMeans(across(ASDR_5:ASDR_9)),
         Mort_10_14 = rowMeans(across(ASDR_10:ASDR_14)),
         Mort_15_19 = rowMeans(across(ASDR_15:ASDR_19)),
         Mort_20_24 = rowMeans(across(ASDR_20:ASDR_24)),
         Mort_25_29 = rowMeans(across(ASDR_25:ASDR_29)),
         Mort_30_34 = rowMeans(across(ASDR_30:ASDR_34)),
         Mort_35_39 = rowMeans(across(ASDR_35:ASDR_39)),
         Mort_40_44 = rowMeans(across(ASDR_40:ASDR_44)),
         Mort_45_49 = rowMeans(across(ASDR_45:ASDR_49)),
         Mort_50_54 = rowMeans(across(ASDR_50:ASDR_54)),
         Mort_55_59 = rowMeans(across(ASDR_55:ASDR_59)),
         Mort_60_64 = rowMeans(across(ASDR_60:ASDR_64)),
         Mort_65_69 = rowMeans(across(ASDR_65:ASDR_69)),
         Mort_70_74 = rowMeans(across(ASDR_70:ASDR_74)),
         Mort_75_79 = rowMeans(across(ASDR_75:ASDR_79)),
         Mort_80_84 = rowMeans(across(ASDR_80:ASDR_84)),
         Mort_85_over = rowMeans(across(ASDR_85:ASDR_95))) |>  #using 95 for now since there are fewer 100 y/os and don't want equal weight
  select(!c(starts_with("ASDR"), SEX)) |>
  pivot_longer(cols = c(Mort_0_1:Mort_85_over), names_to = "Age", names_prefix = "Mort_", values_to = "mort_rate") |>
  mutate(year_floor = floor(YEAR/5)*5) |>
  group_by(Sex,year_floor, Age) |>
  mutate(mort_rate_average = mean(mort_rate)) |>
  distinct(Sex, Age, year_floor, mort_rate_average) |>
  mutate(Age = str_replace(Age, "_", " to "),
         Age = str_c(Age," years"),
         Age = ifelse(Age == "85 to over years","85 years and over",Age)) |>
  pivot_wider(id_cols = c("Sex","Age"), names_from = year_floor, values_from = mort_rate_average) |>
  clean_names()

#need to convert from mortality rate to survival rate
#then to mirror the SSA data the raw number should be a ratio of present (2017) survival rates
census_data_combined <- census_17_proc |>
  left_join(census_mort_proj_processed) |>
  mutate(across(where(is.numeric), \(x) (1-x)/(1-x2017))) |>
  select(!x2017)

# Create final projections for each region  ------------------------------------

mort_proj <- life_table %>%
  select(region_cc, sex, age, Sx) %>%
  left_join(census_data_combined, by= c("sex", "age")) %>%
  mutate(across(c(5:11), .fns = ~.*Sx)) %>%
  #select(-Sx)
  rename("x2018" = Sx) #keep the calculated Sx

# Clean-up to values >= 1  ------------------------------------ This could use some adjustments to make it more dynamic

# qc_over_1 <- Mort_Proj %>% filter_at(vars(4:11), any_vars(. >= 1))

save(deaths, mort_proj, mort_pop, file="Output/Mort_Proj.Rdata")

