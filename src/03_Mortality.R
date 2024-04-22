# CMAP | Noel Peterson, Mary Weber, Alex Bahls | 7/12/2021

#need to update the input dataset

library(dplyr)
library(tidyverse)
library(tidycensus)
library(readxl)

#looked into using CDC wonder data but it had too many supressed counts to use reliably
#i.e. in 2019 external WI there are 44/138 county/age/sex combinations with suppressed counts

# Parameters ---------------------------------------------------------

load("Output/POP_PEP.Rdata")
load("Output/Age_0_4_Freq.Rdata")
AGE_0_4_FREQ$Population <- as.numeric(AGE_0_4_FREQ$Population)

MORT_YEARS <- c(2014:2018)
DEATHS_XLSX <- "Input/CMAPMortality1990-2019.xlsx"

county_fips_index <- as_tibble(cbind(names(cmapgeo::county_fips_codes$cmap),
                                     cmapgeo::county_fips_codes$cmap)) |>
  rename(county_name = 1, county_code = 2)

# Create mortality age groups ---------------------------------------------------------

MORT_POP <- tibble()

for (YEAR in MORT_YEARS) {
   MORT_POP <- bind_rows(MORT_POP, POP[[as.character(YEAR)]])
}

MORT_POP <- MORT_POP |> mutate(Region_cc = case_when(
  Region == "CMAP Region" ~ GEOID,
  T ~ Region
))

# Load deaths data
Deaths <- read_excel(DEATHS_XLSX) %>%
  filter(Year %in% MORT_YEARS) %>%
  mutate(GEOID = as.character(GEOID),
         Age = case_when(Age %in% c("85 to 89 years", "90 to 94 years", "95 years and over") ~ "85 years and over",
                         TRUE ~ Age),
         Region_cc = case_when(
           Region == "CMAP Region" ~ GEOID,
           T ~ Region
         )) %>%
  group_by(GEOID, Sex, Age, Year, Region_cc) %>%
  summarize(Mortality = sum(Mortality), .groups = "drop")

# Join pop to deaths
MORT_DATA <- MORT_POP %>%
  mutate(Age = ifelse(Age == "85 years and older","85 years and over", Age)) %>%
  full_join(Deaths, by=c('GEOID', 'Age', 'Sex', 'Year', 'Region_cc')) %>%
  group_by(Age, Sex, Region_cc) %>%
  summarise(Population = sum(Population),
            Mortality = sum(Mortality),
            .groups = "drop") %>%
  arrange(Region_cc, desc(Sex))

# Use PUMA proportion estimates for 0-1 and 1-4 age group -------------------------------------

AGE_0_4_PROP <- AGE_0_4_FREQ %>%
  mutate(Age = case_when(AgeGroup == 'Less than 1 year' ~ '0 to 1 years',
                         TRUE ~ AgeGroup)) %>%
  left_join(county_fips_index, by = c("Region_cc" = "county_name")) |>
  mutate(Region_cc = case_when(
    str_detect(Region_cc, "External") ~ Region_cc,
    T ~ county_code
  )) |>
  select(-c(AgeGroup,Population, county_code))

MORT_DATA_with_1_4 <- MORT_DATA %>%
  left_join(AGE_0_4_PROP, by=c('Age','Region_cc', 'Sex')) |>
  mutate(Population = case_when(Age == '0 to 1 years' ~ lead(Population)*Age_0_4_Share,
                                Age == '1 to 4 years' ~ lag(Population)*Age_0_4_Share,
                                TRUE ~ Population)) %>%
  select(!Age_0_4_Share) %>%
  filter(Age != '0 to 4 years')

# Life Tables Calculations ---------------------------------------------------------

#Life tables are a common means of constructing survival rates -- see forecast book p. 54

LT <- tibble(Age = unique(Deaths$Age)) %>%
  mutate(x = as.numeric(str_split_fixed(Age, " ", 2)[,1])) %>%
  arrange(x) %>%
  add_column(Ax = c(0.1,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5,0.5))

#i think Ax is an assumption about when in the internal people will die;
#we assume the average person that dies in the five year interval dies halfway through
#but for babies, much more likely to die earlier in interval;
#we expect more deaths right after birth than 9 months after birth


#see page 54 of forecast book for variable definitions
LifeTable <- MORT_DATA_with_1_4 %>%
  left_join(LT, by="Age") %>%
  select(Region_cc, Sex, Age, Mortality, Population, x, Ax) %>%
  arrange(Region_cc, desc(Sex), x) %>%
  group_by(Region_cc, Sex) %>%
  mutate(Mx = (Mortality/as.numeric(Population)), #Mortality rate (using mid-year population)
    n = case_when(Age == '0 to 1 years' ~ 1, # n -- number of years in interval
                  Age == '1 to 4 years' ~ 4,
                  Age == '85 years and over' ~ 2/Mx, #I have thought about this calc a lot and don't have a perfect
                              #conceptual understanding of it; however the n value for 85+ isn't used in subsequent code
                  TRUE ~ 5),
    Qx = ifelse(Age == '85 years and over', 1,  # 85+ should always be 1
                (n*Mx/(1+n*(1-Ax)*Mx))), #proportion that will die during that age internal; formula to adjust for mid-year population denom
    Px = (1-Qx), #probabilty of living through age period
    Ix = head(accumulate(Px, `*`, .init=100000), -1), # 0-1 should always be 100000 -- number of people who reach age interval; 100000*p
    Dx = (ifelse(Age == '85 years and over', Ix, Ix -lead(Ix))), #number of people that will die during internal
    Lx = (ifelse(Age == '85 years and over', Ix/Mx, n*(lead(Ix)+(Ax*Dx)))), # of person years lived
    temp = ifelse(Age == '85 years and over', Lx, 0),
    Tx = (ifelse(Age == '85 years and Over', Lx, accumulate(Lx, `+`, .dir = "backward"))), #cumulative number of person years lived after internval
    Ex = (Tx/Ix), #life expectancy, number of people over number of future person years
    Sx = case_when(Age == '0 to 1 years' ~ Lx/Ix,
                   Age == '1 to 4 years' ~ Lx/(lag(Lx)*4),
                   Age == '5 to 9 years' ~ Lx/(lag(Lx) + lag(Lx, n = 2)),
                   Age == '85 years and over' ~ Lx/(Lx +lag(Lx)),
                   TRUE ~ Lx/lag(Lx)) #survival rate in life years
  ) %>%
  select(-temp) %>%
  relocate(c(x, n, Ax), .before= Mortality)%>%
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
  pivot_wider(id_cols = c("Sex","Age"), names_from = year, values_from = mort_rate_average)

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
  pivot_wider(id_cols = c("Sex","Age"), names_from = year_floor, values_from = mort_rate_average)

#need to convert from mortality rate to survival rate
#then to mirror the SSA data the raw number should be a ratio of present (2017) survival rates
census_data_combined <- census_17_proc |>
  left_join(census_mort_proj_processed) |>
  mutate(across(where(is.numeric), \(x) (1-x)/(1-`2017`))) |>
  select(!`2017`)

# Create final projections for each region  ------------------------------------

Mort_Proj <- LifeTable %>%
  select(Region_cc, Sex, Age, Sx) %>%
  left_join(census_data_combined, by= c("Sex", "Age")) %>%
  mutate(across(c(5:11), .fns = ~.*Sx)) %>%
  #select(-Sx)
  rename("2018" = Sx) #keep the calculated Sx

# Clean-up to values >= 1  ------------------------------------ This could use some adjustments to make it more dynamic

# qc_over_1 <- Mort_Proj %>% filter_at(vars(4:11), any_vars(. >= 1))

save(Deaths, Mort_Proj, MORT_POP, file="Output/Mort_Proj.Rdata")

