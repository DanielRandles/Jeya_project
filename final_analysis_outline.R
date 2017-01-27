# GLOBAL 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rethinking)
library(lme4)
library(lmerTest)
setwd("~/Jeya_project")

## read wide version of merged dataset with all variables 
base_df <- readRDS("base_df.rds")

## mod1: intercepts and slopes for each country by year(random) + norm immigration rate (fixed)
model <- lmer(immigration_rate_norm.y ~ 1 + I(Year - 2000) + (1 + I(Year - 2000)|Country), base_oecd2)

### add coefficients from mod1 to base df 
temp <- coef(model)$Country
temp <- temp %>% rename(Immigration_intercept = `(Intercept)`, Immigration_rate = `I(Year - 2000)`)
temp$Country <- row.names(temp)
base_df2 <- left_join(base_df, temp, by = 'Country')
rm(temp)

## filter rows by country
base_df3 <- select(base_df, -COU.y, -CO2, -Nationality, -lead2_gender_gap, -lead_oecd_immig, -lead_nonoecd_immig, -lead2_oecd_immig, -lead2_nonoecd_immig, -immigration_rate, -immigration_rate_norm)
base_df3 <- base_df3 %>% group_by(Country) %>% summarize(Annual_pop = mean(Annual_pop, na.rm=TRUE), 
                                                         Dwellings_without_basic_facilities = mean(Dwellings_without_basic_facilities, na.rm=TRUE),
                                                         Employees_working_long_hours = mean(Employees_working_very_long_hours, na.rm=TRUE),
                                                         Employment_rate = mean(Employment_rate, na.rm=TRUE),
                                                         Household_net_adjusted_disposable_income = mean(Household_net_adjusted_disposable_income, na.rm=TRUE),
                                                         Life_satisfaction = mean(Life_satisfaction, na.rm=TRUE),
                                                         Long_term_unemployment_rate = mean(Long_term_unemployment_rate, na.rm=TRUE),
                                                         genderwage_gap = mean(genderwage_gap, na.rm=TRUE),
                                                         intentional_homicides = mean(intentional_homicides, na.rm=TRUE),
                                                         population_density = mean(population_density, na.rm=TRUE),
                                                         urbanpop_frac = mean(urbanpop_frac, na.rm=TRUE),
                                                         asylum_seekers = mean(asylum_seekers, na.rm=TRUE),
                                                         immigrants = mean(immigrants, na.rm=TRUE),
                                                         oecd_immig = mean(oecd_immig, na.rm=TRUE),
                                                         nonoecd_immig = mean(nonoecd_immig, na.rm=TRUE),
                                                         Immigration_rate = immigrants/Annual_pop)

### add normalized immigration rate to df3
base_df3$immigration_rate_norm <- (base_df3$Immigration_rate - mean(base_df3$Immigration_rate, na.rm = T))/sd(base_df3$Immigration_rate, na.rm = T)

## filter rows by year for each country
base_df4 <- select(base_df, -COU.y, -CO2, -Nationality, -lead2_gender_gap, -lead_oecd_immig, -lead_nonoecd_immig, -lead2_oecd_immig, -lead2_nonoecd_immig, -immigration_rate, -immigration_rate_norm)
base_df4 <- base_df4 %>% group_by(Country, Year) %>% summarize(Annual_pop = mean(Annual_pop, na.rm=TRUE), 
                                                         Dwellings_without_basic_facilities = mean(Dwellings_without_basic_facilities, na.rm=TRUE),
                                                         Employees_working_long_hours = mean(Employees_working_very_long_hours, na.rm=TRUE),
                                                         Employment_rate = mean(Employment_rate, na.rm=TRUE),
                                                         Household_net_adjusted_disposable_income = mean(Household_net_adjusted_disposable_income, na.rm=TRUE),
                                                         Life_satisfaction = mean(Life_satisfaction, na.rm=TRUE),
                                                         Long_term_unemployment_rate = mean(Long_term_unemployment_rate, na.rm=TRUE),
                                                         genderwage_gap = mean(genderwage_gap, na.rm=TRUE),
                                                         intentional_homicides = mean(intentional_homicides, na.rm=TRUE),
                                                         population_density = mean(population_density, na.rm=TRUE),
                                                         urbanpop_frac = mean(urbanpop_frac, na.rm=TRUE),
                                                         asylum_seekers = mean(asylum_seekers, na.rm=TRUE),
                                                         immigrants = mean(immigrants, na.rm=TRUE),
                                                         oecd_immig = mean(oecd_immig, na.rm=TRUE),
                                                         nonoecd_immig = mean(nonoecd_immig, na.rm=TRUE),
                                                         Immigration_rate = immigrants/Annual_pop)

### add normalized immigration rate to df4
base_df4$immigration_rate_norm <- (base_df4$Immigration_rate - mean(base_df4$Immigration_rate, na.rm = T))/sd(base_df4$Immigration_rate, na.rm = T)
