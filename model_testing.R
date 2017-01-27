base_df$immigration_rate_norm <- (base_df$immigration_rate - mean(base_df$immigration_rate, na.rm = T))/sd(base_df$immigration_rate, na.rm = T)
model <- lmer(genderwage_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)

model <- lmer(genderwage_gap_fut_2 ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)


temp <- base_df %>% filter(Year == 2014)

model <- lm(genderwage_gap ~ immigration_rate_norm, temp)


oecd_non_split_3$oecd_lead_immigration_rate_norm <- (oecd_non_split_3$lead_oecd - mean(oecd_non_split_3$lead_oecd, na.rm = T))/sd(oecd_non_split_3$lead_oecd, na.rm = T)
oecd_non_split_3$oecd_lead2_immigration_rate_norm <- (oecd_non_split_3$lead_oecd_2 - mean(oecd_non_split_3$lead_oecd_2, na.rm = T))/sd(oecd_non_split_3$lead_oecd_2, na.rm = T)


model <- lmer(genderwage_gap ~ 1 + oecd_lead_immigration_rate_norm  + I((Year - 2000)/14) + (1+ oecd_lead_immigration_rate_norm + I((Year - 2000)/14)|Country), oecd_non_split_3)


base_df <- base_df %>% group_by(Country) %>% mutate(genderwage_gap_fut_2 = lead(genderwage_gap, 2))
model <- lmer(genderwage_gap_fut_2 ~ 1 + immigration_rate_norm  + I((Year - 2000)/14) + (1+ immigration_rate_norm + I((Year - 2000)/14)|Country), base_df)


temp <- base_df %>% filter(Year == 2014)

model <- lm(genderwage_gap ~ immigration_rate_norm, temp)


## filter data for year 2014

### add immigration rate by total pop for each country

base_oecd <- mutate(base_oecd, oecd_immigration_rate=immigrants/Annual_pop)
base_oecd$immigration_rate_norm <- (base_oecd$oecd_immigration_rate - mean(base_oecd$oecd_immigration_rate, na.rm = T))/sd(base_oecd$oecd_immigration_rate, na.rm = T)
base_oecd <- base_oecd %>% filter(Year==2014)

model1 <- lm(Life_satisfaction~immigration_rate_norm + Employment_rate + Long_term_unemployment_rate, data=base_oecd)
summary(model1)

model2 <- lm(Long_term_unemployment_rate~Immigration_rate.y+Immigration_intercept.y, data=base_oecd2.short)

model3 <- lm(oecd_immigration_rate~Household_net_adjusted_disposable_income, data=base_oecd)

model4 <- lm(oecd_immigration_rate~Employees_working_very_long_hours, data=base_oecd)

model <- lmer(immigration_rate_norm.y ~ 1 + I(Year - 2000) + (1 + I(Year - 2000)|Country), base_oecd2)

### add immigration intercept and slope 
temp <- coef(model)$Country
temp <- temp %>% rename(Immigration_intercept = `(Intercept)`, Immigration_rate = `I(Year - 2000)`)
temp$Country <- row.names(temp)
base_oecd2 <- left_join(base_oecd, temp, by = 'Country')
rm(temp)

model <- lmer(genderwage_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)


### lme model testing for intentional homicides over time by country

base_oecd <- mutate(base_oecd, oecd_immigration_rate=immigrants/Annual_pop)
base_oecd$immigration_rate_norm <- (base_oecd$oecd_immigration_rate - mean(base_oecd$oecd_immigration_rate, na.rm = T))/sd(base_oecd$oecd_immigration_rate, na.rm = T)
model <- lmer(intentional_homicides ~ 1 + immigration_rate_norm + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_oecd)

base_oecd2 <- base_oecd %>% filter(Year<=2008)
model2 <- lm(Long_term_unemployment_rate~Immigration_rate+Immigration_intercept, data=base_oecd2)



### add annual pop of countries into new dataset
merge <- select(base_df, Country, Year, Annual_pop, genderwage_gap) %>% filter(Year==2002) %>% select(-Year)
merge <- distinct(merge, Country)
oecd_non_split_3 <- left_join(oecd_non_split, merge)


## add immigration rate by total pop for each country

oecd_non_split_3 <- mutate(oecd_non_split_3, oecd_immigration_rate=oecd_n/Annual_pop, non_oecd_rate=nonoecd_n/Annual_pop)

## add immigration rate by total pop for each country for +2 years 
oecd_non_split_3 <- mutate(oecd_non_split_3, lead_oecd = new_oecd_n/Annual_pop, lead_oecd_2 = new_oecd_n2/Annual_pop)

## normalize immigration rates
base_df$immigration_rate_norm <- (base_df$immigration_rate - mean(base_df$immigration_rate, na.rm = T))/sd(base_df$immigration_rate, na.rm = T)
base_df <- base_df %>% group_by(Country) %>% mutate(genderwage_gap_fut_2 = lead(genderwage_gap, 2))

## normalize immigration rates post-data split
oecd_non_split_3$oecd_lead_immigration_rate_norm <- (oecd_non_split_3$lead_oecd - mean(oecd_non_split_3$lead_oecd, na.rm = T))/sd(oecd_non_split_3$lead_oecd, na.rm = T)
oecd_non_split_3$oecd_lead2_immigration_rate_norm <- (oecd_non_split_3$lead_oecd_2 - mean(oecd_non_split_3$lead_oecd_2, na.rm = T))/sd(oecd_non_split_3$lead_oecd_2, na.rm = T)


## Longitudinal models

model <- lmer(genderwage_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)

model <- lmer(genderwage_gap_fut_2 ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)


temp <- base_df %>% filter(Year == 2014)

model <- lm(genderwage_gap ~ immigration_rate_norm, temp)



model <- lmer(genderwage_gap ~ 1 + oecd_lead_immigration_rate_norm  + I((Year - 2000)/14) + (1+ oecd_lead_immigration_rate_norm + I((Year - 2000)/14)|Country), oecd_non_split_3)

model <- lmer(genderwage_gap_fut_2 ~ 1 + immigration_rate_norm  + I((Year - 2000)/14) + (1+ immigration_rate_norm + I((Year - 2000)/14)|Country), base_df)


temp <- base_df %>% filter(Year == 2014)

model <- lm(genderwage_gap ~ immigration_rate_norm, temp)


