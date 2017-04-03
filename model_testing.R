#GLOBAL
library(sjPlot)
library(lme4)

# Longitudinal models
## change in immigration rate over the years for each country  
mod_1 <- lmer(immigration_rate_norm ~ 1 + I(Year - 2000) + (1 + I(Year - 2000)|Country), base_df)

## Genderwage gap and immigration
gender_model <- lmer(genderwage_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)
lead_gender_mod <- lmer(lead2_gender_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)

## lme model testing for intentional homicides over time by country
crime_mod <- lmer(intentional_homicides ~ 1 + immigration_rate_norm + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df4)

# Cross-sectional analyses

model <- lm(genderwage_gap ~ immigration_rate_norm, base_df3)

model1 <- lm(Life_satisfaction~immigration_rate_norm + Employment_rate + Long_term_unemployment_rate, data=base_df3)
summary(model1)

model2 <- lm(Long_term_unemployment_rate~filt_Immigration_slope+filt_Immigration_intercept, data=base_df3)

model3 <- lm(oecd_immig_rate~Household_net_adjusted_disposable_income, data=base_df3)

model4 <- lm(Employees_working_long_hours~norm_oecd_rate+Household_net_adjusted_disposable_income, data=base_df3)

model5 <- lm(Life_satisfaction~norm_oecd_rate, data=base_df3)

# longitudinal model summary print 
sjt.lmer(crime_mod, gender_model, lead_gender_mod, mod_1,
         p.numeric = FALSE,
         show.header = TRUE, 
         string.est = "Estimate", 
         string.ci = "Confidence Interval", 
         string.p = "p-value",
         string.dv = "Response", 
         string.pred = "Coefficients",
         string.interc = "Intercept",
         depvar.labels = c("Intentional Homicides", "Gender Wage Gap", "Future Gender Wage Gap", "Immigration Rate"),
         pred.labels = c("Immigration Rate",
                         "Year"))
