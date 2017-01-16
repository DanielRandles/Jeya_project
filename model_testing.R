base_df$immigration_rate_norm <- (base_df$immigration_rate - mean(base_df$immigration_rate, na.rm = T))/sd(base_df$immigration_rate, na.rm = T)
model <- lmer(genderwage_gap ~ 1 + immigration_rate_norm  + I(Year - 2000) + (1+ immigration_rate_norm + I(Year - 2000)|Country), base_df)


base_df <- base_df %>% group_by(Country) %>% mutate(genderwage_gap_fut_2 = lead(genderwage_gap, 2))
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
