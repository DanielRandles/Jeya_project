### Preliminary and exploratory analyses

library(plyr)
library(dplyr)

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


