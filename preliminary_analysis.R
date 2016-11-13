# GLOBAL 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rethinking)
library(lme4)
library(lmerTest)

## read wide version of merged dataset with all variables 
base_df <- readRDS("base_df.rds")

## set annual population variable as numeric values 
base_df$Annual_pop <- as.numeric(base_df$Annual_pop)

## transpose immigration_type data from wide to long
base_df$immigration_type <- gsub('Inflows of asylum seekers by nationality', 'asylum_seekers', base_df$immigration_type)
base_df$immigration_type <- gsub('Inflows of foreign population by nationality', 'immigrants', base_df$immigration_type)
base_df <- spread(base_df, immigration_type, No.of.individuals)


## add summary statistics(mean, SD, correlation) comparing immigration rates by Country for each year
base_df2 <- base_df %>% group_by(Country) %>% summarise(cor_immigration_year = cor(x = Year, y =(immigrants/Annual_pop), use ='pairwise.complete.obs'),
                                                        cor_asylum_year = cor(x = Year, y = asylum_seekers/Annual_pop, use = 'pairwise.complete.obs'))

base_df3 <- base_df %>% group_by(Country, Year) %>% summarise(immigration_mean = mean(immigrants, na.rm=TRUE), 
                                                        SD = sd(immigrants, na.rm=TRUE),
                                                        immigration_sum = sum(immigrants, na.rm = TRUE),
                                                        Current_pop = mean(Annual_pop, na.rm = TRUE),
                                                        immigration_rate = (immigration_sum / Current_pop),
                                                        Assault_rate = mean(Assault_rate),
                                                        immigration_rate_normalized = (immigration_rate - .012595)/.01207,
                                                        genderwage_gap = mean(genderwage_gap))

## Used to filter out countries when needed
base_df4 <- base_df3 %>% filter(Country != 'Ireland' & Country != 'Turkey' & Country != 'Greece')


## plot mean immigration rate by Country+Year with r values 

ggplot(base_df3, aes(Year, immigration_rate )  ) + geom_point() + # + coord_flip() + facet_grid(Country ~.)
  facet_wrap(~Country)                                                                                                          


temp <- base_df3 %>% filter(Year == 2014)
linear <- lm(genderwage_gap ~ immigration_rate, data = base_df3)
quadratic <- lm(Assault_rate ~ immigration_rate + I(immigration_rate^2), data = base_df3)
cubic <- lm(Assault_rate ~ immigration_rate + I(immigration_rate^2) + I(immigration_rate^3), data = base_df3)


linear_lmer_int <- lmer(genderwage_gap ~ immigration_rate_normalized + (1 |Country), data = base_df3 )
linear_lmer_int_slope <- lmer(genderwage_gap ~ immigration_rate_normalized + Year + (1 + immigration_rate_normalized|Country), data = base_df3 )

temp <- base_df3 %>% group_by(Country) %>% summarise(mean_assault_rate = mean(Assault_rate, na.rm = TRUE), sd_assault_rate = sd(Assault_rate, na.rm = TRUE))


