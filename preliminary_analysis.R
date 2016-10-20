# GLOBAL 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

## read wide version of merged dataset with all variables 
base_df <- readRDS("base_df.rds")
base_df$Annual_pop <- as.numeric(base_df$Annual_pop)

## add summary statistics(mean, SD, correlation) comparing immigration rates by Country for each year
base_df2 <- base_df %>% group_by(Country) %>% summarise(cor_immigration_year = cor(Year, No.of.individuals))

base_df3 <- base_df %>% group_by(Country, Year) %>% summarise(immigration_mean = mean(No.of.individuals, na.rm=TRUE), 
                                                        SD = sd(No.of.individuals, na.rm=TRUE),
                                                        immigration_sum = sum(No.of.individuals, na.rm = TRUE),
                                                        Current_pop = mean(Annual_pop, na.rm = TRUE),
                                                        
                                                        immigration_rate = immigration_sum / Current_pop)

## Used to filter out countries when needed
base_df4 <- base_df3 %>% filter(Country != 'Ireland' & Country != 'Turkey' & Country != 'Greece')


## plot mean immigration rate by Country+Year with r values 

ggplot(base_df3, aes(Year, immigration_rate )  ) + geom_point() + # + coord_flip() + facet_grid(Country ~.)
  facet_wrap(~Country)                                                                                                          

<<<<<<< HEAD
## transpose immigration_type data from wide to long
immigration_type_split <- spread(base_df, immigration_type, No.of.individuals)
=======
>>>>>>> origin/master
