# GLOBAL 
library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)

## read wide version of merged dataset with all variables 
base_df <- readRDS("base_df.rds")

## add summary statistics(mean, SD, correlation) comparing immigration rates by Country for each year
base_df2 <- base_df %>% group_by(Country) %>% summarise(cor_immigration_year = cor(Year, No.of.individuals))

base_df3 <- base_df %>% group_by(Country, Year) %>% summarise(immigration_mean = mean(No.of.individuals, na.rm=TRUE), 
                                                        SD = sd(No.of.individuals, na.rm=TRUE))


## plot mean immigration rate by Country+Year with r values 

ggplot(base_df3, aes(Year, immigration_mean )  ) + geom_point() + # + coord_flip() + facet_grid(Country ~.)
  facet_wrap(~Country)                                                                                                          
