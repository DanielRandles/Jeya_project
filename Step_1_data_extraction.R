# GLOBAL 

library(plyr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(lme4)
library(lmerTest)
setwd("~/Jeya_project")

## open mined data 
migration<-read.csv('./data/migration.csv')
healthbli_2014<-read.csv('./data/healthBLI_2014.csv')
healthbli_2013<-read.csv('./data/healthBLI_2013.csv')
genderwage_gap<-read.csv('./data/genderw_gap.csv')
totalpop<-read.csv('./data/totalpop.csv')
pop_density <- read.csv('./data/Pop_density.csv')
urbanpop_frac <- read.csv('./data/Urbanpop_fraction.csv')
homicide <- read.csv('./data/homicide.csv')

## clean base migration dataset 
### remove unwanted columns 
migration$VAR<-NULL
migration$GEN<-NULL
migration$Gender<-NULL
migration$YEA<-NULL
migration$Flags<-NULL
migration$Flag.Codes<-NULL

#### rename column headings
colnames(migration)[2]<- "Nationality"
colnames(migration)[7]<-"No.of.individuals"
colnames(migration)[3]<-"immigration_type"

### rearrange columns
migration<- migration[c("COU","Country","Year","immigration_type","CO2","Nationality","No.of.individuals")]

### subset required immigration variables 
migration<- filter(migration, immigration_type %in% c("Inflows of foreign population by nationality", "Inflows of asylum seekers by nationality"))

## clean health bli_2013 df
### rename column headings 
colnames(healthbli_2013)[1]<-"COU"
colnames(healthbli_2013)[3]<-"better_life_indicator"
colnames(healthbli_2013)[4]<-"Year"
colnames(healthbli_2013)[5]<-"BLI_value"

### remove triplets of identical rows 
health_2013_fixed<- select(healthbli_2013,COU, Country, better_life_indicator, BLI_value)
health_2013_fixed2 <- health_2013_fixed %>% group_by(COU, better_life_indicator) %>% filter(row_number(BLI_value) ==1) %>% ungroup()

### transpose from long to wide 
health_2013_fixed2<-spread(health_2013_fixed2, better_life_indicator, BLI_value)

## clean health bli_2014 df
### add year column 
healthbli_2014["Year"]<- 2014

### rename column headings 
colnames(healthbli_2014)[1]<-"COU"
colnames(healthbli_2014)[3]<-"better_life_indicator"
colnames(healthbli_2014)[5]<-"BLI_value"

### remove triplets of identical rows 
health_2014_fixed<- select(healthbli_2014,COU, Country, better_life_indicator, BLI_value)
health_2014_fixed2 <- health_2014_fixed %>% group_by(COU, better_life_indicator) %>% filter(row_number(BLI_value) ==1) %>% ungroup()

### transpose from long to wide 
health_2014_fixed2<-spread(health_2014_fixed2, better_life_indicator, BLI_value)

## clean population stats df
### remove first row of empty cells
totalpop<-totalpop[-1,]

### rename column headings
colnames(totalpop)[1]<-"Country"

### transpose df from wide to long 
totalpop<-gather(totalpop, Year, Annual_pop, -Country)

### remove non-OECD countries from totalpop df
totalpop<- totalpop[!totalpop$Country %in% c("G7","OECD - Total","World","Brazil","China (People's Republic of)", "Colombia", "India","Indonesia","Russia", "South Africa"), ] 

### fix values of Year column       
Year2<- c(X2000=2000,X2001=2001,X2002=2002,X2003=2003,X2004=2004,X2005=2005,X2006=2006,X2007=2007,X2008=2008,X2009=2009,X2010=2010,X2011=2011,X2012=2012,X2013=2013,X2014=2014)
totalpop$Year <- Year2[totalpop$Year]

## clean gender wage df 
### rename column headings
colnames(genderwage_gap)[2]<-"Year"
colnames(genderwage_gap)[3]<-"genderwage_gap"

## clean intentional homicides df 
### remove unwanted columns 
homicide$Country.Code <- NULL
homicide$Indicator.Name <- NULL
homicide$Indicator.Code <- NULL
homicide$X2015 <- NULL
homicide$X2016 <- NULL
homicide <- select(homicide, Country.Name, starts_with("X20"))

### rename column headings 
colnames(homicide)[1] <- "Country"

### transpose from wide to long 
homicide <- gather(homicide, Year, intentional_homicides, -Country)

### fix Year column values 
Year2<- c(X2000=2000,X2001=2001,X2002=2002,X2003=2003,X2004=2004,X2005=2005,X2006=2006,X2007=2007,X2008=2008,X2009=2009,X2010=2010,X2011=2011,X2012=2012,X2013=2013,X2014=2014)
homicide$Year <- Year2[homicide$Year]

## clean pop_density df 
### remove unwanted columns 
pop_density$Country.Code <- NULL
pop_density$Indicator.Name <- NULL
pop_density$Indicator.Code <- NULL
pop_density$X2016 <- NULL
pop_density <- select(pop_density, Country.Name, starts_with("X20"))

### rename column headings 
colnames(pop_density)[1] <- "Country"

### transpose from wide to long 
pop_density <- gather(pop_density, Year, population_density, -Country)

### fix Year column values 
Year2<- c(X2000=2000,X2001=2001,X2002=2002,X2003=2003,X2004=2004,X2005=2005,X2006=2006,X2007=2007,X2008=2008,X2009=2009,X2010=2010,X2011=2011,X2012=2012,X2013=2013,X2014=2014)
pop_density$Year <- Year2[pop_density$Year]

## clean urban population df 
### remove unwanted columns 
urbanpop_frac$Country.Code <- NULL
urbanpop_frac$Indicator.Name <- NULL
urbanpop_frac$Indicator.Code <- NULL
urbanpop_frac$X2016 <- NULL
urbanpop_frac <- select(urbanpop_frac, Country.Name, starts_with("X20"))

### rename column headings 
colnames(urbanpop_frac)[1] <- "Country"

### transpose from wide to long 
urbanpop_frac <- gather(urbanpop_frac, Year, urbanpop_frac, -Country)

### fix Year column values 
Year2<- c(X2000=2000,X2001=2001,X2002=2002,X2003=2003,X2004=2004,X2005=2005,X2006=2006,X2007=2007,X2008=2008,X2009=2009,X2010=2010,X2011=2011,X2012=2012,X2013=2013,X2014=2014)
urbanpop_frac$Year <- Year2[urbanpop_frac$Year]

## fix OECD 2013 + 2014 dfs 
### add Year column
Year <- rep(2013, 37)
health_2013_fixed2$Year <- Year
Year <- rep(2014, 37)
health_2014_fixed2$Year <- Year
rm(Year)

### combine OECD dfs
health_fixed_3 <- rbind(health_2013_fixed2, health_2014_fixed2)

## merge index dfs with base migration df 
base_df<-left_join(migration, totalpop)
base_df<-left_join(base_df, health_fixed_3, by = c('Country', 'Year'))
base_df<-left_join(base_df, genderwage_gap)
base_df<-left_join(base_df, homicide)
base_df<-left_join(base_df, pop_density)
base_df<-left_join(base_df, urbanpop_frac)

### rename column headings for base df-repeated for multiple spaces between certain columns
names(base_df) <- sub(" ", "_", names(base_df))
names(base_df) <- sub(" ", "_", names(base_df))
names(base_df) <- sub(" ", "_", names(base_df))
names(base_df) <- sub(" ", "_", names(base_df))
names(base_df) <- sub(" ", "_", names(base_df))
names(base_df) <- sub("-", "_", names(base_df))

## remove all redundant objects
rm(genderwage_gap, health_2013_fixed, health_2013_fixed2, health_2014_fixed, health_2014_fixed2, health_fixed_3, healthbli_2013, healthbli_2014, homicide, migration, pop_density, totalpop, urbanpop_frac, Year2)

## set annual population variable as numeric values 
base_df$Annual_pop <- as.numeric(base_df$Annual_pop)

## transpose immigration_type data from wide to long
base_df$immigration_type <- gsub('Inflows of asylum seekers by nationality', 'asylum_seekers', base_df$immigration_type)
base_df$immigration_type <- gsub('Inflows of foreign population by nationality', 'immigrants', base_df$immigration_type)
base_df <- spread(base_df, immigration_type, No.of.individuals)

## addition of useful variables for model testing
### separate OECD and non-OECD nationalities of immigrants
Countries <- unique(base_df$Country)
base_oecd <- base_df %>% group_by(Nationality) %>% filter(Nationality %in% Countries)
base_nonoecd <- base_df %>% group_by(Nationality) %>% filter(!Nationality %in% Countries)

### n of immigrants for each country by year for oecd + non_oecd
base_oecd1 <- base_oecd %>% group_by(Country, Year) %>% summarize(oecd_immig = sum(immigrants, na.rm=TRUE))
base_nonoecd1 <- base_nonoecd %>% group_by(Country, Year) %>% summarize(nonoecd_immig = sum(immigrants, na.rm=TRUE))

### lead n of immigrants for 2 years
base_oecd1 <- base_oecd1 %>% mutate(lead_oecd_immig = lead(oecd_immig))
base_oecd1 <- base_oecd1 %>% mutate(lead2_oecd_immig = lead(lead_oecd_immig))
base_nonoecd1 <- base_nonoecd1 %>% mutate(lead_nonoecd_immig = lead(nonoecd_immig))
base_nonoecd1 <- base_nonoecd1 %>% mutate(lead2_nonoecd_immig = lead(lead_nonoecd_immig))

### lead gender wage +2 years
base_df <- base_df %>% group_by(Country) %>% mutate(lead2_gender_gap = lead(genderwage_gap, 2))

### merge lead values from the oecd + non-oecd split dfs
oecd_non_split <- left_join(base_oecd1, base_nonoecd1)

### merge lead values from split data to base df
base_df <- left_join(base_df, oecd_non_split)

### calculate immigration rate and normalized immigration rate
base_df <- mutate(base_df, immigration_rate=immigrants/Annual_pop)
base_df$immigration_rate_norm <- (base_df$immigration_rate - mean(base_df$immigration_rate, na.rm = T))/sd(base_df$immigration_rate, na.rm = T)

## remove objects
rm(base_nonoecd, base_nonoecd1, base_oecd, base_oecd1, oecd_non_split)

## save final df as RDS
saveRDS(base_df, "base_df.rds")
