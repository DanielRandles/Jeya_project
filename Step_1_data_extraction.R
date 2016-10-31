# GLOBAL 

library(plyr)
library(dplyr)
library(tidyr)

## open mined data 
migration<-read.csv('./data/migration.csv')
healthbli_2014<-read.csv('./data/healthBLI_2014.csv')
healthbli_2013<-read.csv('./data/healthBLI_2013.csv')
genderwage_gap<-read.csv('./data/genderw_gap.csv')
totalpop<-read.csv('./data/totalpop.csv')

### clean base migration dataset 

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
rm(health_2013_fixed)

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

### merge index dfs with base migration df 
Year <- rep(2013, 37)
health_2013_fixed2$Year <- Year
Year <- rep(2014, 37)
health_2014_fixed2$Year <- Year
rm(Year)
health_fixed_3 <- rbind(health_2013_fixed2, health_2014_fixed2)


base_df<-left_join(migration, totalpop)
base_df<-left_join(base_df, health_fixed_3, by = c('Country', 'Year'))
base_df<-left_join(base_df, genderwage_gap)

saveRDS(base_df, "base_df.rds")

