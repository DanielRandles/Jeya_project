# GLOBAL 
setwd("~//Documents//PSYC90_16_17")
library(plyr)
library(dplyr)
library(tidyr)

## open mined data 
migration<-read.csv('.\\data\\migration.csv')
healthbli_2014<-read.csv('.\\data\\healthBLI_2014.csv')
healthbli_2013<-read.csv('.\\data\\healthBLI_2013.csv')
genderwage_gap<-read.csv('.\\data\\genderw_gap.csv')
totalpop<-read.csv('.\\data\\totalpop.csv')

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

## clean health bli_2014 df

### add year column 
healthbli_2014["Year"]<- 2014

### rename column headings 
colnames(healthbli_2014)[1]<-"COU"
colnames(healthbli_2014)[3]<-"better_life_indicator"
colnames(healthbli_2014)[5]<-"BLI_value"

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

base_df<-left_join(migration, totalpop)
base_df<-left_join(base_df, healthbli_2013)
base_df<-left_join(base_df, healthbli_2014)
base_df<-left_join(base_df, genderwage_gap)

saveRDS(base_df, "base_df.rds")
=======
  # Extracts data from primary sources (all .csv) and merge them into a df
  
  ## Loading packages
  
  library(plyr)
library(dplyr)




## Loading data

mig_pop <-read.csv(file=".//original_data//mig_pop2.csv")
updated <- read.csv(file ='.//original_data//updated.csv')
saveRDS(updated, './/transformed_data//mig_pop_v2.rds')
updated <- readRDS(file = './/transformed_data//mig_pop_v2.rds')
mig_pop <- read.csv(file="mig_pop.csv")
View(mig_pop)
mig_pop$X<-NULL
health2014<-read.csv(file="health_OECD")
health2014<-read.csv(file="health_OECD")
health<-read.csv(file="health_OECD")
health<-read.csv(file="health_OECD.csv")
View(health)
health<-health[-1,]
health[1]<-"Country"
health<-read.csv(file="health_OECD.csv")
health<-health[-1,]
colnames(health)[1]<-"Country"
write.csv(health, file="health.csv")
health<-read.csv(file="health.csv")
View(health)
merge(health, mig.pop, by=c("Country", "Year"))
mig_pop2<-merge(health, mig_pop, by=c("Country", "Year"))
View(mig_pop2)
mig_pop3<-rbind(mig_pop, health)
mig_pop3<-rbind.fill(mig_pop, health)
plyr

mig_pop3<-rbind.fill(mig_pop, health)
View(mig_pop3)
mig_pop3<-rbind(mig_pop, health)
mig_pop2<-merge(health, mig_pop, by=c("Country", "Year"),all=TRUE)
View(mig_pop2)
rm(mig_pop3)
rm(health)
genderwage<-read_csv(file="genderw_gap.csv")
genderwage<-read.csv(file="genderw_gap.csv")
View(genderwage)
colnames(genderwage)[3]<-"Gender_Wage_gap"
colnames(genderwage)[2]<-"Year"
mig_pop<-merge(genderwage,migpop2, by=c("Country", "Year"),all=TRUE)
mig_pop<-merge(genderwage,mig_pop2, by=c("Country", "Year"),all=TRUE)
View(mig_pop)
write.csv(mig_pop,file="updated.csv")
View(genderwage)
View(mig_pop)

setwd("C:/Users/Jeyasakthi/Downloads/Psych")
mig_pop <-read.csv(file="mig_pop2.csv")

#load migration stats data by country
mig_pop <- read.csv(file="mig_pop.csv")
mig_pop$X<-NULL

#load self-reported health for 2014
health<-read.csv(file="health_OECD.csv")

# dplyr to combine data frames 
library(dplyr)
current_df<-full_join(mig_pop2, genderwage)
>>>>>>> origin/master
