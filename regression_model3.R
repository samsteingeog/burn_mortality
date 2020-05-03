

#knitr::opts_chunk$set(echo = TRUE)

library(tidyverse)
library(lubridate)
library(caTools)
library(caret)
library(MASS)
library(glmnet)
library(randomForest)
library(e1071)
library(rminer)


### Run these files first

# Run these files to get data frames
# Maybe we should turn them into .R files instead?
#rmarkdown::render('./ClimateStations_RAWS.Rmd') # gives us weather dataframe ("fires_weather")
#rmarkdown::render('./FireDBR_calcs.Rmd') # gives us dbr dataframe ("dbr_data")

fires_weather <- read_csv("./data/fires_weather.csv")
dbr_data <- read_csv("./data/dbr_data.csv")




## Join data sources

# read in average data csv file
# gives us elevation, slope, aspect averages for whole fire
average_data <- read_csv("data/avg_std.csv")

#import Yiyi's file with data from 10k sample points
sample_pts <- read_csv("data/sample_pts")

#clean up date values
sample_pts$ALARM_DATE <- ymd_hms(sample_pts$ALARM_DATE) #change from char to date
sample_pts$CONT_DATE <- ymd_hms(sample_pts$CONT_DATE)
sample_pts$fireduration <- interval(sample_pts$ALARM_DATE, sample_pts$CONT_DATE) %>% #coerce duration dates into time values
  as.duration()

#adding in summed biomass values (summed across fires)
sumDB <- read_csv("data/SumDB.txt") %>%
  dplyr::select(-OBJECTID) %>%
  rename("OBJECTID" = "FIREID")

sumLB <- read_csv("data/SumLB.txt") %>%
  dplyr::select(-OBJECTID) %>%
  rename("OBJECTID" = "FIREID")

meanTD <- read_csv("data/MeanTD.txt") %>%
  dplyr::select(-OBJECTID) %>%
  rename("OBJECTID" = "FIREID")

VegCover <- read_csv("data/VegTypeSum.txt") %>%
  dplyr::select(-OBJECTID) %>%
  rename("OBJECTID" = "FIREID", "conifer" = "VALUE_1", "shrub" = "VALUE_2", "herbaceous" = "VALUE_3", "barren_other" = "VALUE_4", 
         "urban" = "VALUE_5", "hardwood" = "VALUE_6", "water" = "VALUE_7", "agricultural" = "VALUE_8")



#r all data tables

all_data <- full_join(sample_pts, fires_weather, by = "OBJECTID") %>%
  dplyr::select(-ALARM_DATE.y, -CONT_DATE.y, -Shape_Leng.y, -YEAR_, -STATE) #drop extraneous variables

# drop geometry to avoid later computational limits
all_dat <- all_data %>%
  dplyr::select(-geometry)

#drop missing values for DEM and derived data
#drop values without info on dbr
all_dat <- all_dat %>%  
  filter(!(dem == -9999)) %>%
  filter(!(dbr == 0)) %>%
  filter(!(is.na(GIS_ACRES))) %>%
  filter(!(is.na(wind_max))) %>%
  mutate(fireduration = replace(fireduration, fireduration== as.duration(0), as.duration(43200))) #add in 12 hours of fireduration for fires that started and ended the same day

#change datetime data to numeric for later regression functions
all_dat$fireduration <- as.numeric(all_dat$fireduration) 
all_dat$DeadBio <- as.numeric(all_dat$DeadBio)


all_dat <- data.frame(all_dat) 


all_dbr <- all_dat$dbr #just dependent variables


all_ind <- all_dat %>% #just independent variables
  dplyr::select(slope, aspect, dem, LivingBio, DeadBio, fireduration, GIS_ACRES, 
                month_temp, week_temp, month_precip, week_precip, wind_speed, wind_max, month_humid, week_humid) 


all_df <- cbind(all_dbr, all_ind) %>% #had to create a data table with no NAs for later functions
  rename("dbr" = "all_dbr")

all_df <- na.omit(all_df)


#redo data frames now that observations with missing data in relevant fields has been removed

all_ind <- all_dat %>% #just independent variables
  dplyr::select(slope, aspect, dem, LivingBio, DeadBio, fireduration, GIS_ACRES, 
                month_temp, week_temp, month_precip, week_precip, wind_speed, wind_max, month_humid, week_humid) 

all_dbr <- all_dat$dbr #just dependent variables

#r ave data tables
# join all the datasets (averages, weather, dbr, biomass, landcover type)
ave_data <- full_join(average_data, fires_weather, by = "OBJECTID") %>%
  full_join(dbr_data, by = "OBJECTID") %>%
  dplyr::select(-YEAR_, -STATE) %>% #drop extraneous variables
  left_join(sumDB, by = "OBJECTID") %>%
  dplyr::select(-COUNT, -AREA) %>%
  rename("DeadBio_SUM" = "SUM") %>%
  left_join(sumLB, by = "OBJECTID") %>%
  dplyr::select(-COUNT, -AREA) %>%
  rename("LiveBio_SUM" = "SUM") %>%
  left_join(meanTD, by = "OBJECTID") %>%
  dplyr::select(-COUNT, -AREA) %>%
  rename("TreeDens" = "MEAN") %>%
  left_join(VegCover, by = "OBJECTID")

# drop geometry to void computation time issues later
ave_df <- ave_data %>%
  dplyr::select(-geometry)

#drop missing values for DEM and derived data
#drop values without info on dbr
ave_df <- ave_df %>%  
  filter(!is.na(DEM_MEAN)) %>%
  filter(!is.na(dbr_means)) %>%
  filter(!is.na(wind_speed)) %>%
  filter(!is.na(wind_max))

# add fire duration to dataframe
ave_df <- ave_df %>%
  mutate(fireduration = as.duration(interval(ALARM_DATE, CONT_DATE))) %>%
  # add in 12 hours of fireduration for fires that started and ended the same day
  mutate(fireduration = replace(fireduration, fireduration== as.duration(0), as.duration(43200))) %>%
  mutate(fireduration = as.numeric(fireduration)) %>% # change to numeric
  data.frame()


ave_df_og <- ave_df

ave_dbr <- ave_df$dbr_means #just dependent variables

ave_ind <- ave_df %>% #just independent variables
  dplyr::select(slope_MEAN, aspect_MEAN, DEM_MEAN, LiveBio_MEAN, DeadBio_MEAN, TreeDens, fireduration, GIS_ACRES, 
                month_temp, week_temp, month_precip, week_precip, wind_speed, wind_max,  month_humid, week_humid, 
                conifer, shrub, herbaceous, barren_other, urban, hardwood, water, agricultural)

ave_ind_alt <- ave_df %>% #trying out summed biomass values (instead of mean)
  dplyr::select(slope_MEAN, aspect_MEAN, DEM_MEAN, LiveBio_SUM, DeadBio_SUM, TreeDens, fireduration, GIS_ACRES, 
                month_temp, week_temp, month_precip, week_precip, wind_speed, wind_max, month_humid, week_humid, 
                conifer, shrub, herbaceous, barren_other, urban, hardwood, water, agricultural)



control <- trainControl(method = "repeatedcv", number = 10, selectionFunction = "best") #this controls the settings for the model selection (10-fold cross validation currently)


## Random Forest 

#setting up random forest
set.seed(1) #set seed for reproducability

all_forest = randomForest(x = all_ind, y = all_dbr, ntree = 300, mtry = 5) #documentation recs using 1/3 # of ind variables for mtry
#tried with various ntree and mtry values and got nearly identical results

#Extract predicted values
all_df$rf_dbr <- all_forest$predicted

## Support Vector Machine

#Set up Support Vector Machines
set.seed(1) #set seed for reproducibility 

## Method 1 - caret packaged
ave_svm1 <- train(ave_ind, ave_dbr, method = "svmLinear", trControl = control) #using same cv settings as before w/ stepwise

#predict new data
ave_df$svm_dbr1 <- predict(ave_svm1, newx = ave_df)


