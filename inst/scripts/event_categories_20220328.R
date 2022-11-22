# Extract disasters categories from FEMA and GTD for Dr. Macgowan 
# Catalina Cañizares and Gabriel Odom 
# 03/25/2022


# In "src/inspect_fema_emdat_20211027.r" we where able to successfully join the
#  FEMA and EMDAT data sets, but we still have some cleaning and cheeking to do. 
#  Before that, Dr. Mark Macgowan is going to check the names used by FEMA and 
#  EMDAT to describe an event to lead our way into the cleaning phase
#  This script has the objective of extracting that list. 

######  Setup and Data  #######################################################

library(tidyverse)
library(lubridate)
library(usdata)
library(boot)

emdat_df <-
  readRDS(file = "data_processed/mass_casualty_emdat_20211027.RDS") %>%
  select(-event_id) %>%
  rename(emdatID = region_id)
fema_df <-
  readRDS(file = "data_processed/mass_casualty_fema_20211027.RDS") %>%
  rename(femaID = event_id)
allEventsMap_df <- readRDS(
  file = "data_processed/database_key_fema_emdat_20211027.RDS"
)

#### Tables ###################################################################

events_FEMA <- 
  fema_df %>% 
  mutate(EventUniqueId = substr(femaID, 1, 6)) %>%
  select(Source, EventUniqueId, incident_type) %>% 
  distinct() %>% 
  group_by(Source, incident_type) %>% 
  count() 


events_EMDAT <- 
  emdat_df %>% 
  mutate(EventUniqueId = substr(emdatID, 1, 9)) %>%
  select(Source, EventUniqueId, incident_type) %>% 
  distinct() %>% 
  group_by(Source, incident_type) %>% 
  count() 


events_categories <- 
  rbind(events_FEMA,events_EMDAT)

write_csv(events_categories, "data_processed/events_categories_20220328.csv")


