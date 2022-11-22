# Create Table of Type of Disasters
# Catalina Cañizares and Gabriel Odom
# 05/27/2022

# Now that we have a unique key for all events (by state, year, and month), we
#  will create a table of the type of disasters based on the classification 
#  of the Hazard Definition and Classification Review Technical Report
#  published by the UN Office for Disaster Risk Reduction (2020)
#  Annex 6 page 72. 

library(tidyDisasters)
library(readxl)
library(tidyverse)

#### Loading data sets ####

# The following table was downloaded from the Hazard Definition and 
#  Classification Review Technical Report  published by the UN Office for
#  Disaster Risk Reduction (2020) Annex 6 page 72, to match the type of disaster
#  reported by FEMA, GTD AND EMDAT and the suggested classification. The process
#  was done by converting the pdf into a word document and then copying and 
#  pasting to an xlsx file. 

hazard_report_df <- 
  read_xlsx("inst/extdata/20220822_hazard_report.xlsx")  %>%
  rename(hazard_type = `Hazard type`, hazard_cluster = `Hazard cluster`) 
# 300 x 2 

usethis::use_data(hazard_report_df, overwrite = TRUE)

hazard_report <- 
  hazard_report_df %>% 
  filter(hazard_type != "TECHNOLOGICAL" | hazard_cluster != "Flood" )
# NOTE: some hazard clusters are classified in two or more
#  types of Hazard Types (Aquaculture, CBRNE, Food safety and Flood). 
#  In our data set we only have floods, for this first version we 
#  are going to assume that all flood are meteorological and hydrological 
#  for the future we will check which floods are technological.

# Called the clean EMDAT, FEMA and keys data to create the data sets. 
clean_emdat <- readRDS(file = "inst/extdata/clean_emdat.RDS")
# 3101152 x 9

clean_fema <- readRDS(file = "inst/extdata/clean_fema.RDS")
# 32,938,843 x 8

data("allKeys_df")
# 498,188 x 4

# In scripts create_EMDAT_disastTypes_20220822 and create_FEMA_20220822 
#  we created tables that match the disaster type reported by each FEMA and 
#  emdat to the UN report according to Dr. Macgowan's and Catalina's opinion 
#  this will be used to match the types of disasters through the data sets. 

fema_hazard_cluster <- readRDS("inst/extdata/fema_hazard_cluster.RDS")
emdat_hazard_cluster <- readRDS("inst/extdata/emdat_hazard_cluster.RDS")


#### FEMA ##### 

# Created a unique fema events data set to be able to match the disaster types 
#  to each type of event reported. 

unique_fema_events_df <- 
  clean_fema %>%
  select(Source, femaID = event_id, incident_type) %>% 
  mutate(incident_type = str_remove(incident_type, pattern = "\\("), 
         incident_type = str_remove(incident_type, pattern = "\\)")
  ) %>% 
  distinct()
# 258634 x 3

# Joined the table created with the matches and all the events reported in FEMA

fema_eventTypes_df <- 
  unique_fema_events_df %>% 
  left_join(fema_hazard_cluster, by = "incident_type") %>% 
  select(Source = Source.x, femaID, incident_type, hazard_cluster)
# 258634 x 3

# Now that we matched the type of events with the hazard cluster, which is the
#  sub type of event suggested by the UN, we are going to join to annex 6 to 
#  get the hazard type. 

fema_hazardType1 <- 
  fema_eventTypes_df%>% 
  inner_join(hazard_report, by = "hazard_cluster") %>% 
  select(-`Specific hazard`) %>% 
  distinct()
# 258338 x 5

# There are 296 less cases because FEMA has a category called OTHER. 
#  Dr. Macgowan and Catalina agreed that this category should be maintained as 
#  other, however the UN report does not hold that category, therefore the inner
#  join kicks those cases. To add the OTHER category I decided to do an antijoin
#  and then bind both data sets. 

fema_hazardType2 <- 
  fema_eventTypes_df%>% 
  anti_join(hazard_report, by = "hazard_cluster") %>% 
  mutate(hazard_type = hazard_cluster) %>% 
  distinct()
# 296 x 3

fema_hazardType_df <- 
  bind_rows(fema_hazardType1, fema_hazardType2)
# 258634 x 3

#### EMDAT ####

unique_emdat_events_df <- 
  clean_emdat %>% 
  mutate(
    incident_type = str_remove(incident_type, pattern = "\\("), 
    incident_type = str_remove(incident_type, pattern = "\\)")
  ) %>% 
  select(Source, emdatID = region_id, incident_type) %>% 
  distinct()
# 4424 x 3

# Joined the table created with the matches and all the events reported in EMDAT

emdat_eventTypes_df <- 
  unique_emdat_events_df %>% 
  left_join(emdat_hazard_cluster, by = "incident_type") %>% 
  select(Source = Source.x, emdatID, incident_type, hazard_cluster)
# 4424 x 3

# Now that we matched the type of events with the hazard cluster, which is the
#  sub type of event suggested by the UN, we are going to join to annex 6 to 
#  get the hazard type. 

emdat_hazardType_df <- 
  emdat_eventTypes_df%>% 
  inner_join(hazard_report, by = "hazard_cluster") %>% 
  select(-`Specific hazard`) %>% 
  distinct()
# 4424 x 5


## Complete table with event types for each source ####

incidentType_df <-
  bind_rows(fema_hazardType_df, emdat_hazardType_df) %>% 
  select(Source, incident_type, hazard_cluster)
# 263058 x 5

#### Join to key ####

disastTypes_1 <- 
  allKeys_df %>% 
  inner_join(fema_hazardType_df, by = "femaID") %>% 
  select(eventKey, incident_type, hazard_cluster, hazard_type)

disastTypes_2 <- 
  allKeys_df %>% 
  inner_join(emdat_hazardType_df, by ="emdatID") %>% 
  select(eventKey, incident_type, hazard_cluster, hazard_type)

disastTypes_df <- 
  bind_rows(disastTypes_1, disastTypes_2) %>% 
  distinct()
# 10412 x 4

usethis::use_data(disastTypes_df, overwrite = TRUE)
disastTypes_df %>% count(hazard_type
                         )
