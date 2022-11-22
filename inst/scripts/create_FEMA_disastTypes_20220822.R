# Create Table of Type of Disasters for FEMA
# Catalina Cañizares, Mark Macgowan and Gabriel Odom
# 08/22/2022

# We will create a table of the type of disasters  based on the classification 
#  of the Hazard Definition and Classification Review Technical Report
#  published by the UN Office for Disaster Risk Reduction (2020)
# This script will create a table matching the type of disasters reported by 
#  FEMA and Mark Macgowan and Catalina Canizares opinion regarding the hazard
#  cluster it should belong to

library(tidyDisasters)
library(readxl)
library(tidyverse)

# Loading the data we need to o the matches

clean_fema <- readRDS(
  file = "inst/extdata/clean_fema.RDS"
)
# 32,938,843 x 8

hazard_report <- 
  read_xlsx("inst/extdata/20220822_hazard_report.xlsx")  %>%
  rename(hazard_type = `Hazard type`, hazard_cluster = `Hazard cluster`) 
# 303 x 2 

# We created the list of unique type of events in FEMA to categorize them
#  according to the Hazard Definition and Classification Review Technical Report
#  in the hazard cluster. 

unique_fema_events_df <- 
  clean_fema %>%
  select(Source, femaID = event_id, incident_type) %>% 
  distinct()
# 258634
    
rename_hazard_cluster_fema <- 
  c("Armed Assault" = "Conflict", 
    "Chemical" = "Other chemical hazards and toxins", 
    "Dam/Levee Break" = "Construction/ Structural failure", 
    "Earthquake" = "Seismogenic (earthquakes)", 
    "Fire" = "Environmental degradation (Forestry)", 
    "Flood" = "Flood",
    "Hijacking" = "Behavioural", 
    "Hostage Taking (Kidnapping)" = "Behavioural", 
    "Hurricane" = "Pressure-related", 
    "Other$" = "OTHER", 
    # Other is not a category in the technical report. 
    #  The events that are under the other category in FEMA vary from the loss
    #  of the space shuttle Columbia to power outwage, therefore I believe
    #  leaving this category open is fine. 
    "Severe Storm(s)" = "Precipitation-related", 
    # The events are all related to rain, flooding and ice, so it 
    #  is alright to categorize it as precipitation related 
    "Terrorist" = "Conflict", 
    "Toxic Substances" = "Other chemical hazards and toxins", 
    "Unarmed Assault" = "Conflict", 
    "Bombing/Explosion" = "Behavioural", 
    "Coastal Storm" = "Wind-related", 
    # The examples on the data set are tropical storms such as 
    #  "Tropical Storm Barry", "Tropical Storm Fay" 
    "Drought" = "Precipitation-related",
    "Facility/Infrastructure Attack" = "Behavioural", 
    "Fishing Losses" =  "Environmental degradation", 
    "Freezing" = "Temperature-related",
    "Hostage Taking (Barricade Incident)" = "Behavioural",
    "Human Cause" = "Behavioural", 
    "Mud/Landslide" = "Shallow geohazard", 
    "Severe Ice Storm" = "Precipitation-related",
    "Snow" = "Precipitation-related",
    "Tornado" = "Wind-related",
    "Tsunami" = "Shallow geohazard", 
    "Volcano" = "Volcanogenic (volcanoes and geothermal)")


fema_hazard_cluster_df <- 
  unique_fema_events_df %>%
  mutate(incident_type = str_remove(incident_type, pattern = "\\("), 
         incident_type = str_remove(incident_type, pattern = "\\)")
  ) %>% 
  mutate(
    hazard_cluster = str_replace_all(
      incident_type, rename_hazard_cluster_fema
    )
  ) %>% 
  select(Source, incident_type, hazard_cluster) %>% 
  distinct()

saveRDS(fema_hazard_cluster_df, "inst/extdata/fema_hazard_cluster.RDS")

usethis::use_data(fema_hazard_cluster_df, overwrite = TRUE)

