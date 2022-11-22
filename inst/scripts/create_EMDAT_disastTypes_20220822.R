# Create Table of Type of Disasters for EMDAT
# Catalina Cañizares, Mark Macgowan and Gabriel Odom
# 08/22/2022

# We will create a table of the type of disasters  based on the classification 
#  of the Hazard Definition and Classification Review Technical Report
#  published by the UN Office for Disaster Risk Reduction (2020)
# This script will create a table matching the type of disasters reported by 
#  EMDAT and Dr. Mark Macgowan's and Catalina Canizares's opinion regarding the 
#  hazard cluster it should belong to

library(readxl)
library(tidyDisasters)
library(tidyverse)

# Loading the data we need to do the matches

clean_emdat <- load("inst/extdata/clean_emdat.rda")
data("clean_emdat")
# 3101152 x 9

hazard_report <- 
  read_xlsx("inst/extdata/20220822_hazard_report.xlsx")  %>%
  rename(hazard_type = `Hazard type`, hazard_cluster = `Hazard cluster`) 
# 303 x 2 

# We created the list of unique type of events in EMDAT to categorize them
#  according to the Hazard Definition and Classification Review Technical Report
#  in the hazard cluster. 


unique_emdat_events_df <- 
  clean_emdat %>% 
  select(Source, emdatID = region_id, incident_type) %>% 
  distinct()
# 4424 x 3

# I created a list pairing the type of disaster according to EMDAT and 
#  the technical report, to then use it in the `str_replace_all` function

rename_hazard_cluster <- 
  c("Drought:Drought:NA" = "Precipitation-related", 
    "Extreme temperature:Cold wave:NA" = "Temperature-related", 
    "Epidemic:Parasitic disease:NA" = "Infectious diseases (human and animal)", 
    "Extreme temperature:Severe winter conditions:Snow/Ice" = "Precipitation-related", 
    "Flood:NA:NA" = "Flood", 
    "Landslide:Landslide:NA" = "Shallow geohazard", 
    "Storm:Convective storm:Derecho" = "Wind-related", 
    "Storm:Convective storm:Lightning/Thunderstorms" = "Convective-related", 
    "Storm:Convective storm:Sand/Dust storm" = "Lithometeors", 
    "Storm:Convective storm:Tornado" = "Wind-related", 
    "Storm:Extra-tropical storm:NA" = "Pressure-related", 
    "Storm:Tropical cyclone:NA" = "Wind-related", 
    "Wildfire:Land fire (Brush, Bush, Pasture):NA" = "Environmental degradation (Forestry)",
    "Earthquake:Ground movement:NA" = "Seismogenic (earthquakes)", 
    "Epidemic:Viral disease:NA" = "Infectious diseases (human and animal)", 
    "Extreme temperature:Heat wave:NA" = "Temperature-related", 
    "Flood:Flash flood:NA" = "Flood",
    "Flood:Riverine flood:NA" = "Flood", 
    "Landslide:Mudslide:NA" = "Terrestrial",
    "Storm:Convective storm:Hail" = "Precipitation-related", 
    "Storm:Convective storm:NA" = "Convective-related", 
    "Storm:Convective storm:Severe storm" = "Convective-related",
    "Storm:Convective storm:Winter storm/Blizzard" = "Precipitation-related", 
    "Storm:NA:NA" = "Precipitation-related", 
    # 2007-0663-USA, 2007-0581-USA, 2018-0129-USA, 2003-0829-USA
    #  Classified as precipitation though several events with this category
    #  Are related to hail, snow and floods. 
    "Wildfire:Forest fire:NA" = "Environmental degradation (Forestry)",
    "Wildfire:NA:NA" = "Environmental degradation (Forestry)")


emdat_hazard_cluster_df <- 
  unique_emdat_events_df %>%
  mutate(incident_type = str_remove(incident_type, pattern = "\\("), 
         incident_type = str_remove(incident_type, pattern = "\\)")
  ) %>% 
  mutate(
    hazard_cluster = str_replace_all(
      incident_type, rename_hazard_cluster
    )
  ) %>% 
  select(Source, incident_type, hazard_cluster) %>% 
  distinct()

saveRDS(emdat_hazard_cluster_df, "inst/extdata/emdat_hazard_cluster.RDS")

usethis::use_data(emdat_hazard_cluster_df, overwrite = TRUE)
