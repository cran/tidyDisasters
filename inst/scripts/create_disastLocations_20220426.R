# Create Table of Disaster Locations
# Catalina Cañizares and Gabriel Odom
# 04/26/2022

# Now that we have a unique key for all events (by state, year, and month), we
#   will create a table of geographic locations by event

library(tidyDisasters)
library(tidyverse)

allEventsMap_df <- readRDS(
  file = "inst/extdata/key_fema_emdat.RDS"
)
# 36,224,806 x 5

allLocations_df <-
  allEventsMap_df %>%
  group_by(femaID) %>%
  fill(emdatID, .direction = "updown") %>%
  ungroup() %>%
  replace_na( list(femaID = "", emdatID = "") ) %>%
  mutate(femaID_trunc = str_remove(femaID, pattern = "_[^.]*$")) %>%
  mutate(smashedID = paste0(femaID_trunc, "_", emdatID)) %>%
  select(-femaID_trunc) %>% 
	select(smashedID, state, county) %>% 
	distinct()
# 279,149 x 3


###  Map All Locations to Unique Key  ###
data("allKeys_df")
disastLocations_df <- 
	left_join(allLocations_df, allKeys_df, by = "smashedID") %>% 
	select(eventKey, state, county) %>% 
	distinct()
# 279,149 x 3

usethis::use_data(disastLocations_df)


