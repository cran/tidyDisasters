# Serting our super ID to identify each event
# Catalina Cañizares and Gabriel Odom
# 04/30/2022

# To move the project forward and obtain functional tables from the merge of
#  FEMA and EMDAT we need to create a key. Dr. Odom and Catalina have agreed
#  upon creating a key that will contain the beginning year, month, state and
#  a 4 random number at the end that will identify the same event although it
#  touches different counties.



######  Setup and Data  #######################################################
library(lubridate)
library(tidyverse)

allEventsMap_df <- readRDS(
  file = "data_processed/database_key_fema_emdat_20211027.RDS"
)



######  Creating a Key  #######################################################

# Split GTD from FEMA
# allEventsMap_df$femaID %>%
#   str_sub(1, 2) %>%
#   table()
femaEvents_lgl <-
	allEventsMap_df$femaID %>%
	str_detect("DR|EM|FM") # What are these?

emdatEvents_lgl <-
  allEventsMap_df$femaID %>%
  is.na()

# FEMA key
year_month_state_FEMA_df <-
	allEventsMap_df %>%
  filter(femaEvents_lgl) %>%
  # Remove the last underscore and digits from the event ID
	mutate(key = str_remove(femaID, pattern = "_[^.]*$")) %>%
	group_by(key) %>%
	summarise(
	  startDate = min(Day),
	  state = unique(state)
	) %>%
  ungroup() %>%
	mutate(ym = format(startDate, format = "%Y_%m")) %>%
  group_by(ym, state) %>%
  # Add a counter for the number of events in each state in each month
  mutate(rowNum = as.character(1:n())) %>%
  mutate(rowNum = str_pad(rowNum, width = 3, side = "left", pad = "0")) %>%
	select(-startDate) %>%
  # smush them all together
  mutate(eventKey = paste0(ym, "_", state, "_", rowNum)) %>%
  ungroup() %>%
  select(key, eventKey) %>%
  # Our key will not be able to match back to the FEMA ids in the current form,
  #   so we need to map between the abbreviated FEMA ids and the full ones
  inner_join(
    allEventsMap_df %>%
      select(femaID) %>%
      mutate(key = str_remove(femaID, pattern = "_[^.]*$")),
    by = "key"
  ) %>%
  select(femaID, eventKey) %>%
  distinct() %>%
  arrange(eventKey)

# GTD Key
year_month_state_GTD_df <-
  allEventsMap_df %>%
  filter(!femaEvents_lgl) %>%
  mutate(ym = format(Day, format = "%Y_%m")) %>%
  group_by(ym, state) %>%
  mutate(rowNum = as.character(1:n())) %>%
  mutate(rowNum = str_pad(rowNum, width = 3, side = "left", pad = "0")) %>%
  mutate(eventKey = paste0(ym, "_", state, "_", rowNum)) %>%
  ungroup() %>%
  select(femaID, eventKey)

# EMDAT key
year_month_state_EmDat_df <-
  allEventsMap_df %>%
  filter(emdatEvents_lgl) %>%
  # Remove the last underscore and digits from the event ID
  mutate(key = str_sub(emdatID, end = -6)) %>%
  group_by(key) %>%
  summarise(
    startDate = min(Day),
    state = unique(state)
  ) %>%
  ungroup() %>%
  mutate(ym = format(startDate, format = "%Y_%m")) %>%
  group_by(ym, state) %>%
  # Add a counter for the number of events in each state in each month
  mutate(rowNum = as.character(1:n())) %>%
  mutate(rowNum = str_pad(rowNum, width = 3, side = "left", pad = "0")) %>%
  select(-startDate) %>%
  # smush them all together
  mutate(eventKey = paste0(ym, "_", state, "_", rowNum)) %>%
  ungroup() %>%
  select(key, eventKey) %>%
  # Our key will not be able to match back to the EmDat ids in the current form,
  #   so we need to map between the abbreviated EmDat ids and the full ones
  inner_join(
    allEventsMap_df %>%
      select(emdatID) %>%
      mutate(key = str_sub(emdatID, end = -6)),
    by = "key"
  ) %>%
  select(emdatID, eventKey) %>%
  distinct() %>%
  arrange(eventKey)

# Bind
allKeys_df <-
  bind_rows(
    year_month_state_GTD_df,
    year_month_state_EmDat_df,
    year_month_state_FEMA_df
  ) %>%
  select(eventKey, everything()) %>%
  arrange(eventKey)

# saveRDS(allKeys_df, file = "data_processed/all_outer_keys_20220330.rds")
rm(year_month_state_EmDat_df, year_month_state_FEMA_df, year_month_state_GTD_df)
gc()

allKeys_df %>%
  filter(!is.na(femaID)) %>%
  filter(!is.na(emdatID))
# These partition the space because of how we defined the *Events_lgl vectors



######  Join the Keys  ########################################################

# FEMA
femaMap_df <-
  allEventsMap_df %>%
  filter(femaEvents_lgl) %>%
  select(-emdatID, -Day) %>%
  full_join(allKeys_df, by = "femaID") %>%
  select(eventKey, femaID, emdatID) %>%
  distinct() %>%
  arrange(eventKey)

# EmDat
emdatMap_df <-
  allEventsMap_df %>%
  filter(emdatEvents_lgl) %>%
  select(-femaID, -Day) %>%
  full_join(allKeys_df, by = "emdatID") %>%
  select(eventKey, femaID, emdatID) %>%
  distinct() %>%
  arrange(eventKey)

# All Together
fullMap_df <-
  bind_rows(emdatMap_df, femaMap_df) %>%
  distinct()
