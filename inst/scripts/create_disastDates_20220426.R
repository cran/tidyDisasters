# Create Table of Disaster Dates
# Catalina Cañizares and Gabriel Odom
# 04/26/2022

# Now that we have a unique key for all events (by state, year, and month), we
#   will create a table of calendar dates by event

library(lubridate)
library(tidyDisasters)
library(tidyverse)

allEventsMap_df <- readRDS(
  file = "inst/extdata/key_fema_emdat.RDS"
)
# 36,224,806 x 5

allDates_df <-
  allEventsMap_df %>%
  group_by(femaID) %>%
  fill(emdatID, .direction = "updown") %>%
  ungroup() %>%
  replace_na( list(femaID = "", emdatID = "") ) %>%
  mutate(femaID_trunc = str_remove(femaID, pattern = "_[^.]*$")) %>%
  mutate(smashedID = paste0(femaID_trunc, "_", emdatID)) %>%
  select(-femaID_trunc) %>% 
	select(smashedID, Day) %>% 
	# This groups all the counties together, so we are making the simplifying
	#   assumption that the date range is the same across the whole state, and not
	#   vastly different from one county to the next. Technically this is wrong,
	#   but we believe that this error will be just a few days one way or the
	#   other. For example, the main reason an event would have different start
	#   dates in different counties is a forest fire or hurricane, but these
	#   move relatively quickly, and we would expect the difference in start dates
	#   to be within a few weeks at most.
	distinct()
# 129,715 x 3


###  Map All Locations to Unique Key  ###
data("allKeys_df")
disastDates_df <- 
	left_join(allDates_df, allKeys_df, by = "smashedID") %>% 
	select(eventKey, Day) %>% 
	group_by(eventKey) %>% 
	summarise(
		eventStart = min(Day),
		eventEnd   = max(Day)
	) %>% 
	distinct()
# 8,258 x 3

usethis::use_data(disastDates_df)


