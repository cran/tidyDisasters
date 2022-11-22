# Create Table of Disaster Mortality
# Catalina Cañizares and Gabriel Odom
# 04/27/2022

# Now that we have a unique key for all events (by state, year, and month), we
#   will create a table of the mortality for each event

library(tidyDisasters)
library(tidyverse)

clean_emdat <- readRDS(
  file = "inst/extdata/clean_emdat.RDS"
)
# 3101152 x 9
data("allKeys_df")
# 498,188 x 4
clean_fema <- readRDS(
	file = "inst/extdata/clean_fema.RDS"
)
# 32,938,843 x 8


###  Subset to Death Counts by Event  ###
# FEMA + GTD
subset_fema_df <-
	clean_fema %>% 
	select(femaID = event_id, nKilled = nkill, nWounded = nwound) %>% 
	distinct() %>% 
	mutate(hasCasualty = !is.na(nKilled) | !is.na(nWounded)) %>% 
	filter(hasCasualty) %>% 
	select(-hasCasualty)
# 39 x 3
rm(clean_fema); gc()

# EMDAT
subset_emdat_df <- 
	clean_emdat %>% 
	select(emdatID = region_id, nKilled = nkill, nWounded = nwound) %>% 
	distinct() %>% 
	replace_na(list(nKilled = 0, nWounded = 0)) 
# 4424 x 3; but this includes duplicates for region
rm(clean_emdat); gc()
	

###  Map All death data to Unique Key  ###
disastCasualties1_df <- 
  subset_emdat_df %>% 
	left_join(allKeys_df, by = "emdatID") %>% 
	select(eventKey, nKilled, nWounded) %>% 
	distinct()
# 6,000 x 3

disastCasualties2_df <- 
	subset_fema_df %>% 
	left_join(allKeys_df, by = "femaID") %>% 
	select(eventKey, nKilled, nWounded) 
# 39 x 3

disastCasualties_df <-
	bind_rows(disastCasualties1_df, disastCasualties2_df) %>% 
	arrange(eventKey) %>% 
	# There could be events with casualty information from both data sets. We will
	#   take the maximum
	group_by(eventKey) %>% 
	summarise(
		nKilled  = max(nKilled, na.rm = TRUE),
		nWounded = max(nWounded, na.rm = TRUE)
	) %>% 
	ungroup()
# 6037 x 3

anyDuplicated(disastCasualties_df$eventKey)
# disastCasualties_df[4062:4064, ]

usethis::use_data(disastCasualties_df)


