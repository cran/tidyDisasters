# Join and Inspect Disaster Event Datasets
# 2021-10-27
# Gabriel Odom and Catalina Canizares

# In "src/join_fema_emdat_attempt_20211027.R", we have finally cleaned and
#   tidied the EMDAT and (FEMA + DHS) data sets. They have one row per event,
#   day, state, and county. We can now join these data sets, and we will inspect
#   their utility (we will most likely have additional modifications and
#   cleaning steps to complete, but we need to see the data first).


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

# allEventsMap_df <-
# 	full_join(
# 		fema_df %>%
# 			select(Day, state, county, femaID = event_id),
# 		emdat_df %>%
# 			select(Day, state, county, emdatID = event_id),
# 		by = c("Day", "state", "county")
# 	)
# saveRDS(
# 	allEventsMap_df,
# 	file = "data_processed/database_key_fema_emdat_20211027.RDS"
# )

allEventsMap_df <- readRDS(
	file = "data_processed/database_key_fema_emdat_20211027.RDS"
)



######  Example Using Data  ###################################################

###  Hurricane Katrina  ###
katrina_df <-
	allEventsMap_df %>%
	filter(Day <= "2005-08-31", Day >= "2005-08-01") %>%
	filter(state == "LA", county == "orleans") %>%
	select(-Day, -state, -county) %>%
	drop_na() %>%
	distinct()

katrina_df

# We find multiple records related to Hurricane Katrina. Query these:
katrina_df %>%
	left_join(fema_df, by = "femaID") %>%
	arrange(Day) %>%
	View()
katrina_df %>%
	left_join(emdat_df, by = "emdatID") %>%
	arrange(Day) %>%
	View()


###  Hurricanes in Miami  ###
emdat_df %>%
	filter(state == "FL", county == "miami-dade") %>%
	mutate(isHurricane = str_detect(incident_type, "cyclone")) %>%
	filter(isHurricane) %>%
	select(-incident_type, -isHurricane) %>%
	group_by(emdatID) %>%
	mutate(
		event_start = min(Day),
		event_end = max(Day)
	) %>%
	select(-Day) %>%
	distinct() %>%
	ungroup() %>%
	# Check these against FEMA for Miami-Dade
	left_join(
		allEventsMap_df %>%
			filter(state == "FL", county == "miami-dade") %>%
			select(femaID, emdatID) %>%
			distinct(),
		by = "emdatID"
	) %>%
	inner_join(
		fema_df %>%
			filter(state == "FL", county == "miami-dade") %>%
			select(-state, -county, -Day, -nkill, -nwound) %>%
			distinct(),
		by = "femaID"
	) %>%
	View()


#### Counties affected by Severe Ice and Snow By year ##########################
# COMMENT 2022-02-18: note that FEMA ID is unique by state AND county, which
#   means that we need to extract the first six characters (DR-XXX) in order
#   to group events. So perhaps we should ask "how many counties experienced
#   an {x} disaster event in year {y}?"

fema_df %>%
	mutate(Year = year(Day)) %>%
	filter(incident_type %in% c("Severe Ice Storm", "Snow")) %>%
	group_by(state, county, Year) %>%
	summarise(Frozen = n() >= 1L, .groups = "keep") %>%
	group_by(Year) %>%
	summarise(Count = sum(Frozen)) %>%
	ggplot() +
	  aes(x = Year, y = Count) +
	  labs(y = "No. Counties Frozen") +
	  geom_point()
# We just found the 1993 "Storm of the Century" and the 1996 blizzard:
# - https://en.wikipedia.org/wiki/1993_Storm_of_the_Century
# - https://en.wikipedia.org/wiki/North_American_blizzard_of_1996


#### How long do fires last in each state? ####
# NOTE: this will only account for  lethal fires, meaning more than 3 people
# where killed.
# EMDAT says lethal fires, on average, in each state last...

long_fire_emdat <-
  emdat_df %>%
  filter(
    incident_type == "Wildfire:NA:NA" |
    incident_type == "Wildfire:Forest fire:NA" |
    incident_type == "Wildfire:Land fire (Brush, Bush, Pasture):NA"
  ) %>%
  group_by(emdatID) %>%
  mutate(
    event_start = min(Day),
    event_end = max(Day)
  ) %>%
  mutate(diff = as.numeric(event_end - event_start) + 1) %>%
  ungroup() %>%
  group_by(state) %>%
  summarise(mean_fire = mean(diff), .groups = "keep") %>%
  arrange(desc(mean_fire))

# By state and year
lethal_fires_wide <-
  emdat_df %>%
  mutate(Year = year(Day)) %>%
  filter(
    incident_type == "Wildfire:NA:NA" |
      incident_type == "Wildfire:Forest fire:NA" |
      incident_type == "Wildfire:Land fire (Brush, Bush, Pasture):NA"
  ) %>%
  group_by(emdatID) %>%
  mutate(
    event_start = min(Day),
    event_end = max(Day)
  ) %>%
  mutate(diff = as.numeric(event_end - event_start) + 1) %>%
  ungroup() %>%
  group_by(state, Year) %>%
  summarise(mean_fire = mean(diff), .groups = "keep") %>%
  mutate(Year = paste0("Year: ", Year)) %>%
  arrange(Year) %>%
  pivot_wider(names_from = "Year", values_from = mean_fire)

# FEMA says fires, on average, in each state last...

lethal_fire_fema_wide <-
  fema_df %>%
  filter(incident_type == "Fire") %>% # 31,907,780 rows
  mutate(EventUniqueId = substr(femaID, 1, 6)) %>%
  select(-county, -femaID) %>%
  distinct() %>% # 17,446 rows
  group_by(EventUniqueId) %>%
  mutate(
    event_start = min(Day),
    event_end = max(Day)
  ) %>%
  mutate(diff = as.numeric(event_end - event_start) + 1) %>%
  ungroup() %>%
  select(-EventUniqueId, -Day) %>%
  distinct() %>% # 631 rows
  mutate(Year = year(event_start)) %>%
  group_by(state, Year) %>%
  summarise(mean_fire = mean(diff), .groups = "keep") %>%
  mutate(Year = paste0("Year: ", Year)) %>%
  arrange(Year) %>%
  pivot_wider(names_from = "Year", values_from = mean_fire)


# We are finding similar results in terms of the time that wildfires are burning:
# "Between 2003 and 2012, this number skyrocketed to nearly seven and half weeks (52 days)".
# https://mashable.com/article/wildfire-burn-how-long-climate-change
# thanks to this table we could add data into this document
# https://sgp.fas.org/crs/misc/IF10244.pdf


#### Which is the deadliest type of disaster in the USA ####

dead_EMDAT <-
  emdat_df %>%
  mutate(Year = year(Day)) %>%
  mutate(EventUniqueId = substr(emdatID, 1, 9)) %>%
  select(EventUniqueId, incident_type, nkill, Year) %>%
  distinct() %>%
  group_by(incident_type, Year) %>%
  summarise(total_death = sum (nkill), .groups = "keep") %>%
  mutate(Year = paste0("Year: ", Year)) %>%
  arrange(Year) %>%
  pivot_wider(names_from = "Year", values_from = total_death) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>% 
  arrange(desc(total))

# We are finding similar results: https://www.prb.org/resources/which-types-of-disasters-are-the-deadliest-in-the-u-s-the-answer-is-surprising/
 
#### Which is the disaster that injures more people in the USA ####

injured_EMDAT <-
  emdat_df %>%
  mutate(Year = year(Day)) %>%
  mutate(EventUniqueId = substr(emdatID, 1, 9)) %>%
  select(EventUniqueId, incident_type, nwound, Year) %>%
  distinct() %>%
  group_by(incident_type, Year) %>%
  summarise(total_wound = sum (nwound), .groups = "keep") %>%
  mutate(Year = paste0("Year: ", Year)) %>%
  arrange(Year) %>%
  pivot_wider(names_from = "Year", values_from = total_wound) %>% 
  rowwise() %>% 
  mutate(total = sum(c_across(where(is.numeric)), na.rm = TRUE)) %>% 
  arrange(desc(total))



###  Pulse Nightclub  ###
allEventsMap_df %>%
	filter(Day <= "2016-06-30", Day >= "2016-06-01") %>%
	filter(state == "FL", county == "orange") %>%
	arrange(Day)

# We find one record, from the GDT:
fema_df %>%
	filter(event_id == "201606120001_001")


###  9/11  ###
allEventsMap_df %>%
	filter(Day <= "2001-09-30", Day >= "2001-09-01") %>%
	filter(state == "NY", county == "new york") %>%
	arrange(Day) %>%
	View()

fema_df %>%
	filter(event_id %in% c("200109110004_001", "DR-1391-NY_3658"))


###  Hurricane Irene  ###
allEventsMap_df %>%
	filter(Day <= "2011-08-31", Day >= "2011-08-01") %>%
	filter(state == "NY", county == "new york") %>%
	arrange(Day)

fema_df %>%
	filter(event_id %in% c("EM-3328-NY_007", "DR-4020-NY_006")) %>%
	View()
emdat_df %>%
	filter(event_id == "2011-0328-USA_0001") %>%
	View()


#### Favorite Macgowan disaster
allEventsMap_df %>%
  filter(Day <= "2013-04-30", Day >= "2013-04-01") %>%
  filter(state == "MA", county == "suffolk") %>%
  arrange(Day)

fema_df %>%
  filter(event_id %in% c("EM-3362-MA_004")) %>%
  View()
emdat_df %>%
  filter(event_id == "2011-0328-USA_0001") %>%
  View()

###  Seattle 2013 YRBS  ###
allEventsMap_df %>%
	filter(Day <= "2012-12-31", Day >= "2012-01-01") %>%
	filter(state == "WA", county == "king") %>%
	arrange(Day)

# We find an event:
fema_df %>%
	filter(event_id == "DR-4056-WA_003")
emdat_df %>%
	filter(event_id == "2012-0611-USA_0002")


###  San Diego 2019 YRBS  ###
allEventsMap_df %>%
	filter(Day <= "2018-12-31", Day >= "2018-01-01") %>%
	filter(state == "CA", county == "san diego") %>%
	arrange(Day) %>%
	View()

# We need a helper function that collapses date ranges from the subset of the
#   events key data (but not the original events key, because we want the
#   queries to be as clean as possible).
fema_df %>%
	filter(event_id %in% c("FM-5251-CA_001", "DR-4353-CA_016")) %>%
	View()
# We see that there was a long-burning fire in 2018, but we don't know if there
#   were any casualties



######  Pull in Clean YRBS  ###################################################
yrbs_df <- read_csv(
	"data_processed/YRBS_20210607.csv",
	col_types = cols(...1 = col_skip())
)


###  Map from school districts to counties  ###
yrbs_df %>%
	pull(district) %>%
	table()

districtCountyMap_char <- c(
	# county = "School District" (in yrbs)
	baltimore = "Baltimore",
	suffolk = "Boston",
	broward = "Broward County",
	mecklenburg = "Charlotte-Mecklenburg County",
	cook = "Chicago",
	cuyahoga = "Cleveland",
	dallas = "Dallas",
	wayne = "Detroit",
	`district of colummbia` = "District of Columbia",
	duval = "Duval County",
	tarrant = "Fort Worth",
	harris = "Houston",
	`los angeles` = "Los Angeles",
	shelby = "Memphis",
	`miami-dade` = "Miami-Dade County",
	milwakee = "Milwakee",
	orleans = "New Orleans",
	`new york` = "New York City",
	alameda = "Oakland",
	orange = "Orange County",
	`palm beach` = "Palm Beach County",
	philadelphia = "Philadelphia",
	`san bernadino` = "San Bernadino",
	`san diego` = "San Diego County",
	`san francisco` = "San Francisco",
	king = "Seattle"
)

districtCountyMap_df <- tibble(
	county = names(districtCountyMap_char),
	district = unname(districtCountyMap_char)
)

yrbs_df %>%
	left_join(districtCountyMap_df, by = "district") %>%
	select(-district, -key) %>%
	select(Year, state, county, everything())

