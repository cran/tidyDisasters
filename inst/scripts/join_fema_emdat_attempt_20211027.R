# Attempt to Match EMDAT to FEMA
# Gabriel Odom and Catalina Canizares
# 2021-10-06
# Last update 2021-10-25


# Overview: we would like to see if we can create a mapping between the events
#   in the EMDAT database and the FEMA+Terrorism databases. We have event IDs in
#   both sets of data, and we have day, state, and county as well. We should be
#   able to create a mapping by finding the closest matches by day and location
#   between the data sets.



######  Import the Wrangled Data  #############################################
library(tidyverse)
library(lubridate)
library(usdata)

emdat_df   <- readRDS(file = "data_processed/emdat_clean_20211006.RDS")
femaRaw_df <- read_csv(file = "data_processed/mass_casualty_20211019.csv")
uscities <- read_csv("data_raw/uscities.csv")
# TO DO: add back the original FEMA disaster tag (DONE); remove US territories
#   (other than Puerto Rico?); remove periods from county names (DONE)


###  Clean EMDAT Columns  ###
emdat2_df <-
	emdat_df %>%
	rename(
		incident_type = disaster_type,
		state = State,
		county = County
	) %>%
	select(
		Source, event_id, region_id, Day, state, county,
		incident_type, nkill, nwound
	)


### Clean FEMA Counties and States ###
# Identify states that are not US territory and remove
uscities %>% 
	select(state = state_id) %>% 
	pull(state) %>%
	unique()

# 50 states + DC and Puerto Rico
USstates <- c(
	"AK", "AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL",	"GA", "HI", "IA",
	"ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", "MI", "MN", "MO", "MS",
	"MT", "NC", "ND", "NE", "NH", "NJ",	"NM", "NV", "NY", "OH", "OK", "OR", "PA",
	"PR", "RI", "SC", "SD",	"TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY"
)

femaRaw1_df <-
	femaRaw_df %>% 
	mutate(
		county = str_to_lower(county), 
		county = str_remove_all(county, pattern = "\\.")
	) %>% 
	filter(state %in% USstates)


###  Clean FEMA Dates  ###
# We have some events where the end happens before the beginning
femaRaw1_df %>% 
	mutate(duration = end_date - begin_date) %>% 
	# rowwise() %>% 
	mutate(duration = end_date - begin_date) %>% 
	filter(duration < 0)
# We have 10: 6 from 1 event, 2 from another, and 2 from 2 other events.
# TO DO: clean these dates (DONE). While we're at it, add casualty information if 
#   Wikipedia has it.

# Missing dates?
femaRaw1_df %>% 
	filter(is.na(end_date)) %>% 
	View()
# 8076 records; 0 with missing beginning records.
femaRaw1_df %>% 
	filter(is.na(end_date)) %>% 
	pull(incident_type) %>% 
	table()
# 465 fires, and 7611 records for COVID-19 (all starting on January 20, 2020)
# For now, I think we remove COVID-19 records, and treat them distinctly

femaRaw2_df <-
	femaRaw1_df %>% 
	mutate(
  	# Hurricane Earl: https://www.fema.gov/disaster/1241
    begin_date = case_when(
      eventid == "DR-1241-FL" ~ as_date("1998-09-03"),
      TRUE ~ begin_date
    ),
    end_date = case_when(
      eventid == "DR-1241-FL" ~ as_date("1998-09-04"),
      TRUE ~ end_date
    ), 
    # https://www.fema.gov/disaster/1725
    begin_date = case_when(
      eventid == "DR-1725-ND" ~ as_date("2007-07-15"),
      TRUE ~ begin_date
    ),
    end_date = case_when(
      eventid == "DR-1725-ND" ~ as_date("2007-07-16"),
      TRUE ~ end_date
    ),  
    # https://www.fema.gov/disaster/2202
    begin_date = case_when(
      eventid == "FM-2202-TX" ~ as_date("1998-06-18"),
      TRUE ~ begin_date
    ),
    end_date = case_when(
      eventid == "FM-2202-TX" ~ as_date("1998-06-22"),
      TRUE ~ end_date
    ),
    # https://www.fema.gov/disaster/2199
    begin_date = case_when(
      eventid == "FM-2199-TX" ~ as_date("1998-05-13"),
      TRUE ~ begin_date
    ),
    end_date = case_when(
      eventid == "FM-2199-TX" ~ as_date("1998-05-20"),
      TRUE ~ end_date
    )
  ) 



######  Fire Duration  ########################################################
# How long to fires last?
femaRaw2_df %>% 
	filter(incident_type == "Fire") %>% 
	mutate(duration = end_date - begin_date) %>% 
	group_by(state) %>% 
	pull(duration) %>% 
	as.numeric() %>% 
	summary()


# More on the fires...
# Fixing the 465 fire records. Let's add 2 weeks for the forest fires in the
#   southeast. We know that the average duration of a US forest fire is 52
#   days, but this number is wildly skewed by the west coast fires. In the
#   southwast, daily rainfall is common, so these fires can usually be 
#   contained within a week or so.
# https://mashable.com/article/wildfire-burn-how-long-climate-change
# https://www.naplesnews.com/story/news/local/florida/2019/04/19/wildfire-season-starts-slow-florida-concern-grows-panhandle/3427721002/
femaRaw2_df %>% 
	filter(incident_type == "Fire") %>% 
	mutate(duration = end_date - begin_date) %>% 
	group_by(state) %>% 
	summarise(meanLen = mean(duration, na.rm = TRUE)) %>% 
	arrange(meanLen) %>% 
	View()
# Well that's just horribly wrong.

# Exploring fire durations by state and year
femaRaw2_df %>% 
	filter(incident_type == "Fire") %>% 
	mutate(duration = as.numeric(end_date - begin_date)) %>% 
	mutate(year = year(begin_date) - 1990) %>% 
	glm(duration ~ state + year, data = ., family = "poisson")
# If the coefficient for fire duration is negative, and we know that fires are
#   getting longer, then this relationship is NOT linear. I think we just find
#   state x year averages and then impute
femaRaw2_df %>% 
	filter(incident_type == "Fire") %>% 
	mutate(duration = as.numeric(end_date - begin_date)) %>% 
	mutate(year = year(begin_date) - 1990) %>% 
	ggplot() +
	  aes(x = year, y = log(duration), colour = state, group = state) +
	  geom_line()
# This is relatively flat over time, but we should still include that
#   information if available; for years where there is no information at all,
#   then we will impute to the average of the state.



###  Imputation by Mean  ###
ImputeMean <- function(x) {
	replace(x, is.na(x), mean(x, na.rm = TRUE))
}

fireDurationImpute_df <- 
	femaRaw2_df %>% 
	filter(incident_type == "Fire") %>% 
	mutate(duration = as.numeric(end_date - begin_date)) %>% 
	mutate(year = year(begin_date)) %>% 
	group_by(state, year) %>% 
	# For records with both state and year, impute by the average within both
	mutate(duration2 = ImputeMean(duration)) %>% 
	ungroup() %>% 
	group_by(state) %>% 
	mutate(duration3 = round(ImputeMean(duration2))) %>% 
	mutate(endImputed_date = begin_date + duration3) %>% 
	# select(begin_date, duration3, end_date, endImputed_date) 
	select(eventid, state, endImputed_date) 

femaRaw3_df <- 
	femaRaw2_df %>% 
	left_join(fireDurationImpute_df, by = c("eventid", "state")) %>% 
	mutate(
		endImp_date = case_when(
			!is.na(end_date) ~ end_date,
			is.na(end_date) ~ endImputed_date
		)
	)
# Confirm the imputed dates are the same when not missing
femaRaw3_df %>% 
	filter(!is.na(end_date)) %>% 
	mutate(error = as.numeric(end_date - endImp_date)) %>% 
	pull(error) %>% 
	summary()


###  Get rid of COVID  ###
# Confirmed that the only incident type without imputation is "Biological"
femaRaw3_df %>% 
  filter(is.na(endImp_date)) %>% 
  pull(incident_type) %>% 
  table()

# Confirmed that if I filter by biological I will only take away COVID
femaRaw3_df %>%
  filter(incident_type == "Biological") %>% 
  pull(begin_date) %>% 
  table()

# Taking away COVID and confirming the filter   
femaRaw4_df <- 
  femaRaw3_df %>% 
  filter(incident_type != "Biological" & begin_date <= "2020-01-20")

femaRaw4_df %>% 
  filter(is.na(endImp_date))
# all there


# # Let's look at the hurricane in Taylor co, FL in September 1998
# emdat_df %>% 
# 	filter(State == "FL") %>% 
# 	filter(Day <= "1998-09-08") %>% 
# 	filter(Day >= "1998-08-31")
# # Not there...


###  Split FEMA by Day  ###
femaByDay_df <- 
	femaRaw4_df %>% 
	group_by(eventid) %>% 
	mutate(event_id = as.character(1:n())) %>%
	mutate(event_id = str_pad(event_id, width = 3, side = "left", pad = "0")) %>% 
	mutate(event_id = paste0(eventid, "_", event_id)) %>% 
	mutate(duration = endImp_date - begin_date) %>% 
	# We fixed events with end day before start day
	# filter(duration >= 0) %>% 
	rowwise() %>% 
	mutate(
		Day = list( seq(begin_date, endImp_date, by = 'day') )
	) %>% 
	unnest(Day) %>% 
	ungroup() %>% 
	select(
		-duration, -begin_date, -end_date, -endImp_date, -endImputed_date, -eventid
	) %>% 
	select(Source, event_id, Day, everything())

saveRDS(femaByDay_df, file = "data_processed/mass_casualty_fema_20211027.RDS")



######  Attempt to Join  ######################################################
inner_join(
  emdat2_df, femaByDay_df,
  # by = c("Day", "state", "county")
  by = c("Day", "state")
) %>% 
	View()
# 5604 rows when we match by day, state, and county; 495,453 rows when we match
#   by day and state.
# After splitting the FEMA records to include date ranges, we match on 14,550
#   records by day, state, and county. When matching on day and state alone, we
#   get 5,875,598 records. I think this means that we need to add rows for each
#   county in EMDAT.



# Do we find Hurricane Earl?
inner_join(
	emdat2_df, femaByDay_df,
	# by = c("Day", "state", "county")
	by = c("Day", "state")
) %>% 
	filter(state == "FL") %>% 
	filter(Day <= "1998-09-08") %>% 
	filter(Day >= "1998-08-31")
# No...

# Do we find Hurricane Katrina?
inner_join(
	emdat2_df, femaByDay_df,
	# by = c("Day", "state", "county")
	by = c("Day", "state")
) %>% 
	filter(state == "LA") %>% 
	filter(Day <= "2005-08-31") %>% 
	filter(Day >= "2005-08-23") %>% 
	View()
# Yes! (but with 1300+ records. Why?)
# Ah, all of the counties in one data set are matched to EACH county in the
#   other. We REALLY need to add county data to EMDAT.



######  Counties in EMDAT  ####################################################
usCounties_df <- 
	uscities %>% 
	select(state = state_id, county = county_name) %>% 
	mutate(county = str_to_lower(county)) %>% 
	distinct()

emdatCountyImupted_df <- 
	emdat2_df %>% 
	filter(is.na(county)) %>% 
	select(-county) %>% 
	left_join(usCounties_df, ., by = "state") %>% 
	select(one_of(colnames(emdat2_df))) %>% 
	bind_rows(
		emdat2_df %>% 
			filter(!is.na(county))
	) %>% 
	arrange(event_id)

saveRDS(
	emdatCountyImupted_df,
	file = "data_processed/mass_casualty_emdat_20211027.RDS"
)
