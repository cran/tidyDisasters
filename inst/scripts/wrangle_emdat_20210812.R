# Explore EMDAT Disaster Data Set
# Gabriel Odom and Catalina Canizares
# 2021-08-12



######  Setup  ################################################################
library(readxl)
library(usdata)
library(tidyverse)

emdatRaw_df <- read_excel(
	path = "data_raw/Death_20210621.xlsx",
	skip = 6,
	guess_max = 3000
)



######  First Pass  ###########################################################
emdatWrangled1_df <- 
	emdatRaw_df %>% 
	###  Find Rows of Interest  ###
	filter(ISO == "USA") %>% 
	filter(
		!is.na(`Total Deaths`) | !is.na(`No Injured`)
	) %>% 
	
	###  Replace Missing Dates  ###
	replace_na(list(`Start Month` = 1, `Start Day` = 1)) %>% 
	mutate(
		begin_date = paste0(
			as.character(`Start Year`), "-",
			str_pad(`Start Month`, width = 2, side = "left", pad = "0"), "-",
			str_pad(`Start Day`, width = 2, side = "left", pad = "0")
		)
	) %>%
	replace_na(list(`End Month` = 12, `End Day` = 28)) %>% 
	mutate(
		end_date = paste0(
			as.character(`End Year`), "-",
			str_pad(`End Month`, width = 2, side = "left", pad = "0"), "-",
			str_pad(`End Day`, width = 2, side = "left", pad = "0")
		)
	) %>% 
	
	###  Combine Disaster Types/Categories  ###
	mutate(
		disaster_type = paste0(
			`Disaster Type`, ":", `Disaster Subtype`, ":", `Disaster Subsubtype`
		)
	) %>% 
	
	###  Tidy  ###
	mutate(Source = "EMDAT") %>% 
	select(
		Source, event_id = `Dis No`, begin_date, end_date, Location, disaster_type,
		nkill = `Total Deaths`, nwound = `No Injured`, event_name = `Event Name`,
		disaster_mag = `Dis Mag Value`, disaster_scale = `Dis Mag Scale`
	) 

emdatWrangled2_df <- emdatWrangled1_df %>% 
	select(event_id, Location) %>% 
	mutate(LocationOrig = Location) %>% 
	separate_rows(Location, sep = ",|;") %>% 
	mutate(Location = str_to_lower(Location)) %>% 
	mutate(
		Location = str_replace(
			Location, pattern = "provinces", replacement = "province"
		)
	) %>% 
	mutate(
		Location = str_remove(Location, pattern = "province")
	) %>% 
	mutate(Location = str_trim(Location, side = "both")) %>% 
	mutate(
		Location = str_remove(Location, pattern = " \\)")
	) %>% 
	mutate(state = usdata::state2abbr(Location)) %>%   
	# filter(is.na(state)) %>% 
	# pull(Location) %>% 
	# table() %>% 
	# sort(decreasing = TRUE) %>% 
	# head(100)
  mutate(stateabb = usdata::abbr2state(Location)) %>% 
  mutate(abbstate = usdata::state2abbr(stateabb)) %>% 
  mutate(
    state = case_when(
       ! is.na(state) ~ state,
         is.na(state) ~ abbstate
    )) 
  # filter(is.na(state)) 
  # mutate(state = str_remove(Location, pattern = "bahamas"))
  