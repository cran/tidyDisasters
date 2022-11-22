# Nth (Final?) Attempt at Wrangling EMDAT Location Data
# Gabriel Odom and Catalina Canizares
# 2021-09-13



######  Setup  ################################################################
library(readxl)
library(usdata)
library(tidyverse)

emdatRaw_df <- read_excel(
	path = "data_raw/Death_20210621.xlsx",
	skip = 6,
	guess_max = 3000
)



######  Initial Wrangling  ####################################################
# Based on "src/wrangle_emdat_20210812.R", we already know this part works
emdatWrangled0_df <- 
	emdatRaw_df %>% 
	###  Find Rows of Interest  ###
	filter(ISO == "USA") %>% 
	filter(
		!is.na(`Total Deaths`) | !is.na(`No Injured`)
	) %>% 
	
	###  Remove Meta-data from Geo-Locations Column  ###
	# Some of the regions are marked with " (Adm1)." or " (Adm2)."
	mutate(
		`Geo Locations` = str_remove_all(
			`Geo Locations`,
			pattern = " \\(Adm\\d\\)\\."
		)
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
	# There is no storm in NOAA that matches this criteria. See
	#   https://www.ncdc.noaa.gov/billions/events/US/1989-2021
	#   https://www.weather.gov/aly/MajorWinterStorms
	# There was a blizzard in Seattle, but no official death count:
	#   https://climate.washington.edu/stormking/December1990.html
	filter(`Dis No` != "1990-0420-USA") %>% 
	
	
	select(
		Source, event_id = `Dis No`, begin_date, end_date, Location, disaster_type,
		nkill = `Total Deaths`, nwound = `No Injured`, event_name = `Event Name`,
		disaster_mag = `Dis Mag Value`, disaster_scale = `Dis Mag Scale`
	)


######  Add on Clean Location Data  ###########################################
emdatLocClean_df <- readRDS(
	"data_processed/emdat_clean_locations_20210913.RDS"
)

emdatClean1 <- 
	emdatWrangled0_df %>% 
	left_join(emdatLocClean_df, by = "event_id") %>% 
	select(
		Source, event_id, begin_date, end_date, Location, State, County, 
		everything()
	)


write_csv(emdatClean1, "data_processed/emdat_clean_locations_20210923.csv")


##### Erasing strange cases ###################################################

# After revising the final data set we found some cases that can be erased 

emdatLocClean1_df <- read_csv("data_processed/emdat_clean_locations_20210923.csv")

# I created an identification of cases in order to erase the ones that we detcted

emdatLocClean2_df <- emdatLocClean1_df %>% 
group_by(event_id) %>% 
  mutate(rowNum = as.character(1:n())) %>%
  mutate(rowNum = str_pad(rowNum, width = 4, side = "left", pad = "0")) %>% 
  arrange(event_id) %>% 
  mutate(region_id = paste0(event_id, "_", rowNum)) %>% 
  select(-rowNum)

# Here I eliminate the cases that are incorrect and erased the id column. 

emdatLocClean3_df <-   emdatLocClean2_df %>%
filter(`region_id` != "1992-0271-USA_0003") %>%  
filter(`region_id` != "2000-0175-USA_0003") %>% 
filter(`region_id` != "2001-0099-USA_0001") %>% 
filter(`region_id` != "2011-0425-USA_0001") %>% 
filter(`region_id` != "2012-0232-USA_0001") %>%
filter(`region_id` != "2014-0318-USA_0001") %>% 
filter(`region_id` != "2017-0282-USA_0001") %>%   
filter(`region_id` != "2018-0258-USA_0007") %>% 
filter(`region_id` != "2018-0419-USA_0019") %>% 
filter(`region_id` != "2018-0010-USA_0004") %>%   
filter(`region_id` != "2021-0150-USA_0001")


###### Pivot the dates to make the ranges long ################################

emdatclean_final<- emdatLocClean3_df %>% 
  mutate(diff=  end_date- begin_date) %>% 
  group_by(`region_id`) %>%
  mutate(Day = list(seq (begin_date, end_date, by = 'day'))) %>% 
  unnest(Day) %>%
  ungroup() %>% 
  select(Day, diff, begin_date, end_date, everything())


saveRDS(emdatclean_final, file = "data_processed/emdat_clean_20211006.RDS")