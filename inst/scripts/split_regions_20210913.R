# Map Regions to Cities/Counties
# Gabriel Odom and Catalina Canizares
# 2021-09-13


library(readxl)
library(tidyverse)
library(usdata)



######  Data  #################################################################
emdatRaw_df <- read_excel(
	path = "data_raw/Death_20210621.xlsx",
	skip = 6,
	guess_max = 3000
)

cities_df <-
	read_csv("data_raw/uscities.csv") %>% 
	mutate(city_ascii = str_to_lower(city_ascii)) %>% 
	mutate(county_ascii = str_to_lower(county_name)) %>% 
	select(State = state_id, County = county_ascii, City = city_ascii) %>% 
	mutate(
		County = str_replace(County, pattern = "st\\.", replacement = "st")
	)

emdatLocWrangled_df <- readRDS(
	file = "data_processed/emdat_locations_split_20210827.RDS"
) %>% 
	mutate(
		County = str_replace(County, pattern = "st\\.", replacement = "st")
	) %>% 
	mutate(
		State = case_when(
			event_id == "2004-0338-USA" & State == "SC" & Region == "mississippi" ~ "MS",
			TRUE ~ State
		),
		Region = case_when(
			event_id == "2004-0338-USA" & State == "MS" & Region == "mississippi" ~ "",
			TRUE ~ Region
		)
	)

# How are missing regions marked?
table(is.na(emdatLocWrangled_df$Region)) # 418
table(emdatLocWrangled_df$Region == "")  #  58

# How are missing counties marked?
table(is.na(emdatLocWrangled_df$County)) #  438
table(emdatLocWrangled_df$County == "")  # 1262



######  Partition the Data  ###################################################
# State Alone:
statesOnly_df <- 
	emdatLocWrangled_df %>% 
	filter(is.na(Region) | Region == "") %>% 
	filter(is.na(County) | County == "") 

# State and County (but no region):
noRegion_df <-
	emdatLocWrangled_df %>% 
	filter(is.na(Region) | Region == "") %>% 
	filter(!is.na(County) & County != "") 


# State and Region (but no county):
noCounty_df <- 
	emdatLocWrangled_df %>% 
	filter(!is.na(Region) & Region != "") %>% 
	filter(is.na(County) | County == "") %>% 
	# We need a region ID to keep track of which rows we have fixed and are still
	#   missing
	group_by(event_id) %>% 
	mutate(rowNum = as.character(1:n())) %>%
	mutate(rowNum = str_pad(rowNum, width = 4, side = "left", pad = "0")) %>% 
	arrange(event_id) %>% 
	mutate(region_id = paste0(event_id, "_", rowNum)) %>% 
	select(-rowNum)

# State, County, and Region:
allGeoData_df <- 
	emdatLocWrangled_df %>% 
	filter(
		( !is.na(Region) & Region != "" ) & ( !is.na(County) & County != "" )
	)



######  Clean noRegion_df  ####################################################
noRegion_df %>% 
	nrow()

noRegion_df %>% 
	select(event_id, State, County) %>% 
	inner_join(
		cities_df %>% 
			select(State, County) %>% 
			distinct(),
		by = c("State", "County")
	) %>% 
	nrow()

# We are able to match all the counties to US counties, so no spelling errors
#   that we can see immediately



######  Clean allGeoData_df  ##################################################
allGeoData_df %>% 
	nrow()

allGeoData2_df <- 
	allGeoData_df %>% 
	mutate(
		County = case_when(
			County == "randall swisher" ~ "randall, swisher",
			County == "land" &
				event_id == "2007-0244-USA" &
				  State == "TX" ~
				    "eastland",
			# We think this was a copy-paste error in the original data, as it is
			#   adjacent to a "Hays county" in TX
			County == "hays" &
				event_id == "2015-0186-USA" &
				  State == "OK" ~
				    "rogers",
			TRUE ~ County
		)
	) %>% 
	separate_rows(County, sep = ",") %>% 
	# I don't want to do this for everything because I don't know what other issue
	#   it may cause
	mutate(County = str_trim(County))

allGeoData2_df %>% 
	select(event_id, State, County) %>% 
	inner_join(
		cities_df %>% 
			select(State, County) %>% 
			distinct(),
		by = c("State", "County")
	) %>% 
	nrow()

# # This matches all but 5 rows. What are they?
# allGeoData2_df %>%
# 	select(event_id, State, County) %>%
# 	anti_join(
# 		cities_df %>%
# 			select(State, County) %>%
# 			distinct(),
# 		by = c("State", "County")
# 	)
# 
# # 2002-0283-USA
# emdatLocWrangled_df %>% 
# 	filter(event_id == "2002-0283-USA") %>% 
# 	pull(LocationOrig)
# 
# # 2007-0244-USA
# emdatLocWrangled_df %>% 
# 	filter(event_id == "2007-0244-USA") %>% 
# 	pull(LocationOrig)
# # We removed the "east" from "eastland county". Oops
# 
# # 2015-0186-USA
# emdatLocWrangled_df %>% 
# 	filter(event_id == "2015-0186-USA") %>% 
# 	pull(LocationOrig)


cleanGeoCountyData <- 
	bind_rows(
		statesOnly_df, noRegion_df, allGeoData2_df
	) %>% 
	select(-Region)

rm(allGeoData_df, allGeoData2_df, noRegion_df, statesOnly_df)



######  Now for the Regions Data  #############################################

###  Match to Counties First  ###
matchedCounties_df <- 
	noCounty_df %>% 
	select(event_id, region_id, State, Region) %>% 
	mutate(
		Region = str_replace(Region, pattern = "st\\.", replacement = "st")
	) %>% 
	inner_join(
		cities_df %>% 
			select(State, Region = County) %>% 
			distinct(),
		by = c("State", "Region")
	) %>% 
	rename(County = Region)

View(matchedCounties_df)
# 1143 matches to counties; UPDATE: we replaced "st." with "st" and we now have
#   1151


###  Match to Cities  ###
matchedCities_df <- 
	noCounty_df %>% 
	select(event_id, region_id, State, Region) %>% 
	inner_join(
		cities_df %>% 
			select(State, Region = City) %>% 
			distinct(),
		by = c("State", "Region")
	) %>% 
	rename(City = Region) %>% 
	anti_join(
		matchedCounties_df, 
		by = c("event_id", "State", "City" = "County")
	) %>% 
	# 58 cities that are not counties, so join again to cities_df to find the
	#   counties that they sit in
	left_join(
		cities_df, by = c("State", "City")
	) %>% 
	select(-City)

cleanRegionData_df <- 
	bind_rows(
		matchedCounties_df, matchedCities_df
	) %>% 
	arrange(event_id) %>% 
	ungroup()



######  The Leftovers  ########################################################
# We are missing 40 rows. What are they?
missingCounties_df <- 
	noCounty_df %>% 
	anti_join(cleanRegionData_df, by = "region_id") 
	
View(missingCounties_df)

upMIcounties <- c(
	"Alger", "Delta", "Houghton", "Luce", "Menominee", "Baraga", "Dickinson",
	"Iron", "Mackinac", "Ontonagon", "Chippewa", "Gogebic", "Keweenaw",
	"Marquette", "Schoolcraft"
) %>% 
	str_to_lower()


missingCounties2_df <- 
	missingCounties_df %>% 
	mutate(
		County = case_when(
			region_id == "1992-0120-USA_0003" ~ "humboldt",
			region_id == "1992-0215-USA_0001" ~ "san bernardino",
			region_id == "1992-0271-USA_0002" ~ "kauai",
			region_id == "1992-0271-USA_0003" ~ "honolulu",
			region_id == "1993-0432-USA_0001" ~ "kitsap",
			region_id == "1993-0432-USA_0001" ~ "kitsap",
			# https://www.azcentral.com/story/news/local/arizona/2015/09/15/brief-history-arizonas-deadly-flash-floods/72335682/
			region_id == "1997-0172-USA_0001" ~ "coconino",
			region_id == "1999-0298-USA_0001" ~ "kenedy",
			region_id == "2003-0139-USA_0006" ~ "washington",
			# https://en.wikipedia.org/wiki/Buford,_Georgia
			region_id == "2003-0139-USA_0008" ~ "gwinnett",
			region_id == "2003-0210-USA_0037" ~ "mcdonough",
			region_id == "2003-0210-USA_0054" ~ "kosciusko",
			# We removed the "north", and they misspelled the "hampton" part
			region_id == "2003-0210-USA_0130" ~ "northampton",
			region_id == "2003-0210-USA_0132" ~ "oklahoma",
			region_id == "2003-0210-USA_0133" ~ "cleveland",
			region_id == "2003-0210-USA_0150" ~ "todd",
			# They misspelled "hood"
			region_id == "2003-0210-USA_0166" ~ "hood",
			region_id == "2005-0585-USA_0001" ~ "monroe",
			region_id == "2006-0180-USA_0007" ~ "stanislaus",
			region_id == "2016-0123-USA_0011" ~ "upshur",
			# Emory is in Rains County, TX, which is already included; we will keep
			#   Collin County
			region_id == "2016-0123-USA_0012" ~ "collin",
			region_id == "2016-0123-USA_0023" ~ "bexar",
			# Misspelling again
			region_id == "2016-0203-USA_0001" ~ "greenbrier",
			region_id == "2016-0552-USA_0002" ~ "bossier",
			# Upper Peninsula MI counties
			region_id == "2017-0253-USA_0002" ~ paste(upMIcounties, collapse = ", "),
			region_id == "2017-0362-USA_0004" ~ "harris",
			region_id == "2017-0362-USA_0068" ~ "west carroll",
			region_id == "2017-0362-USA_0076" ~ "vermilion",
			region_id == "2017-0434-USA_0010" ~ "calaveras",
			# We deleted the "west". This is "westlake village"
			region_id == "2018-0468-USA_0003" ~ "los angeles",
			region_id == "2018-0468-USA_0005" ~ "los angeles",
			region_id == "2018-0468-USA_0007" ~ "los angeles",
			region_id == "2020-0212-USA_0001" ~ "midland",
			region_id == "2020-0212-USA_0002" ~ "midland",
			region_id == "2020-0219-USA_0001" ~ "terrebonne",
			region_id == "2020-0219-USA_0003" ~ "lafourche"
		)
	)



######  Join the Data  ########################################################
missingCounties3_df <- 
	missingCounties2_df %>% 
	select(event_id, State, County) %>% 
	separate_rows(County, sep = ", ") %>% 
	ungroup()


cleanGeography_df <- 
	bind_rows(
		missingCounties3_df, 
		cleanRegionData_df %>% 
			select(-region_id),
		cleanGeoCountyData %>% 
			select(event_id, State, County)
	) %>% 
	arrange(event_id)
# We started with 1954 rows, and we now have 1973 rows because some locations
#   contained multiple counties

View(cleanGeography_df)

saveRDS(
	cleanGeography_df,
	"data_processed/emdat_locations_cleaner_20210913.RDS"
)

