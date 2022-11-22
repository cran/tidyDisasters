# Third Step at Wrangling EMDAT Location Data
# Gabriel Odom and Catalina Canizares
# 2021-08-24

# This follows the work in wrangle_locations_state_abbrev_spellings_20210826.R



######  Setup  ################################################################
library(readxl)
library(usdata)
library(tidyverse)

emdatRaw_df <- read_excel(
	path = "data_raw/Death_20210621.xlsx",
	skip = 6,
	guess_max = 3000
)



#####  Additional Wrangling  ##################################################
# We keep finding rows that need more work, so we have gone back to the "state
#   abbreviations" script for some cases, but others we have added here.

emdatLocWrangled_df <- readRDS(
	file = "data_processed/emdat_locations_cleaner_20210826.RDS"
) %>% 
	###  Add Missing Locations  ###
	mutate(
		Location = case_when(
			event_id == "1994-0510-USA" ~ "FL, GA, SC, NC",
			event_id == "1995-0208-USA" ~ "PR",
			# https://en.wikipedia.org/wiki/Hurricane_Barry_(2019)
			event_id == "2019-0335-USA" ~ "LA, MS, AL, AR",
			# https://en.wikipedia.org/wiki/Hurricane_Bob
			# Richmond in this context most likely means Richmond County, aka Staten
			#   Island in NYC
			event_id == "1991-0218-USA" ~ str_replace(
				Location,
				pattern = "DE, NY, richmond",
				replacement = "(DE), staten island (NY)"
			),
			TRUE ~ Location
		)
	) 

###  Remove Directional Words  ###
# Eliminate words "southern", "south", "eastern", "north", "west", "western"
#   "area", "near", "areas", "northern"
emdatLocWrangled2_df <- 
	emdatLocWrangled_df %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "central")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "southern")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "south")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "eastern")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "east")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "northern")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "north")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "western")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "west")
	) %>% 
	mutate(
		Location = str_replace_all(
			Location, pattern = "areas", replacement = "area"
		),
		Location = str_remove_all(Location, pattern = "area")
	) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "near")
	) %>% 
	mutate(
		Location = str_replace_all(
			Location, pattern = "districts", replacement = "district"
		),
		Location = str_remove_all(Location, pattern = "district")
	) %>% 
  mutate(
    Location = str_replace_all(
      Location, pattern = "county", replacement = "counties"
    ),
    Location = str_remove_all(Location, pattern = "counties")
  ) %>% 
	mutate(
		Location = str_remove_all(Location, pattern = "middle")
	) %>% 
  mutate(
    Location = str_remove_all(Location, pattern = "cities")
  ) %>% 
	mutate(
		Location = str_replace_all(Location,	pattern = " and ", replacement = ", ")
	)

# Any of the rows with lower-case letters are rows that still need work.
emdatLocWrangled2_df %>% 
	mutate(inProgress = str_detect(Location, pattern = "[:lower:]")) %>% 
	filter(inProgress) %>% 
	# It appears that rows with counties in multiple states have ")," in them
	View()



######  Split by '),' and ');'  ###############################################
# We found a few rows that require us to fix them by hand before we can split
#   the data
# UPDATE 20210820: we found A LOT more typos that needed correction by hand.
#   we moved them to "wrangle_locations_state_abbrev_spellings_20210817.R
separado_df <-
	emdatLocWrangled2_df %>% 
	
	###  Separate into Rows  ###
	# Split when we see ")," or ");"
	separate_rows(Location, sep = "\\),|\\);") %>% 
	mutate(inProgress = str_detect(Location, pattern = "[:lower:]")) %>% 
	
	###  More Fixes  ###
	# Fix a few counties that are still listed as states
	mutate(
		Location = case_when(
			event_id == "2004-0245-USA" & str_detect(Location, pattern = "\\(MD") ~
				"washington (MD",
			TRUE ~ Location
		)
	) %>% 
	mutate(
		Location = case_when(
			event_id == "2000-0175-USA" & str_detect(Location, pattern = "worth") ~
				"fort worth [tarrant] (TX",
			TRUE ~ Location
		)
	) %>% 
	# Split state abbreviations from regions
	rowwise() %>% 
	mutate(
		State = case_when(
			!inProgress ~ Location,
			inProgress ~ paste(
				unlist(
					str_extract_all(
						Location, pattern = "[A-Z]{2}"
					)
				),
				collapse = ", "
			)
		)
	) %>% 
	mutate(
		Region = case_when(
			!inProgress ~ NA_character_,
			inProgress ~ paste(
				unlist(
					str_remove_all(
						Location, pattern = "[A-Z]{2}"
					)
				),
				collapse = ", "
			)
		)
	) %>% 
	ungroup()



###### Separate regions by "(" or "[" and tidy ################################

separado1_df <-
	separado_df %>% 
  separate_rows(Region, sep = "\\),") %>%
	separate_rows(Region, sep = "\\],|\\]") %>%
  separate(Region, c("Region", "County"), sep = "\\(|\\[") %>%  
	# Additional pieces discarded in 2 rows [684, 719]
	# UPDATE 2021-08-27: fixed entries for (2017-0136-USA, 2017-0282-USA)
	# Missing pieces filled with `NA` in 9 rows;
	#   [23, 26, 75, 388, 694, 719, 732, 747, 791]
	# We inspected these 9 locations, and there are no issues
	# UPDATE 2021-08-27: we added the row split on "]," and "]", and we have 9
	#   rows flagged after the column split on "[" and "("
  mutate(
    County = str_remove_all(County, pattern = "\\)"),
    County = str_remove_all(County, pattern = "\\,")
  ) %>% 
  mutate( 
    County = str_trim(County, side = "both"), 
    Region = str_trim(Region, side = "both")
  )

separado1_df %>% 
	view()

#######  Separate regions by "," ##########

separado2_df <- separado1_df %>% 
  separate_rows(Region, sep = "\\,") %>% 
  mutate( 
    Region = str_trim(Region, side = "both")
  ) 
  # drop_na(Region) %>% 
  # drop_na(County)

######  Save as RDS to Preserve Formatting  ###################################

saveRDS(
	separado2_df, file = "data_processed/emdat_locations_split_20210827.RDS"
)
	

