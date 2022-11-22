# Second Attempt at Wrangling EMDAT Location Data
# Gabriel Odom and Catalina Canizares
# 2021-08-24



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
		disaster_mag = `Dis Mag Value`, disaster_scale = `Dis Mag Scale`, `Geo Locations`
	)



######  Replace Regions with States  ##########################################
### In specific locations like "Pacific Northwest, Rockies, Northern Plains, 
# Upper Midwest, and the Northeast" where replaced with the
# information provided in column Geo location, whenever it was available.
emdatWrangled1_df <- 
	emdatWrangled0_df %>% 
	
	###  Regions to States  ###
  mutate(
  	Location = case_when(
      event_id == "2019-0045-USA" ~ str_replace(
      	Location,
      	pattern = "Midwest and Northeast",
        replacement = `Geo Locations`
      ),
      TRUE ~ Location
    ),
  	Location = case_when(
  		event_id == "2021-0096-USA" ~ `Geo Locations`,
  		TRUE ~ Location
  	),
  	Location = case_when(
  		event_id == "2018-0170-USA" ~ `Geo Locations`,
  		TRUE ~ Location
  	),
  	Location = case_when(
  		event_id == "2021-0211-USA" ~ `Geo Locations`,
  		TRUE ~ Location
  	),
  	  Location = case_when(
        event_id == "2019-0046-USA" ~ str_replace(
        Location, pattern = "Upper midwest",
        replacement = `Geo Locations`
    ),
    TRUE ~ Location),
      Location = case_when(
      event_id == "2019-0593-USA" ~ str_replace(
      Location, pattern = "Pacific Northwest, Rockies, Northern Plains, Upper Midwest, and the Northeast",
      replacement = `Geo Locations`
    ),
    TRUE ~ Location),
    Location = case_when(
      event_id == "2020-9585-USA" ~ str_replace(
        Location, pattern = "West and central states",
        replacement = `Geo Locations`
      ),
      TRUE ~ Location),
    Location = case_when(
      event_id == "2020-0580-USA" ~ str_replace(
        Location, pattern = "Plains , Midwest, Mid-Atlantic",
        replacement = `Geo Locations`
      ),
      TRUE ~ Location), 
    Location = case_when(
      event_id == "2018-0046-USA" ~ str_replace(
        Location, pattern = "Colorado, PLains and Midwest",
        replacement = `Geo Locations`
      ),
      TRUE ~ Location), 
  ) %>% 
	select(-`Geo Locations`) %>% 
	
	###  Fix Madison County Flood Dates  ### 
	# We believe the dates and location are wrong for the central VA flood of
	#   1995. There are only 6 floods in the USA in 1995, and we believe that the
	#   flood listed as being in the "Blue Ridge Mountain Canyon" in July of 1995
	#   is the same as the "Madison County Flood" in June of 1995
	# https://dailyprogress.com/community/greenenews/news/remembering-the-1995-flood/article_6edab9ae-b629-11ea-ae49-7f15490e6a62.html
	# https://www.glenallenweather.com/upload/Floods/MadisonCo1995.pdf
	# https://journals.ametsoc.org/view/journals/wefo/14/3/1520-0434_1999_014_0384_tmcvff_2_0_co_2.xml
	mutate(
		Location = case_when(
			event_id == "1995-0143-USA" ~ "madison (VA)",
			TRUE ~ Location
		),
		begin_date = case_when(
			event_id == "1995-0143-USA" ~ "1995-06-25",
			TRUE ~ begin_date
		),
		end_date = case_when(
			event_id == "1995-0143-USA" ~ "1995-06-27",
			TRUE ~ end_date
		)
	)



######  Map State Abbreviations to State Names  ###############################
# We have some states that are listed as abbreviations, which we will map to
#   their proper names first. In the next section, we will take all state names
#   and map them back to abbreviations

###  Map to States  ###
# abbreviations to states
replacePattern1_char <- 
	usdata::state_stats$state %>% 
	as.character() %>% 
	str_to_lower()
names(replacePattern1_char) <- as.character(usdata::state_stats$abbr)


###  Find Problems  ###
emdatLocWrangled1_df <- 
	emdatWrangled1_df %>% 
	select(event_id, Location) %>% 
	mutate(LocationOrig = Location) %>% 
	mutate(
		Location = str_replace_all(Location, replacePattern1_char)
	) %>% 
	mutate(Location = str_to_lower(Location)) %>% 
	# DO NOT USE
	# mutate(badEncoding = any(tools::showNonASCII(Location)))
	mutate(
		Location = str_replace_all(
			Location, pattern = "provinces", replacement = "province"
		),
		Location = str_remove_all(Location, pattern = "province")
	) %>% 
	
	###  Washington DC  ###
	# mutate(hasWash = str_detect(Location, pattern = "washing"))
  # mutate(hasDC = str_detect(Location, pattern = "d\\.c|dc"))
	# mutate(hasDC = str_detect(Location, pattern = "district of c")) %>% 
	# filter(hasDC)
	mutate(
		Location = str_replace_all(
			Location, pattern = "washington (dc)", replacement = "district of columbia" 
		),
		Location = str_replace_all(
			Location, pattern = "washington dc", replacement = "district of columbia" 
		),
		Location = str_replace_all(
			Location, pattern = "washington d.c.", replacement = "district of columbia" 
		),
		Location = str_replace_all(
			Location, pattern = "dc", replacement = "district of columbia" 
		)
	) %>% 
	
	###  New York City  ###
	# The places that "New York City" shows up, it shows up with states, not cities
	# mutate(hasNYC = str_detect(Location, pattern = "york c"))
	# mutate(hasNYC = str_detect(Location, pattern = "york"))
	mutate(
		Location = str_replace_all(
			Location, pattern = "new york city", replacement = "NY"
		),
		Location = str_replace_all(
			Location, pattern = "new-york", replacement = "NY"
		),
		Location = str_replace_all(
			Location, pattern = "n-york", replacement = "NY"
		)
	)



######  Fix State Name Misspellings  ##########################################

emdatLocWrangled2_df <- 
	emdatLocWrangled1_df %>% 
	
	###  Spelling issues  ###
	mutate(
		Location = str_replace_all(
			Location, pattern = "georgie", replacement = "georgia"
		),
		Location = str_replace_all(
			Location, pattern = "virginie", replacement = "virginia"
		),
		Location = str_replace_all(
			Location, pattern = "virgina", replacement = "virginia"
		),
		Location = str_replace_all(
			Location, pattern = "caroline du nord", replacement = "north carolina"
		), 
		Location = str_replace_all(
			Location, pattern = "north craolina", replacement = "north carolina"
		),
		Location = str_replace_all(
			Location, pattern = "nth.carolina", replacement = "north carolina"
		),
		Location = str_replace_all(
			Location, pattern = "norh carolina", replacement = "north carolina"
		),
		Location = str_replace_all(
			Location, pattern = "californie", replacement = "california"
		),
		Location = str_replace_all(
			Location, pattern = "louisiane", replacement = "louisiana"
		),
		Location = str_replace_all(
			Location, pattern = "lousiana", replacement = "louisiana"
		),
		# MS smart quotes strike again
		Location = str_replace_all(
			Location, pattern = "hawai‘i", replacement = "hawaii"
		),
		Location = str_replace_all(
			Location, pattern = "hawai", replacement = "hawaii"
		),
		Location = str_replace_all(
			Location, pattern = "hawaiii", replacement = "hawaii"
		),
		Location = str_replace_all(
			Location, pattern = "okhlahoma", replacement = "oklahoma"
		),
		Location = str_replace_all(
			Location, pattern = "ohlahoma", replacement = "oklahoma"
		),
		Location = str_replace_all(
		  Location, pattern = "okhahoma", replacement = "oklahoma"
		),
		Location = str_replace_all(
			Location, pattern = "washigton", replacement = "washington"
		),
		Location = str_replace_all(
			Location, pattern = "wsahington", replacement = "washington"
		),
		Location = str_replace_all(
			Location, pattern = "mississipi", replacement = "mississippi"
		),
		Location = str_replace_all(
			Location, pattern = "mississppi", replacement = "mississippi"
		),
		Location = str_replace_all(
			Location, pattern = "mssissippi", replacement = "mississippi"
		),
		Location = str_replace_all(
			Location, pattern = "pennsylvannia", replacement = "pennsylvania"
		),
		Location = str_replace_all(
			Location, pattern = "pensylvania", replacement = "pennsylvania"
		),
		Location = str_replace_all(
			Location, pattern = "pennsylvanie", replacement = "pennsylvania"
		),
		Location = str_replace_all(
			Location, pattern = "pennyslvania", replacement = "pennsylvania"
		),
		Location = str_replace_all(
			Location, pattern = "nebarska", replacement = "nebraska"
		),
		Location = str_replace_all(
			Location, pattern = "ilinois", replacement = "illinois"
		),
		Location = str_replace_all(
			Location, pattern = "new mexique", replacement = "new mexico"
		),
		Location = str_replace_all(
			Location, pattern = "missourri", replacement = "missouri"
		),
		Location = str_replace_all(
			Location, pattern = "arkanasas", replacement = "arkansas"
		),
		Location = str_replace_all(
			Location, pattern = "arkensas", replacement = "arkansas"
		),
		Location = str_replace_all(
			Location, pattern = "tannesse", replacement = "tennessee"
		),
		Location = str_replace_all(
			Location, pattern = "tennesse", replacement = "tennessee"
		),
		Location = str_replace_all(
			Location, pattern = "tennesseee", replacement = "tennessee"
		),
		Location = str_replace_all(
			Location, pattern = "kensas", replacement = "kansas"
		),
		Location = str_replace_all(
			Location, pattern = "hansas", replacement = "kansas"
		),
		Location = str_replace_all(
			Location, pattern = "massachusets", replacement = "massachusetts"
		),
		Location = str_replace_all(
			Location, pattern = "massachussetts", replacement = "massachusetts"
		),
		Location = str_replace_all(
		  Location, pattern = "massachussets", replacement = "massachusetts"
		),
		Location = str_replace_all(
			Location, pattern = "minissotta", replacement = "minnesota"
		),
		Location = str_replace_all(
			Location, pattern = "minnessota", replacement = "minnesota"
		),
		Location = str_replace_all(
			Location, pattern = "indianaa", replacement = "indiana"
		),
		Location = str_replace_all(
			Location, pattern = "indiania", replacement = "indiana"
		),
		Location = str_replace_all(
			Location, pattern = "montania", replacement = "montana"
		),
		Location = str_replace_all(
			Location, pattern = "rhode islands", replacement = "rhode island"
		),
		Location = str_replace_all(
			Location, pattern = "sth.carolina", replacement = "SC"
		),
		Location = str_replace_all(
			Location, pattern = "floride", replacement = "florida"
		),
		Location = str_replace_all(
			Location, pattern = "new jesey", replacement = "new jersey"
		),
		Location = str_replace_all(
			Location, pattern = "south dacota", replacement = "south dakota"
		)
	) 



######  Map State Names Back to State Abbreviations  ##########################
# states to abbreviations
replacePattern2_char <- as.character(usdata::state_stats$abbr)
names(replacePattern2_char) <- 
	usdata::state_stats$state %>% 
	as.character() %>% 
	str_to_lower()
	
emdatLocWrangled3_df <- 
  emdatLocWrangled2_df %>% 
	
	###  Directional States  ###
	# Fix for WV/NC/SC/ND/SD: the string replace sees "virginia" and replaces it
	#   with "VA" without regarding the "West" part; similar story for the other
	#   "North/South" states
	mutate(
		Location = str_replace_all(
			Location, pattern = "west virginia", replacement = "WV"
		),
		Location = str_replace_all(
			Location, pattern = "south and north carolinas", replacement = "SC, NC"
		),
		Location = str_replace_all(
			Location, pattern = "south and north carolina", replacement = "SC, NC"
		),
		Location = str_replace_all(
			Location, pattern = "north and south carolina", replacement = "NC, SC"
		),
		Location = str_replace_all(
			Location, pattern = "north and south carolina", replacement = "NC, SC"
		),
		Location = str_replace_all(
			Location, pattern = "s \\& n carolina", replacement = "SC, NC"
		),
		Location = str_replace_all(
			Location, pattern = "north, south carolina", replacement = "NC, SC"
		),
		Location = str_replace_all(
			Location, pattern = "north carolina", replacement = "NC"
		),
		Location = str_replace_all(
			Location, pattern = "south carolina", replacement = "SC"
		),
		Location = str_replace_all(
			Location, pattern = "north dakota", replacement = "ND"
		),
		Location = str_replace_all(
			Location, pattern = "south dakota", replacement = "SD"
		)
	) %>% 
	
	###  Replace States with Abbreviations  ###
	mutate(
		Location = str_replace_all(
			Location, pattern = "states", replacement = "state"
		),
		Location = str_remove_all(Location, pattern = "state")
	) %>% 
	mutate(
		Location = str_replace_all(Location, replacePattern2_char)
	)



######  Fixing State Abbreviations and Other Locations  #######################
# There are some cities/counties with the same names as states; change these
#   back to their original names
# USE "city, city [county] (state)," pattern
emdatLocWrangled4_df <- 
	emdatLocWrangled3_df %>% 
	mutate(
		Location = str_replace(
			Location, pattern = "kIA", replacement = "kiowa"
		),
		Location = str_replace(
			Location, pattern = "INpolis", replacement = "indianapolis"
		),
		Location = str_replace(
			Location, pattern = "OK city", replacement = "oklahoma city"
		),
		Location = case_when(
			event_id == "2018-0419-USA" ~ str_replace_all(
				Location, pattern = "CO", replacement = "colorado"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2016-0010-USA" ~ 
				"DC, NY, NJ, PA, MD, VA, TN, KY, DE, WV, GA, NC",
			TRUE ~ Location
		),
		# USE "city, city [county] (state)," pattern
		Location = case_when(
			event_id == "2012-0232-USA" ~ "colorado springs [el paso] (CO)",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "1995-0150-USA" ~ 
			  "MO, OK, WI, IA, NY, (DC), chicago IL",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2014-0540-USA" ~
			  "blair [washington] (NE), IA, KS, AR, WY",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2003-0210-USA" ~ str_replace_all(
				Location, pattern = "WA", replacement = "washington"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2003-0210-USA" ~ str_remove(
				Location, pattern = "- KS city"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2003-0210-USA" ~ str_replace(
				Location,
				pattern = "MS, craighead",
				replacement = "mississippi, craighead"
			),
			TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2003-0210-USA" ~ str_replace(
		    Location,
		    pattern = "hamilton-chattanooga,",
		    replacement = "chattanooga [hamilton],"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "MS, NV districts",
				replacement = "mississippi, nevada districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "perry, WA districts",
				replacement = "perry, washington districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "warrick, WA districts",
				replacement = "warrick, washington districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "lee, IA districts",
				replacement = "lee, iowa districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "mayes, OK, tulsa",
				replacement = "mayes, oklahoma, tulsa"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "wood, WY districts",
				replacement = "wood, wyoming districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0245-USA" ~ str_replace(
				Location,
				pattern = "WA district",
				replacement = "washington district"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2004-0245-USA" ~ str_replace(
				Location,
				pattern = "broome, DE districts",
				replacement = "broome, delaware districts"
			),
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2017-0362-USA" ~ 
				"rockport, corpus christi, port lavaca, cypress, houston, beaumont, port arthur, angelina, aransas, atascosa, austin, bastrop, bee, bexar, brazoria, brazos, burleson, caldwell, calhoun, cameron, chambers, colorado, comal, dewitt, fayette, fort bend, galveston, goliad, gonzales, grimes, guadalupe, hardin, harris, jackson, jasper, jefferson, jim wells, karnes, kerr, kleberg, lavaca, lee, leon, liberty, live oak, madison, matagorda, montgomery, newton, nueces, orange, polk, refugio, sabine, san jacinto, san patricio, trinity, tyler, victoria, walker, waller, washington, wharton, willacy, wilson, san augustine (TX), acadia, forrest, iberia, lafayette, vernon, beauregard, calcasieu, cameron, jefferson davis, vermillion, allen, natchitoches, rapides, sabine (LA)",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2002-0310-USA" ~ 
				"breckinridge, meade, crittenden, webster, hopkins, ohio, hardin, edmonson districts (KY), bollinger, howell districts (MO), charles, calvert, dorchester, wicomico, cecil districts (MD), clay, union, johnson, pope, moultrie, saline, bond (IL), gordon district (GA), atchison district (KS), erie, allegany districts (NY), stark district (OH), indiana, mercer, venango, butler, armstrong, columbia, lebanon, allegheny districts (PA), rutherford, lake, henry, carter districts (TN), shenandoah, greensville, bedford, campbell, nottoway, prince george (VA), marshall district (WV), pontotoc, chickasaw districts (MS), perry district (IN)",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2002-0700-USA" ~ str_replace(
				Location,
				pattern = "crittenden, MS districts",
				replacement = "crittenden, mississippi districts"
			),
			TRUE ~ Location
		), 
		Location = case_when(
			event_id == "2002-0119-USA" ~ str_replace(
				Location,
				pattern = "noble, OK, osage, pawnee, payne, roger mills, TX, WA",
				replacement =
					"noble, oklahoma, osage, pawnee, payne, roger mills, texas, washington"
			),
			TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2012-0542-USA" ~ str_replace(
		    Location,
		    pattern = "OK distr",
		    replacement =	"oklahoma district"
		  ),
		  TRUE ~ Location
		), 
		Location = case_when(
			event_id == "2012-0176-USA" ~ str_replace(
				Location,
				pattern = "WA",
				replacement =	"washington"
			),
			TRUE ~ Location
		), 
		Location = case_when(
			event_id == "2000-0067-USA" ~ 
			  "bullock, montgomery districts (AL ), colquitt, grady, mitchell, thomas, tift districts (GA ), washington district (FL ), lonoke, prairie, saline districts (AR )",
			TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2017-0564-USA" ~ 
		    "justin [denton], collin, rockwall, lubbock, seymour [baylor], dallas, fort worth [tarrant], houston [harris] (TX), el reno [canadian], oklahoma city, caddo [bryan], cleveland (OK), KS, KY, TN, MS, AL, (GA), indianapolis IN",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2001-0145-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2002-0165-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2002-0751-USA" ~ 
		    "DC, NC, SC, VA, NY, PA, KY, NJ, MD, MO, TN ",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2002-0880-USA" ~ str_replace(
		    Location,
		    pattern = "oswego, DE, dutchess districts",
		    replacement =
		      "oswego, delaware, dutchess districts"
		  ),
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2003-0139-USA" ~ 
		    "mitchell, worth districts (GA), jackson district (AL), NC, SC, TN",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2003-0139-USA" ~ 
		    "milam, guadalupe, bexar districts (TX), eagle district (CO), (MS), richland district (SC), franklinton city, washington district (LA), bufford city, gwinnett district (GA)",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2012-0177-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2004-0338-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2004-0338-USA" ~ str_replace(
		    Location,
		    pattern = "MS",
		    replacement =	"mississippi"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2004-0415-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2007-0244-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2015-0431-USA" ~ str_replace(
		    Location,
		    pattern = "WA",
		    replacement =	"washington"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2016-0331-USA" ~ 
		    "(MN); buffalo, chippewa, clark, columbia, crawford, eau claire, jackson, la crosse, monroe, richland, sauk, trempealeau, vernon (WI); allamakee, benton, black hawk, bremer, buchanan, butler, cedar, chickasaw, delaware, floyd, franklin, linn, wright (IA)",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2016-0123-USA" ~ 
		    "fort worth, dallas, wise, tarrant, denton, parker, wylie, plano, little elm, rockwall, bettie, emory in collin, denton, upshur, rockwall, rains, san antonio, helotes, alamo heights, kirby, hollywood park, carrizo springs, terrel hills, bexar, dimmit (TX), OK, KS, MO, AR, MS, LA",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2016-0181-USA" ~ 
		    "houston district (TX), brenham city, washington district, (TX), MT, KS, MO, CO",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2018-0010-USA" ~ 
		    "santa barbara, montecito, carpinteria, los angeles county, burbank, ventura (CA)",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2018-0010-USA" ~ 
		    "jefferson city, golden, barry county, carl junction, jasper county, MO",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2005-0585-USA" ~ 
		    "florida keys, naples areas, collier district FL",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2017-0381-USA" ~ 
	    "florida keys, monroe, south florida, jacksonville [duval], marco island, naples [collier], fort lauderdale [broward], lakeland [polk], orlando [orange], clay (FL), savannah, tybee island [chatham], brunswick, st. simons island [glynn], mcintosh, camden (GA), charleston, folly beach, the isle of palms, sullivan’s island [charleston], hilton head, beaufort, edisto beach [colleton] (SC)",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "1991-0423-USA" ~ 
		    "new england, (NY), NJ",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "1997-0307-USA" ~ str_replace(
		    Location,
		    pattern = "from GA to NY",
		    replacement =
		      "GA, SC, NC, VA, MD, DE, NJ, NY"
		  ),
		  TRUE ~ Location
		),
		# UPDATE 20210820: we don't know what this was supposed to be. We know that
		#   there is a "nevada county" in CA and AR.
		# And we found them!
		Location = case_when(
			event_id == "2004-0249-USA" ~ str_replace(
				Location,
				pattern = "NV",
				replacement = "nevada"
			),
			TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2005-0716-USA" ~ str_replace(
		    Location,
		    pattern = "NV",
		    replacement = "nevada"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
			event_id == "2017-0434-USA" ~ str_replace(
				Location,
				pattern = "NV",
				replacement = "nevada"
			),
			TRUE ~ Location
		),
		Location = case_when(
		  event_id == "1998-0066-USA" ~ str_replace(
		    Location,
		    pattern = "s AL, n and c GA",
		    replacement = "AL, GA"
		  ),
		  TRUE ~ Location
		),
		Location = case_when(
		event_id == "2001-0099-USA" ~ 
			"seattle [king], tacoma [pierce], olympia [thurston] (WA)",
		    TRUE ~ Location
		  ), 
		Location = case_when(
		  event_id == "2012-0542-USA" ~ 
		    "cleveland, creek, oklahoma, payne districts (OK)",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2014-0318-USA" ~ 
		    "san francisco, napa valley [napa], sonoma, vallejo [solano] (CA)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2015-0138-USA" ~ 
		    "fairdale area, [dekalb], rochelle town [ogle] (IL), MO, NC, IN, OH, KY, TX, IA, AR, MI, WV, WI, PA, OK, KS, TN ",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2016-0322-USA" ~ 
		    "GA, FL, SC, NY, (NJ), cedar key (FL), aurora (NC), tidewater region, (VA)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0136-USA" ~ 
		    "canton [van zandt] (TX), kampville [st charles] (MO), logan, marion, miles station [macoupin], sumner [lawrence], pawnee [sangamon] (IL); natural dam [crawford], mississippi, clay (AR), oak ridge [morehouse] (LA), labette (KS), cameron [le flore] (OK); goshen [oldham] (KY), OH, PA, WV, MS, AL, TN, IN",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0062-USA" ~ 
		    "san diego, los angeles, san francisco, sacramento (CA)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0563-USA" ~ 
		    "oak grove [jackson], trimble, plattsburg, lathrop, clinton [clay], (MO), centerville [appanoose], [muscatine] (IA), [sherburne], [freeborn], lake ann [carver] (MN), wabaunsee, pottawatomie, butler, (KS), WI, AR, OK, IL, MS, MI, NY, PA, MA, OH, NE, IN",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2017-0253-USA" ~ 
		    "minneapolis – st. paul, lac qui parle, montevideo [chippewa], slayton [murray], st. cloud [stearns], monticello [wright], coon rapids [anoka], (MN), st. croix (WI), upper peninsula (MI), richardson, stark, morton, milton, langdon [cavalier], (ND), deuel, jerauld, beadle, clark, (SD)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0512-USA" ~ 
		    "(MO), bedford in lawrence, muncie [delaware], salem [washington], blackford, jay (IN), celina, [mercer], williamsfield, [ashtabula], clyde, [sandusky], bloomingville, [erie], republic, west lodi, [seneca], galion [crawford], steuben, norwalk, fitchville, wakeman [huron], nova, hayesville [ashland], south vienna, [wayne], calcutta, [columbiana], clark, (OH), erie [erie], (PA), noble, [richland], (IL)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0383-USA" ~ 
		    "jersey shore, seaside park, point pleasant beach [ocean], long branch [monmouth] (NJ), fernandina beach [nassau], (FL)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2017-0434-USA" ~ 
		    "napa, sonoma, mendocino, lake, solano, butte, yuba, nevada, humboldt, calavaras, orange counties (CA)",
		  TRUE ~ Location
		),  
		Location = case_when(
		  event_id == "2018-0419-USA" ~ 
		    "bastrop, burnet, colorado, fayette, hood, jim wells, kerr, kimble, la salle, live oak, llano, mason, mcmullen, nueces, real, san patricio, travis, and williamson counties, (TX)",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2019-0250-USA" ~ 
		    "jefferson city, golden [barry], carl junction [jasper county], MO",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2021-0193-USA" ~ 
		    "davidson, rutherford, sullivan, wilson (TN)",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2005-0716-USA" ~ 
		    "reno, [washoe] (NV), truckee [nevada] (CA), napa, sonoma, mendocino, marin, solano, los angeles (CA), carson city district (NV )",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2007-0244-USA" ~ 
		    "wichita falls [wichita], georgetown [williamson], burnet, marble falls, granite shoals [burnet district], granbury [hood district], lampasas, parker [eastland] (TX), miami, commerce [ottawa],  shawnee, tecumseh, maud [pottawatomie], oklahoma city [oklahoma], waurika [jefferson], bartlesville, dewey [washington], [love], [lincoln] (OK), coffeyville area [montgomery], osawatomie [miami], allen, labette, neosho, wilson, [woodson] (KS), rockville, papinville [bates], [vernon] (MO)",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2012-0176-USA" ~ 
		    "bay, calhoun, escambia, franklin, gadsden, gulf, holmes, jackson, jefferson, leon, liberty, madison, okaloosa, santa rosa, taylor, wakulla, walton, washington, brevard, citrus, hardee, hernando, hillsborough, indian river, lake, manatee, marion, orange, osceola, pasco, pinellas, polk, seminole, sumter, volusia (FL)",
		  TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2017-0022-USA" ~ 
		    "cook (GA), MS, AL, LA, FL",
		  TRUE ~ Location
		),
    Location = case_when(
      event_id == "1995-0045-USA" ~ 
        "new orleans (LA)",
      TRUE ~ Location
     ),
    Location = case_when(
      event_id == "2021-0150-USA" ~ 
        "maui, kalawao, kaua‘i, oahu island [honolulu], HI",
      TRUE ~ Location
    ), 
		Location = case_when(
		  event_id == "2016-0322-USA" ~ 
		    "GA, FL, SC, NY, (NJ), cedar key (FL), aurora (NC), (VA)",
		  TRUE ~ Location
		)
	) %>% 
  
	# Single states with no counties are included with other states when we split
	#   on ")," or ");", so we need to wrap these states in "()".
	mutate(
		Location = case_when(
			event_id == "2016-0292-USA" ~ 
				"allamakee, clayton, fayette, howard, winneshiek counties (IA), LA, MS, MO, IL, IN",
			TRUE ~ Location
		),
		Location = case_when(
			event_id == "2021-0178-USA" ~ 
				"calhoun, bibb, jefferson, shelby and calhoun counties (AL); (MS); coweta and fayette counties (GA); TN",
			TRUE ~ Location
		), 
    Location = case_when(
      event_id == "2016-0552-USA" ~ 
        "shreveport, haugton (LA), TX, AR, MS",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1991-0423-USA" ~ 
        "RI, CT, MA, NH, VT, ME, NY, NJ",
      TRUE ~ Location
    ),
		Location = case_when(
		  event_id == "2016-0521-USA" ~ 
		    "MO, MD, OK, IN, MI, NM, OH, VA",
		  TRUE ~ Location
		),
		# https://www.azcentral.com/story/news/local/arizona/2017/07/16/payson-flash-flood-results-four-deaths/482805001/
		Location = case_when(
			event_id == "2017-0282-USA" ~ 
				"water wheel falls [gila], AZ",
			TRUE ~ Location
		),
		Location = case_when(
		  event_id == "2011-0425-USA" ~ 
		    "charlotte [mecklenburg], NC",
		  TRUE ~ Location
		), 
		Location = case_when(
		  event_id == "2016-0181-USA" ~ 
		    "brenham [washington], houston (TX), MT, KS, MO, CO",
		  TRUE ~ Location
		)
  )
 


######  Fix Miscellaneous  ####################################################
# UPDATE 2021-08-23: Catalina fixed new england, bahamas, and single cities

emdatLocWrangled5_df <- 
	emdatLocWrangled4_df %>% 
	
  ###  Replace New England with the actual states  ###
  mutate (
    Location = str_replace_all(
     Location, pattern = "new england", 
      replacement = "RI, CT, MA, NH, VT, ME"
    )
  ) %>%    
  
  ###  We eliminated Bahamas  ###
  mutate(
    Location = case_when(
      event_id == "1999-0435-USA" ~ 
        "FL",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1999-0619-USA" ~ 
        "NC, VA",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1992-0066-USA" ~ 
        "FL, LA",
      TRUE ~ Location
    )
  ) %>% 
  
  ###  Add states where the city was mentioned without state  ###
  mutate(
    Location = case_when(
    	event_id == "1991-0426-USA" ~ "oakland (CA)",
      TRUE ~ Location
    ),
    Location = case_when(
        event_id == "1994-0149-USA" ~
          "los angeles (CA)",
        TRUE ~ Location
      ),
    Location = case_when(
      event_id == "1995-0288-USA" ~
          "atlanta (GA), detroit (MI), philadelphia (PA), chicago (IL), milwaukee (WI)",
        TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1995-0073-USA" ~
        "los angeles (CA), dallas (TX), fort worth (TX), NM",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1995-0403-USA" ~
        "TX, OK, MO, KS, AR, MS, IA, (IL), los angeles (CA)",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "1997-0083-USA" ~
        "grand forks (ND), fargo (ND)",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "2015-0046-USA" ~
        "TN, KS, DC, OH, VA, GA, NC, (SC), boston (MA)",
      TRUE ~ Location
    ),
    Location = case_when(
      event_id == "2018-0206-USA" ~
        "NY, DC, TX, KS, CO, OK, MO, IL, IN, IA, NJ, PA, VA, WV, MA, (CT), baltimore (MD)",
      TRUE ~ Location
    )
  )



######  Save as RDS to Preserve Formatting  ###################################
saveRDS(
	emdatLocWrangled5_df,
	file = "data_processed/emdat_locations_cleaner_20210826.RDS"
) 

# write_csv(emdatLocWrangled4_df, "data_processed/emdatLocWrangled4_df.csv")



