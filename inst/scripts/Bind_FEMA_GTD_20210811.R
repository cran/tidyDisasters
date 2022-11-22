# Bind FEMA and GTD
# Catalina Cañizares
# 20210811
# last update: 20211018



# Setup -------------------------------------------------------------------

# install.packages("tidyverse")
# install.packages("usdata")
library(tidyverse)
library(readxl)
library(usdata)
library(lubridate)

# Opening data sets
# The last argument (guess_max) is to allow the guess of the type of columns 
# to go further than 100000
Terrorism <- read_xlsx("data_raw/terror_20210811.xlsx", guess_max = 100000)
NDisaster <- read_xlsx("data_raw/FEMA_20210318.xlsx")
uscities <- read_csv("data_raw/uscities.csv")


# GTD ---------------------------------------------------------------------

terror_clean <- 
  Terrorism %>% 
  filter(
    country == 217, 
    nkill >= 3,
  ) %>% 
  mutate(
    begin_date = paste0(
      iyear, "-",
      str_pad(imonth, width = 2, side = "left", pad = "0"), "-",
      str_pad(iday, width = 2, side = "left", pad = "0")
    )
  ) %>% 
  mutate(
    end_date = begin_date
    ) %>% 
  mutate(
    begin_date = ymd(begin_date),
    end_date = ymd(end_date)
  ) %>% 
  filter(
    begin_date >= "1990-01-01"
  ) %>% 
  mutate(
    state = state2abbr(provstate),
      ) %>%
  mutate(
    state = case_when(
      provstate == "Puerto Rico" ~ "PR",
      provstate != "Puerto Rico" ~  state )
    ) %>%  
  rename(incident_type = attacktype1_txt
  )  %>% 
  mutate(Source = "GTD") %>% 
  mutate(eventid = as.character(eventid)) %>% 
  select(
     Source, eventid, begin_date, end_date, state, city, incident_type,
     nkill, nwound
  ) 

usCounties_df <- 
  uscities %>% 
  select(city, state = state_id, county = county_name) %>% 
  mutate(
    city = case_when(
      city == "New York" ~ "New York City",
      city != "New York" ~ city
    )
  )

terror_clean_2 <- 
  terror_clean %>% 
  left_join(usCounties_df, by = c("state", "city")) %>% 
  select(
    Source, eventid, begin_date, end_date, state, county, incident_type, nkill, nwound
  )

# Update: 20211019 we added the original event id, so we saved a new version
#  of the CSV document. 

# write_csv(terror_clean_2, "Data Clean/Scripted/terror_clean_20210812.csv")

write_csv(terror_clean_2, "Data Clean/Scripted/terror_clean_20211019.csv")

# FEMA --------------------------------------------------------------------

FEMA_clean <- 
  NDisaster %>% 
  rename(
    begin_date = incidentBeginDate, end_date = incidentEndDate, 
         county = designatedArea, incident_type = incidentType, 
           eventid = femaDeclarationString 
  ) %>% 
  mutate(
    county = str_remove(county, pattern = " \\(.*")
  ) %>% 
  separate(
    begin_date, into = c("begin_date", "hour"), sep = "T"
    ) %>% 
  separate(
    end_date, into = c("end_date", "hour" ), sep = "T"
  ) %>% 
  mutate(
    begin_date = ymd(begin_date),
    end_date = ymd(end_date)
  ) %>%
  filter(
    begin_date >= "1990-01-01"
  ) %>% 
  mutate(Source = "FEMA") %>% 
  select(
    Source, eventid, begin_date, end_date, state, county, incident_type
  ) 

# Explore the data --------------------------------------------------------

FEMA_clean$incidentType %>%
  table() %>%
  sort()


# Bind --------------------------------------------------------------------


mass_casualty <- bind_rows(FEMA_clean, terror_clean_2)

# Update: 20211019 We added the event id to each data set and then binded. 
#  Therefore, we generated a new data set. 

# write_csv(mass_cassualty,"Data clean/Scripted/mass_cassualty_20210812")

write_csv(mass_casualty,"data_processed/mass_casualty_20211019.csv")



