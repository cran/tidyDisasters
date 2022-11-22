## ---- message=FALSE, warning=FALSE--------------------------------------------
library(tidyDisasters)
library(lubridate)
library(tidyverse)

data("disastDates_df")
data("disastCasualties_df")
data("disastLocations_df")
data("disastTypes_df")

disastTypes_df %>%
  left_join(disastDates_df, by = "eventKey") %>%
  left_join(disastCasualties_df, by = "eventKey") %>% 
  left_join(disastLocations_df, by = "eventKey") %>% 
  mutate(Year = year(eventStart)) %>% 
  filter(Year == 2017 & state == "TX" & incident_type == "Hurricane") %>% 
  distinct() %>% 
  rmarkdown::paged_table()


## -----------------------------------------------------------------------------

fires_df <- 
	disastLocations_df %>%
  left_join(disastTypes_df, by = "eventKey") %>%
  left_join(disastDates_df, by = "eventKey") %>%
  mutate(Year = year(eventStart)) %>% 
  filter(hazard_cluster == "Environmental degradation (Forestry)") %>%
  group_by(state, county, Year) %>%
  summarise(Fire = n() >= 1L, .groups = "keep") %>%
  group_by(Year) %>%
  summarise(Count = sum(Fire))
  
ggplot(fires_df) +
  theme_classic() +
  theme(axis.text.x = element_text(size = 10, angle = 90)) +
  aes(x = Year, y = Count) +
  labs(
    title = "Number of Counties Affected by Fires Since the 90s",
    caption = "Data from the tidyDisasters R Package",
    y = "No. Counties affected by fires"
  ) +
  scale_x_continuous(breaks = 1990:2020) +
  scale_y_continuous(breaks = seq(0, 1000, by = 100)) +
	geom_vline(xintercept = 2000) +
	geom_vline(xintercept = 2001) +
  geom_point(size = 2, color = "#DA3330")
  

