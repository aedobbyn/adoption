
# Children in foster care waiting for adoption by race and Hispanic origin
# from
# http://datacenter.kidscount.org/data/tables/6674-children-in-foster-care-waiting-for-adoption-by-race-and-hispanic-origin#detailed/1/any/false/573,869,36,868,867/2638,2601,2600,2598,2603,2597,2602,1353/13723,13724

# Data originally form Adoption and Foster Care Analysis and Reporting System (AFCARS), \n
# made available through the National Data Archive on Child Abuse and Neglect

library(here)
library(tidyverse)
library(stringr)

adop_raw <- readxl::read_xlsx(here("data", "adop.xlsx")) 

# --------------- Initial munge ----------------

# Initial cleaning
adop <- adop_raw %>% 
  janitor::clean_names() %>% 
  rename(
    year = timeframe %>% as.numeric(),
    count = data %>% as.numeric()
  ) %>% 
  filter(!dataformat == "Percent") %>%   # We can calculate percents ourselves
  select(-dataformat) %>% 
  arrange(year) %>% 
  filter(!race_group == "Total")  # And sums


# Relevel race
adop$race_group <- factor(adop$race_group)

new_race_labels <- levels(factor(adop$race_group)) %>% 
  str_replace_all("Non-Hispanic ", "")
new_race_labels[which(new_race_labels == "multiple race groups")] <- "Multiple"

levels(adop$race_group) <- new_race_labels


# --------------- Plot ----------------

# Facet by every state
ggplot(adop, 
       aes(year, count, colour = race_group)) +
  geom_line() +
  facet_wrap(~ location)

# Just Illinois
ggplot(adop %>% filter(location == "Illinois"), 
       aes(year, count, colour = race_group)) +
  geom_line() +
  theme_bw() +
  ggtitle("Illinois") +
  labs(x = "Year", y = "Count",
       subtitle = "Children in Foster Care Waiting for Adoption",
       colour = "Race",
       caption = "Data from http://datacenter.kidscount.org/")

# Whole US
ggplot(adop %>% filter(location == "United States"), 
       aes(year, count, colour = race_group)) +
  geom_line() +
  theme_bw() +
  ggtitle("US-Wide") +
  labs(x = "Year", y = "Count",
       subtitle = "Children in Foster Care Waiting for Adoption",
       colour = "Race",
       caption = "Data from http://datacenter.kidscount.org/")






# ------ Census ------
# Whitepaper 
# https://www.census.gov/prod/2014pubs/p20-572.pdf
# https://www.census.gov/prod/cen2010/briefs/c2010br-14.pdf
# All data 
# https://www2.census.gov/census_2010/04-Summary_File_1/
# Data dictionary
# https://www.census.gov/prod/cen2010/doc/sf1.pdf










