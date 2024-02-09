#------------------------ LOADING LIBRARIES ----------------------------
library(haven)
library(tidyverse)
library(ggplot2)

#------------------------- CREATING FUNCTIONS -----------------------------

# Summarizing function
crime_summarizer <- function(df){
  data <- df %>%
    mutate(violent_crime = actual_murder+actual_manslaughter+actual_rape_total+actual_robbery_total+actual_assault_aggravated) %>%
    group_by(year) %>%
    summarize(agencies_reporting = n(),
              total_crime = sum(actual_all_crimes),
              average_crime = mean(actual_all_crimes),
              total_violent_crime = sum(violent_crime),
              average_violent_crime = mean(violent_crime),
              total_murder = sum(actual_murder),
              average_murder = mean(actual_murder),
              total_robbery = sum(actual_robbery_total),
              average_robbery = mean(actual_robbery_total),
              total_assault = sum(actual_assault_total),
              average_assault = mean(actual_assault_total),
              total_rape = sum(actual_rape_total),
              average_rape = mean(actual_rape_total),
              total_burglary = sum(actual_burg_total),
              average_burglary = mean(actual_burg_total),
              total_theft = sum(actual_theft_total),
              average_theft = mean(actual_theft_total),
              total_prop_violent = sum(violent_crime)/sum(total_crime),
              average_prop_violent = mean(violent_crime/total_crime)
    )
  return(data)
}

# Population adjusted Summarizing function
crime_summarizer_adjusted <- function(df){
  data <- df %>%
    mutate(violent_crime = actual_murder + actual_manslaughter + actual_rape_total + actual_robbery_total + actual_assault_aggravated) %>%
    filter(population != 0) %>%
    group_by(year) %>%
    summarize(
      agencies_reporting = n(),
      total_crime_adjusted_for_pop = 100000 * sum(actual_all_crimes / population),
      average_crime_adjusted_for_pop = 100000 * mean(actual_all_crimes / population),
      total_violent_crime = 100000 * sum(violent_crime / population),
      average_violent_crime = 100000 * mean(violent_crime / population),
      total_murder = 100000 * sum(actual_murder / population),
      average_murder = 100000 * mean(actual_murder / population),
      total_robbery = 100000 * sum(actual_robbery_total / population),
      average_robbery = 100000 * mean(actual_robbery_total / population),
      total_assault = 100000 * sum(actual_assault_total / population),
      average_assault = 100000 * mean(actual_assault_total / population),
      total_rape = 100000 * sum(actual_rape_total / population),
      average_rape = 100000 * mean(actual_rape_total / population),
      total_burglary = 100000 * sum(actual_burg_total / population),
      average_burglary = 100000 * mean(actual_burg_total / population)
    )
  return(data)
}

# Under 18 summarizing function
crime_summarizer_under_18 <- function(df){
  data <- df %>%
    mutate(violent_crime_18 = clr_18_murder+clr_18_manslaughter+clr_18_rape_total+clr_18_robbery_total+clr_18_assault_aggravated,
           violent_crime = actual_murder + actual_manslaughter + actual_rape_total + actual_robbery_total + actual_assault_aggravated) %>%
    group_by(year) %>%
    summarize(agencies_reporting = n(),
              total_crime = sum(clr_18_all_crimes),
              average_crime = mean(clr_18_all_crimes),
              total_violent_crime = sum(violent_crime_18),
              average_violent_crime = mean(violent_crime_18),
              total_murder = sum(clr_18_murder),
              average_murder = mean(clr_18_murder),
              total_robbery = sum(clr_18_robbery_total),
              average_robbery = mean(clr_18_robbery_total),
              total_assault = sum(clr_18_assault_total),
              average_assault = mean(clr_18_assault_total),
              total_rape = sum(clr_18_rape_total),
              average_rape = mean(clr_18_rape_total),
              total_burglary = sum(clr_18_burg_total),
              average_burglary = mean(clr_18_burg_total),
              total_theft = sum(clr_18_theft_total),
              average_theft = mean(clr_18_theft_total),
              total_prop_violent = sum(violent_crime_18)/sum(clr_18_all_crimes),
              average_prop_violent = mean(violent_crime_18/clr_18_all_crimes, na.rm = TRUE),
              total_prop_all_clr_crimes = sum(clr_18_all_crimes)/sum(tot_clr_all_crimes),
              average_prop_all_clr_crimes = mean(clr_18_all_crimes/tot_clr_all_crimes, na.rm = TRUE),
              prop_youth_violent = sum(violent_crime_18)/sum(violent_crime)
    )
  return(data)
}

# Under 18 summarizing function adjusted
crime_summarizer_under_18_adjusted <- function(df){
  data <- df %>%
    filter(population != 0) %>%
    mutate(violent_crime_18 = clr_18_murder+clr_18_manslaughter+clr_18_rape_total+clr_18_robbery_total+clr_18_assault_aggravated) %>%
    group_by(year) %>%
    summarize(agencies_reporting = n(),
              total_crime = 100000 * sum(clr_18_all_crimes / population),
              average_crime = 100000 * mean(clr_18_all_crimes / population),
              total_violent_crime = 100000 * sum(violent_crime_18 / population),
              average_violent_crime = 100000 * mean(violent_crime_18 / population),
              total_murder = 100000 * sum(clr_18_murder / population),
              average_murder = 100000 * mean(clr_18_murder / population),
              total_robbery = 100000 * sum(clr_18_robbery_total / population),
              average_robbery = 100000 * mean(clr_18_robbery_total / population),
              total_assault = 100000 * sum(clr_18_assault_total / population),
              average_assault = 100000 * mean(clr_18_assault_total / population),
              total_rape = 100000 * sum(clr_18_rape_total / population),
              average_rape = 100000 * mean(clr_18_rape_total / population),
              total_burglary = 100000 * sum(clr_18_burg_total / population),
              average_burglary = 100000 * mean(clr_18_burg_total / population),
              total_theft = 100000 * sum(clr_18_theft_total / population),
              average_theft = 100000 * mean(clr_18_theft_total / population)
    )
  return(data)
}

#---------------------------- LOADING/CLEANING DATA ---------------------------------

# Crime data
yearly_crime <- readRDS("/Users/eappelson/Downloads/100707-V20/offenses_known_yearly_1960_2022.rds")

# Louisiana specific 
la_crime <- yearly_crime %>%
  filter(state_abb == "LA") %>%
  filter(last_month_reported != "no months reported") %>%
  filter(year >= 2010)

unique(la_crime$agency_name)

la_crime %>%
  group_by(year) %>%
  count() %>%
  arrange(desc(year))

# Mississippi specific
ms_crime <- yearly_crime %>%
  filter(state_abb == "MS") %>%
  filter(last_month_reported != "no months reported")

ms_crime %>%
  group_by(year) %>%
  count()

# Alabama specific
al_crime <- yearly_crime %>%
  filter(state_abb == "AL") %>%
  filter(last_month_reported != "no months reported")

al_crime %>%
  group_by(year) %>%
  count()

la_crime_summarized <- crime_summarizer(la_crime)
la_crime_summarized_adjusted <- crime_summarizer_adjusted(la_crime)
la_crime_under_18_summarized <- crime_summarizer_under_18(la_crime)
la_crime_under_18_summarized_adjusted <- crime_summarizer_under_18_adjusted(la_crime)
# ------------------------------- GENERAL CRIME --------------------------------
# Crime per year
ggplot(la_crime_summarized, aes(x = year, y = total_crime)) + 
  geom_line()

# Average Crime per Year
ggplot(la_crime_summarized, aes(x = year, y = average_crime)) + 
  geom_line()

# Crime per year adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_crime_adjusted_for_pop)) + 
  geom_line()

# Average crime per year adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_crime_adjusted_for_pop)) + 
  geom_line()

# <18 Crime per year 
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_crime)) + 
  geom_line()

# <18 Average crime per year
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_crime)) + 
  geom_line()

# <18 proportion of all crimes
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_prop_all_clr_crimes)) + 
  geom_line()

# <18 average proportion of all crimes
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_prop_all_clr_crimes)) + 
  geom_line()

# ------------------------------- VIOLENT CRIME --------------------------------

# Violent crime per year
ggplot(la_crime_summarized, aes(x = year, y = total_violent_crime)) + 
  geom_line()

# Average violent crime per year
ggplot(la_crime_summarized, aes(x = year, y = average_violent_crime)) + 
  geom_line()

# Violent crime per year adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_violent_crime)) + 
  geom_line()

# Average crime per adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_violent_crime)) + 
  geom_line()

# <18 violent crime per year
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_violent_crime)) + 
  geom_line()

# <18 average violent crime per year
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_violent_crime)) + 
  geom_line()

# Violent proportion of all crime
ggplot(la_crime_summarized, aes(x = year, y = total_prop_violent)) + 
  geom_line()

# Average violent proportion of all crime
ggplot(la_crime_summarized, aes(x = year, y = average_prop_violent)) + 
  geom_line()

# <18 proportion violent crime
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_prop_violent)) + 
  geom_line()

# <18 average proportion violent crime
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_prop_violent)) + 
  geom_line()

# ------------------------------- MURDER CRIME --------------------------------

# Murder over time
ggplot(la_crime_summarized, aes(x = year, y = total_murder)) + 
  geom_line()

# Average murder over time
ggplot(la_crime_summarized, aes(x = year, y = average_murder)) + 
  geom_line()

# Murder over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_murder)) + 
  geom_line()

# Average murder over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_murder)) + 
  geom_line()

# <18 murder over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_murder)) + 
  geom_line()

# <18 average murder over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_murder)) + 
  geom_line()

# ------------------------------- ASSAULT CRIME --------------------------------

# assault over time
ggplot(la_crime_summarized, aes(x = year, y = total_assault)) + 
  geom_line()

# Average assault over time
ggplot(la_crime_summarized, aes(x = year, y = average_assault)) + 
  geom_line()

# assault over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_assault)) + 
  geom_line()

# Average assault over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_assault)) + 
  geom_line()

# <18 assault over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_assault)) + 
  geom_line()

# <18 average assault over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_assault)) + 
  geom_line()

# ------------------------------- ROBBERY CRIME --------------------------------

# robbery over time
ggplot(la_crime_summarized, aes(x = year, y = total_robbery)) + 
  geom_line()

# Average robbery over time
ggplot(la_crime_summarized, aes(x = year, y = average_robbery)) + 
  geom_line()

# robbery over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_robbery)) + 
  geom_line()

# Average robbery over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_robbery)) + 
  geom_line()

# <18 robbery over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_robbery)) + 
  geom_line()

# <18 average robbery over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_robbery)) + 
  geom_line()

# ------------------------------- RAPE CRIME --------------------------------

# rape over time
ggplot(la_crime_summarized, aes(x = year, y = total_rape)) + 
  geom_line()

# Average rape over time
ggplot(la_crime_summarized, aes(x = year, y = average_rape)) + 
  geom_line()

# rape over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_rape)) + 
  geom_line()

# Average rape over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_rape)) + 
  geom_line()

# <18 rape over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_rape)) + 
  geom_line()

# <18 average rape over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_rape)) + 
  geom_line()

# ------------------------------- BURGAL CRIME --------------------------------

# burglary over time
ggplot(la_crime_summarized, aes(x = year, y = total_burglary)) + 
  geom_line()

# Average burglary over time
ggplot(la_crime_summarized, aes(x = year, y = average_burglary)) + 
  geom_line()

# burglary over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = total_burglary)) + 
  geom_line()

# Average burglary over time adjusted for population
ggplot(la_crime_summarized_adjusted, aes(x = year, y = average_burglary)) + 
  geom_line()

# <18 burglary over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_burglary)) + 
  geom_line()

# <18 average burglary over time
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_burglary)) + 
  geom_line()


#------------------------- YOUTH CRIME ANALYSIS --------------------------

# <18 General Crime
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_crime)) + 
  geom_line()

# IN
ggplot(la_crime_under_18_summarized, aes(x = year, y = average_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized_adjusted, aes(x = year, y = total_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized_adjusted, aes(x = year, y = average_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized, aes(x = year, y = total_prop_all_clr_crimes)) + 
  geom_line()

ggplot(la_crime_under_18_summarized, aes(x = year, y = average_prop_all_clr_crimes)) + 
  geom_line()

# <18 Violent Crime
ggplot(la_crime_under_18_summarized, aes(x = year, y = total_violent_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized, aes(x = year, y = average_violent_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized_adjusted, aes(x = year, y = total_violent_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized_adjusted, aes(x = year, y = average_violent_crime)) + 
  geom_line()

ggplot(la_crime_under_18_summarized, aes(x = year, y = total_prop_violent)) + 
  geom_line()

ggplot(la_crime_under_18_summarized, aes(x = year, y = average_prop_violent)) + 
  geom_line()


yearly_crime %>%
  filter(last_month_reported != "no months reported") %>%
  filter(year >= 2010) %>%
  group_by(year,state) %>%
  summarize(crime = mean(clr_18_all_crimes)) %>%
  filter(year == "2021")

yearly_crime %>%
  filter(last_month_reported != "no months reported") %>%
  filter(year >= 2010) %>%
  group_by(year,state) %>%
  summarize(crime = mean(clr_18_all_crimes)) %>%
  filter(year == "2022")
  

yearly_crime %>%
  filter(last_month_reported != "no months reported" & year %in% c(2020, 2022)) %>%
  group_by(year, state) %>%
  summarize(avg_crime = mean(clr_18_all_crimes)) %>%
  pivot_wider(names_from = year, values_from = avg_crime) %>%
  mutate(percent_difference = ((`2022` - `2020`) / `2020`) * 100) %>%
  select(state, percent_difference) %>%
  arrange(desc(percent_difference)) %>%
  mutate(state = str_to_title(state))

yearly_crime %>%
  filter(number_of_months_missing == 0 & year %in% c(2020, 2022)) %>%
  group_by(year, state) %>%
  summarize(avg_crime = mean(clr_18_assault_total)) %>%
  pivot_wider(names_from = year, values_from = avg_crime) %>%
  mutate(percent_difference = ((`2022` - `2020`) / `2020`) * 100) %>%
  select(state, percent_difference) %>%
  arrange(desc(percent_difference)) %>%
  mutate(state = str_to_title(state))

yearly_crime %>%
  mutate(violent_crime_18 = clr_18_murder+clr_18_manslaughter+clr_18_rape_total+clr_18_robbery_total+clr_18_assault_aggravated) %>%
  filter(last_month_reported != "no months reported" & year %in% c(2021, 2022)) %>%
  group_by(year, state) %>%
  summarize(avg_crime = mean(violent_crime_18)) %>%
  pivot_wider(names_from = year, values_from = avg_crime) %>%
  mutate(percent_difference = ((`2022` - `2021`) / `2021`) * 100) %>%
  select(state, percent_difference) %>%
  arrange(desc(percent_difference)) %>%
  mutate(state = str_to_title(state))
