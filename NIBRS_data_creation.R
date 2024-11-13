library(janitor)
library(tidyverse)

load_data_for_year <- function(year, offense_key = "offense_type_id") {
  df_arrest <- read_csv(paste0("LA-", year, "/NIBRS_ARRESTEE.csv")) %>% clean_names()
  df_offense_code <- read_csv(paste0("LA-", year, "/NIBRS_OFFENSE_TYPE.csv")) %>% clean_names()
  df_weapon_id <- read_csv(paste0("LA-", year, "/NIBRS_ARRESTEE_WEAPON.csv")) %>% clean_names()
  df_weapon <- read_csv(paste0("LA-", year, "/NIBRS_WEAPON_TYPE.csv")) %>% clean_names()
  df_incident <- read_csv(paste0("LA-", year, "/NIBRS_incident.csv")) %>% clean_names()
  if (year <= 2015){
    df_agency =  read_csv(paste0("LA-", year, "/agency_participation.csv")) %>% clean_names()
  } else {
    df_agency = read_csv(paste0("LA-", year, "/agencies.csv")) %>% clean_names() %>%
      distinct(agency_id, .keep_all = TRUE)
  }
  
  df <- df_arrest %>%
    left_join(df_offense_code, by = offense_key) %>%
    left_join(df_incident, by = "incident_id") %>% 
    left_join(df_agency, by = "agency_id") %>%
    left_join(df_weapon_id, by = "arrestee_id") %>%
    left_join(df_weapon, by = "weapon_id")
  
  return(df)
}

df_2010 <- load_data_for_year("2010", offense_key = "offense_type_id")
df_2011 <- load_data_for_year("2011", offense_key = "offense_type_id")
df_2012 <- load_data_for_year("2012", offense_key = "offense_type_id")
df_2013 <- load_data_for_year("2013", offense_key = "offense_type_id")
df_2014 <- load_data_for_year("2014", offense_key = "offense_type_id")
df_2015 <- load_data_for_year("2015", offense_key = "offense_type_id")
df_2016 <- load_data_for_year("2016", offense_key = "offense_type_id")
df_2017 <- load_data_for_year("2017", offense_key = "offense_type_id")
df_2018 <- load_data_for_year("2018", offense_key = "offense_type_id")
df_2019 <- load_data_for_year("2019", offense_key = "offense_type_id")
df_2020 <- load_data_for_year("2020", offense_key = "offense_type_id")
df_2021 <- load_data_for_year("2021", offense_key = "offense_code")
df_2022 <- load_data_for_year("2022", offense_key = "offense_code")
df_2023 <- load_data_for_year("2023", offense_key = "offense_code")


list_of_dfs <- list(df_2010, df_2011, df_2012, df_2013, df_2015, df_2016, df_2017, df_2018, df_2019, df_2020, df_2021, df_2022, df_2023)
years <- c(2010, 2011, 2012, 2013, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023)

list_of_dfs <- Map(function(df, year) {
  df[] <- lapply(df, as.character)        
  df$data_year <- as.character(year)
  return(df)
}, list_of_dfs, years)


combined_df <- bind_rows(list_of_dfs)

df <- combined_df %>%
  mutate(
    agencies = case_when(
      !is.na(agency_name) ~ agency_name,
      !is.na(ncic_agency_name) ~ ncic_agency_name,
      !is.na(ucr_agency_name) ~ ucr_agency_name,
      !is.na(pub_agency_name) ~ pub_agency_name
    ),
    agencies = tolower(agencies),
    
    population = case_when(
      !is.na(population) ~ population,
      !is.na(agency_population) ~ agency_population
    )
  ) %>%
  select(agencies, population, incident_id, age_num, crime_against, offense_category_name, offense_name, offense_group, offense_code, weapon_name, data_year) %>%
  mutate(
    age_group = case_when(
      age_num %in% c(0,9,00, 03,06,09) ~ NA,
      age_num < 17 ~ "Youth",
      age_num >= 17 ~ "Adult"
    ),
    weapon_group = case_when(
      weapon_name == "Unarmed" ~ "Unarmed",
      TRUE ~ "Armed"
    ),
    violent_status = case_when(
      offense_name %in% c("Aggravated Assault",
                          "Burglary/Breaking & Entering",
                          "Weapon Law Violations",
                          "Murder and Nonnegligent Manslaughter",
                          "Arson",
                          "Rape",
                          "Statutory Rape",
                          "Sodomy",
                          "Criminal Sexual Contact",
                          "Fondling",
                          "Human Trafficking, Commercial Sex Acts",
                          "Kidnapping/Abduction",
                          "Negligent Manslaughter",
                          "Sexual Assault With An Object") ~ "Violent",
      TRUE ~ "Non-Violent"
    )
  )

