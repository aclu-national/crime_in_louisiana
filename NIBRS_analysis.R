# ------------------------- LIBRARIES AND DATA CREATION --------------------
library(tidyverse)
library(janitor)
library(lubridate)

la_data_2022 <- da38925.0003 %>%
  filter(STATE == "(17) LA") %>%
  select(-FIPS_STATE) %>%
  mutate_all(as.character)

la_data_2021 <- da38807.0003 %>%
  filter(STATE == "(17) LA") %>%
  select(-FIPS_STATE) %>%
  mutate_all(as.character)

la_data_2020 <- da38566.0003 %>%
  filter(STATE == "(17) LA") %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(as.character)

la_data_2019 <- da38565.0003 %>%
  filter(STATE == "(17) LA") %>%
  mutate_if(is.factor, as.character) %>%
  mutate_all(as.character)

la_data_2018 <- da37649.0003 %>%
  filter(STATE == "(17) LA") %>%
  mutate_all(as.character)

la_data_2017 <- da37650.0003 %>%
  filter(STATE == "(17) LA") %>%
  mutate_all(as.character)

la_crime <- bind_rows(la_data_2017, 
          la_data_2018, 
          la_data_2019, 
          la_data_2020, 
          la_data_2021,
          la_data_2022)

names <- read_csv("variable_names.csv") %>%
  clean_names() %>%
  filter(!is.na(variable_name)) %>%
  select(variable_name, variable_label, notes_and_clarifications) %>%
  filter(variable_name != "FIPS_STATE")

names(la_crime) <- names$variable_label

la_crime <- la_crime %>%
  clean_names() %>%
  mutate(
    age_of_offender_1 = as.numeric(age_of_offender_1),
    age_of_offender_2 = as.numeric(age_of_offender_2),
    age_of_offender_3 = as.numeric(age_of_offender_3),
    age_under_18 = ((age_of_offender_1 < 18 & age_of_offender_1 != 0)|
                    (age_of_offender_2 < 18 & age_of_offender_2 != 0)|
                    (age_of_offender_3 < 18 & age_of_offender_3 != 0)),
    age_under_18 = replace_na(age_under_18, FALSE),
    age_17 = (age_of_offender_1 == 17|
                age_of_offender_2 == 17|
                age_of_offender_3 == 17),
    age_17 = replace_na(age_17, FALSE),
    age_16 = (age_of_offender_1 == 16|
                age_of_offender_2 == 16|
                age_of_offender_3 == 16),
    age_16 = replace_na(age_16, FALSE),
    age_15 = (age_of_offender_1 == 15|
                age_of_offender_2 == 15|
                age_of_offender_3 == 15),
    age_15 = replace_na(age_15, FALSE),
    age_14 = (age_of_offender_1 == 14|
                age_of_offender_2 == 14|
                age_of_offender_3 == 14),
    age_14 = replace_na(age_14, FALSE),
    age_13 = (age_of_offender_1 == 13|
                age_of_offender_2 == 13|
                age_of_offender_3 == 13),
    age_13 = replace_na(age_13, FALSE),
    formatted_date = dmy(incident_date),
    year_month = format(formatted_date, "%Y-%b"),
    year = master_file_year,
    post_RTA = formatted_date >= "2019-03-01",
    post_RTA_violent = formatted_date >= "2020-07-01",
    age_given = ((!is.na(age_of_offender_1) & age_of_offender_1 != 0)|
                   (!is.na(age_of_offender_2) & age_of_offender_2 != 0)|
                   !is.na(age_of_offender_3) & age_of_offender_3 != 0),
    weapon_law_violation = (ucr_offense_code_1 == "(520) Weapon Law Violations"|
                            ucr_offense_code_2 == "(520) Weapon Law Violations"|
                            ucr_offense_code_3 == "(520) Weapon Law Violations"),
    drug_law_violation = (ucr_offense_code_1 %in% c("(351) Drug/Narcotic Violations", "(352) Drug Equipment Violations")|
                          ucr_offense_code_2 %in% c("(351) Drug/Narcotic Violations", "(352) Drug Equipment Violations")|
                          ucr_offense_code_3 %in% c("(351) Drug/Narcotic Violations", "(352) Drug Equipment Violations")),
    violent_violation = (ucr_offense_code_1 %in% 
                           c("(091) Murder/Nonnegligent Manslaughter",
                             "(092) Negligent Manslaughter",
                             "(093) Justifiable Homicide",
                             "(100) Kidnaping/Abduction",
                             "(111) Rape",
                             "(112) Sodomy",
                             "(113) Sexual Assault With An Object",
                             "(114) Fondling (Indecent Liberties/Child Molesting)",
                             "(131) Aggravated Assault",
                             "(370) Pornography/Obscene Material")|
                         ucr_offense_code_2 %in% 
                           c("(091) Murder/Nonnegligent Manslaughter",
                             "(092) Negligent Manslaughter",
                             "(093) Justifiable Homicide",
                             "(100) Kidnaping/Abduction",
                             "(111) Rape",
                             "(112) Sodomy",
                             "(113) Sexual Assault With An Object",
                             "(114) Fondling (Indecent Liberties/Child Molesting)",
                             "(131) Aggravated Assault",
                             "(370) Pornography/Obscene Material")|
                          ucr_offense_code_3 %in% 
                           c("(091) Murder/Nonnegligent Manslaughter",
                             "(092) Negligent Manslaughter",
                             "(093) Justifiable Homicide",
                             "(100) Kidnaping/Abduction",
                             "(111) Rape",
                             "(112) Sodomy",
                             "(113) Sexual Assault With An Object",
                             "(114) Fondling (Indecent Liberties/Child Molesting)",
                             "(131) Aggravated Assault",
                             "(370) Pornography/Obscene Material"))
    )

# --------------------------------- ALL CRIMES ---------------------------------

# Average number of incidents involving weapon violations
la_crime %>%
  group_by(year) %>%
  summarize(mean(weapon_law_violation == TRUE, na.rm = TRUE))

# Average number of incidents involving drug violations
la_crime %>%
  group_by(year) %>%
  summarize(mean(drug_law_violation == TRUE, na.rm = TRUE))

# Average number of incidents involving drug violations
la_crime %>%
  group_by(year) %>%
  summarize(mean(drug_law_violation == TRUE, na.rm = TRUE))

# ------------------------------- YOUTH CRIMES ---------------------------------
la_crime %>%
  pull(age_17) %>%
  mean()

la_crime %>%
  filter(age_given) %>%
  pull(age_17) %>%
  mean()

la_crime %>%
  filter(age_under_18) %>%
  pull(age_17) %>%
  mean()

la_crime %>%
  filter(violent_violation) %>%
  pull(age_17) %>%
  mean()

la_crime %>%
  filter(age_given,
         violent_violation) %>%
  pull(age_17) %>%
  mean()

la_crime %>%
  filter(age_under_18,
         violent_violation) %>%
  pull(age_15) %>%
  mean()


# Average incidents involving 17 year old per year
la_crime %>%
  group_by(year) %>%
  filter(age_given) %>%
  summarize(sum(age_17 == TRUE)/n())

la_crime %>%
  group_by(year) %>%
  summarize(mean(age_17 == TRUE))

# Average incidents involving 17 year olds post RTA
la_crime %>%
  group_by(post_RTA) %>%
  filter(age_given) %>%
  summarize(mean(age_17 == TRUE))

la_crime %>%
  group_by(post_RTA) %>%
  summarize(mean(age_17 == TRUE))

la_crime %>%
  group_by(post_RTA) %>%
  filter(age_given) %>%
  summarize(mean(age_under_18 == TRUE))

la_crime %>%
  group_by(post_RTA) %>%
  summarize(mean(age_under_18 == TRUE))

# Proportion of 17 year old incidents out of all under 18 per year
la_crime %>%
  group_by(year) %>%
  filter(age_under_18) %>%
  summarize(mean(age_17 == TRUE))

# Proportion of 17 year old incidents out of all under 18 post RTA
la_crime %>%
  group_by(post_RTA) %>%
  filter(age_under_18) %>%
  summarize(mean(age_17 == TRUE))

# Youth Violent Crime per year
la_crime %>%
  group_by(year) %>%
  summarize(mean(age_under_18 == TRUE & violent_violation == TRUE))

# Youth Violent Crime post RTA
la_crime %>%
  group_by(post_RTA_violent) %>%
  summarize(mean(age_under_18 == TRUE & violent_violation == TRUE))

# 17 year old Violent Crime post RTA
la_crime %>%
  group_by(post_RTA_violent) %>%
  summarize(mean(age_17 == TRUE & violent_violation == TRUE))

la_crime %>%
  group_by(year) %>%
  summarize(length(unique(originating_agency_identifier)))

la_crime %>%
  group_by(post_RTA_violent) %>%
  summarize(mean(age_under_18 & violent_violation))

la_crime %>%
  group_by(post_RTA_violent) %>%
  filter(violent_violation,
         age_given) %>%
  summarize(mean(age_under_18))