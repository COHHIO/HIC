# COHHIO_HMIS
# Copyright (C) 2019  Coalition on Homelessness and Housing in Ohio (COHHIO)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>.

library(tidyverse)
library(lubridate)
library(readxl)
library(here)

non_participating_es <- 
  read_csv(here("raw_data/ES_HUD_Point_in_Time_Report.csv"))

non_participating_th <- 
  read_csv(here("raw_data/TH_HUD_Point_in_Time_Report.csv"))

non_participating <- rbind(non_participating_es, non_participating_th) 

rm(non_participating_es, non_participating_th)


# All Fam -----------------------------------------------------------------

participating_fam_all <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 1,
            range = "a3:c37") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_fam_all_es <- participating_fam_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_fam_all_th <- participating_fam_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_fam_all <- 
  rbind(participating_fam_all_es, participating_fam_all_th)

rm(participating_fam_all_es, participating_fam_all_th)


# All Individual ----------------------------------------------------------

participating_ind_all <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 1,
            range = "a40:d72") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 


# All Children-Only -------------------------------------------------------

participating_co_all <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 1,
            range = "a75:c104") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 


# Vet Fam -----------------------------------------------------------------

participating_fam_vet <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 2,
            range = "a3:c34") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 


# Vet IND -----------------------------------------------------------------

participating_ind_vet <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 2,
            range = "a37:d67") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

# Youth IND ---------------------------------------------------------------

participating_ind_youth <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 3,
            range = "a3:d67") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

# Youth FAM ---------------------------------------------------------------

participating_fam_youth <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 3,
            range = "a38:c75") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

# Subpops -----------------------------------------------------------------

participating_subpops <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 4,
            range = "a4:d8") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHwChildHouseholds",
      rowname == 2 ~ "HHwChildTotalPersons",
      rowname == 3 ~ "HHwChildUnder18",
      rowname == 4 ~ "HHwChild18to24",
      rowname == 5 ~ "HHwChildOver24",
      rowname == 6 ~ "HHwChildMissingDOB",
      rowname == 9 ~ "HHwChildFemale",
      rowname == 10 ~ "HHwChildMale",
      rowname == 11 ~ "HHwChildTransgender",
      rowname == 12 ~ "HHwChildNonConforming",
      rowname == 13 ~ "HHwChildGenderDKR",
      rowname == 14 ~ "HHwChildGenderMissing",
      rowname == 17 ~ "HHwChildNonHispanic",
      rowname == 18 ~ "HHwChildHispanic",
      rowname == 19 ~ "HHwChildEthnicityDKR",
      rowname == 20 ~ "HHwChildEthnicityMissing",
      rowname == 23 ~ "HHwChildRaceWhite",
      rowname == 24 ~ "HHwChildRaceBlack",
      rowname == 25 ~ "HHwChildRaceAsian",
      rowname == 26 ~ "HHwChildRaceAmIndAkNat",
      rowname == 27 ~ "HHwChildRaceNatHawaiiPacIsland",
      rowname == 28 ~ "HHwChildRaceMultiple",
      rowname == 29 ~ "HHwChildRaceDKR",
      rowname == 30 ~ "HHwChildRaceMissing",
      rowname == 33 ~ "HHwChildCHHouseholds",
      rowname == 34 ~ "HHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 














