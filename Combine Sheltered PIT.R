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
      rowname == 1 ~ "HHwoChildHouseholds",
      rowname == 2 ~ "HHwoChildTotalAdults",
      rowname == 3 ~ "HHwoChild18to24",
      rowname == 4 ~ "HHwoChildOver24",
      rowname == 5 ~ "HHwoChildMissingDOB",
      rowname == 8 ~ "HHwoChildFemale",
      rowname == 9 ~ "HHwoChildMale",
      rowname == 10 ~ "HHwoChildTransgender",
      rowname == 11 ~ "HHwoChildNonConforming",
      rowname == 12 ~ "HHwoChildGenderDKR",
      rowname == 13 ~ "HHwoChildGenderMissing",
      rowname == 16 ~ "HHwoChildNonHispanic",
      rowname == 17 ~ "HHwoChildHispanic",
      rowname == 18 ~ "HHwoChildEthnicityDKR",
      rowname == 19 ~ "HHwoChildEthnicityMissing",
      rowname == 22 ~ "HHwoChildRaceWhite",
      rowname == 23 ~ "HHwoChildRaceBlack",
      rowname == 24 ~ "HHwoChildRaceAsian",
      rowname == 25 ~ "HHwoChildRaceAmIndAkNat",
      rowname == 26 ~ "HHwoChildRaceNatHawaiiPacIsland",
      rowname == 27 ~ "HHwoChildRaceMultiple",
      rowname == 28 ~ "HHwoChildRaceDKR",
      rowname == 29 ~ "HHwoChildRaceMissing",
      rowname == 32 ~ "HHwoChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_ind_all_es <- participating_ind_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_ind_all_th <- participating_ind_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_ind_all_sh <- participating_ind_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Safe Haven")

participating_ind_all <- 
  rbind(participating_ind_all_es,
        participating_ind_all_th,
        participating_ind_all_sh)

rm(participating_ind_all_es, participating_ind_all_th, participating_ind_all_sh)

# All Children-Only -------------------------------------------------------

participating_co_all <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 1,
            range = "a75:c104") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "HHChildOnlyHouseholds",
      rowname == 2 ~ "HHChildOnlyTotalChildren",
      rowname == 5 ~ "HHChildOnlyFemale",
      rowname == 6 ~ "HHChildOnlyMale",
      rowname == 7 ~ "HHChildOnlyTransgender",
      rowname == 8 ~ "HHChildOnlyNonConforming",
      rowname == 9 ~ "HHChildOnlyGenderDKR",
      rowname == 10 ~ "HHChildOnlyGenderMissing",
      rowname == 13 ~ "HHChildOnlyNonHispanic",
      rowname == 14 ~ "HHChildOnlyHispanic",
      rowname == 15 ~ "HHChildOnlyEthnicityDKR",
      rowname == 16 ~ "HHChildOnlyEthnicityMissing",
      rowname == 19 ~ "HHChildOnlyRaceWhite",
      rowname == 20 ~ "HHChildOnlyRaceBlack",
      rowname == 21 ~ "HHChildOnlyRaceAsian",
      rowname == 22 ~ "HHChildOnlyRaceAmIndAkNat",
      rowname == 23 ~ "HHChildOnlyRaceNatHawaiiPacIsland",
      rowname == 24 ~ "HHChildOnlyRaceMultiple",
      rowname == 25 ~ "HHChildOnlyRaceDKR",
      rowname == 26 ~ "HHChildOnlyRaceMissing",
      rowname == 29 ~ "HHChildOnlyCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_co_all_es <- participating_co_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_co_all_th <- participating_co_all %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_co_all <- 
  rbind(participating_co_all_es, participating_co_all_th)

rm(participating_co_all_es, participating_co_all_th)

# Vet Fam -----------------------------------------------------------------

participating_fam_vet <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 2,
            range = "a3:c34") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "VetHHwChildHouseholds",
      rowname == 2 ~ "VetHHwChildTotalPersons",
      rowname == 3 ~ "VetHHwChildVets",
      rowname == 6 ~ "VetHHwChildFemale",
      rowname == 7 ~ "VetHHwChildMale",
      rowname == 8 ~ "VetHHwChildTransgender",
      rowname == 9 ~ "VetHHwChildNonConforming",
      rowname == 10 ~ "VetHHwChildGenderDKR",
      rowname == 11 ~ "VetHHwChildGenderMissing",
      rowname == 14 ~ "VetHHwChildNonHispanic",
      rowname == 15 ~ "VetHHwChildHispanic",
      rowname == 16 ~ "VetHHwChildEthnicityDKR",
      rowname == 17 ~ "VetHHwChildEthnicityMissing",
      rowname == 20 ~ "VetHHwChildRaceWhite",
      rowname == 21 ~ "VetHHwChildRaceBlack",
      rowname == 22 ~ "VetHHwChildRaceAsian",
      rowname == 23 ~ "VetHHwChildRaceAmIndAkNat",
      rowname == 24 ~ "VetHHwChildRaceNatHawaiiPacIsland",
      rowname == 25 ~ "VetHHwChildRaceMultiple",
      rowname == 26 ~ "VetHHwChildRaceDKR",
      rowname == 27 ~ "VetHHwChildRaceMissing",
      rowname == 30 ~ "VetHHwChildCHHouseholds",
      rowname == 31 ~ "VetHHwChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_fam_vet_es <- participating_fam_vet %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_fam_vet_th <- participating_fam_vet %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_fam_vet <- 
  rbind(participating_fam_vet_es, participating_fam_vet_th)

rm(participating_fam_vet_es, participating_fam_vet_th)

# Vet IND -----------------------------------------------------------------

participating_ind_vet <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 2,
            range = "a37:d67") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "VetHHwoChildHouseholds",
      rowname == 2 ~ "VetHHwoChildTotalPersons",
      rowname == 3 ~ "VetHHwoChildVets",
      rowname == 6 ~ "VetHHwoChildFemale",
      rowname == 7 ~ "VetHHwoChildMale",
      rowname == 8 ~ "VetHHwoChildTransgender",
      rowname == 9 ~ "VetHHwoChildNonConforming",
      rowname == 10 ~ "VetHHwoChildGenderDKR",
      rowname == 11 ~ "VetHHwoChildGenderMissing",
      rowname == 14 ~ "VetHHwoChildNonHispanic",
      rowname == 15 ~ "VetHHwoChildHispanic",
      rowname == 16 ~ "VetHHwoChildEthnicityDKR",
      rowname == 17 ~ "VetHHwoChildEthnicityMissing",
      rowname == 20 ~ "VetHHwoChildRaceWhite",
      rowname == 21 ~ "VetHHwoChildRaceBlack",
      rowname == 22 ~ "VetHHwoChildRaceAsian",
      rowname == 23 ~ "VetHHwoChildRaceAmIndAkNat",
      rowname == 24 ~ "VetHHwoChildRaceNatHawaiiPacIsland",
      rowname == 25 ~ "VetHHwoChildRaceMultiple",
      rowname == 26 ~ "VetHHwoChildRaceDKR",
      rowname == 27 ~ "VetHHwoChildRaceMissing",
      rowname == 30 ~ "VetHHwoChildCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_ind_vet_es <- participating_ind_vet %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_ind_vet_th <- participating_ind_vet %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_ind_vet_sh <- participating_ind_vet %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = `Safe Haven`
  ) %>%
  mutate(ProjectType = "Safe Haven")

participating_ind_vet <- 
  rbind(participating_ind_vet_es, 
        participating_ind_vet_th,
        participating_ind_vet_sh)

rm(participating_ind_vet_es, participating_ind_vet_th, participating_ind_vet_sh)

# Youth IND ---------------------------------------------------------------

participating_ind_youth <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 3,
            range = "a3:d35") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "UYouthHouseholds",
      rowname == 2 ~ "UYouthTotalYouth",
      rowname == 3 ~ "UYouthUnder18",
      rowname == 4 ~ "UYouth18to24",
      rowname == 7 ~ "UYouthFemale",
      rowname == 8 ~ "UYouthMale",
      rowname == 9 ~ "UYouthTransgender",
      rowname == 10 ~ "UYouthNonConforming",
      rowname == 11 ~ "UYouthGenderDKR",
      rowname == 12 ~ "UYouthGenderMissing",
      rowname == 15 ~ "UYouthNonHispanic",
      rowname == 16 ~ "UYouthHispanic",
      rowname == 17 ~ "UYouthEthnicityDKR",
      rowname == 18 ~ "UYouthEthnicityMissing",
      rowname == 21 ~ "UYouthRaceWhite",
      rowname == 22 ~ "UYouthRaceBlack",
      rowname == 23 ~ "UYouthRaceAsian",
      rowname == 24 ~ "UYouthRaceAmIndAkNat",
      rowname == 25 ~ "UYouthRaceNatHawaiiPacIsland",
      rowname == 26 ~ "UYouthRaceMultiple",
      rowname == 27 ~ "UYouthRaceDKR",
      rowname == 28 ~ "UYouthRaceMissing",
      rowname == 31 ~ "UYouthCHHouseholds",
      rowname == 32 ~ "UYouthCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_ind_youth_es <- participating_ind_youth %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_ind_youth_th <- participating_ind_youth %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_ind_youth_sh <- participating_ind_youth %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = `Safe Haven`
  ) %>%
  mutate(ProjectType = "Safe Haven")

participating_ind_youth <- 
  rbind(participating_ind_youth_es, 
        participating_ind_youth_th, 
        participating_ind_youth_sh)

rm(participating_ind_youth_es, 
   participating_ind_youth_th, 
   participating_ind_youth_sh)

# Youth FAM ---------------------------------------------------------------

participating_fam_youth <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 3,
            range = "a38:c75") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "ParentYouthHouseholds",
      rowname == 2 ~ "ParentYouthTotalPersons",
      rowname == 3 ~ "ParentYouthParentingYouth",
      rowname == 4 ~ "ParentYouthChildren",
      rowname == 5 ~ "ParentYouthUnder18",
      rowname == 6 ~ "ParentYouthUnder18Children",
      rowname == 7 ~ "ParentYouth18to24",
      rowname == 8 ~ "ParentYouth18to24Children",
      rowname == 9 ~ "ParentYouthMissingHoH",
      rowname == 12 ~ "ParentYouthFemale",
      rowname == 13 ~ "ParentYouthMale",
      rowname == 14 ~ "ParentYouthTransgender",
      rowname == 15 ~ "ParentYouthNonConforming",
      rowname == 16 ~ "ParentYouthGenderDKR",
      rowname == 17 ~ "ParentYouthGenderMissing",
      rowname == 20 ~ "ParentYouthNonHispanic",
      rowname == 21 ~ "ParentYouthHispanic",
      rowname == 22 ~ "ParentYouthEthnicityDKR",
      rowname == 23 ~ "ParentYouthEthnicityMissing",
      rowname == 26 ~ "ParentYouthRaceWhite",
      rowname == 27 ~ "ParentYouthRaceBlack",
      rowname == 28 ~ "ParentYouthRaceAsian",
      rowname == 29 ~ "ParentYouthRaceAmIndAkNat",
      rowname == 30 ~ "ParentYouthRaceNatHawaiiPacIsland",
      rowname == 31 ~ "ParentYouthRaceMultiple",
      rowname == 32 ~ "ParentYouthRaceDKR",
      rowname == 33 ~ "ParentYouthRaceMissing",
      rowname == 36 ~ "ParentYouthCHHouseholds",
      rowname == 37 ~ "ParentYouthCHPersons",
      TRUE ~ Measure
    )
  ) %>%
  filter(!is.na(Emergency)) %>%
  select(-rowname) 

participating_fam_youth_es <- participating_fam_youth %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_fam_youth_th <- participating_fam_youth %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_fam_youth <- 
  rbind(participating_fam_youth_es, participating_fam_youth_th)

rm(participating_fam_youth_es, participating_fam_youth_th)

# Subpops -----------------------------------------------------------------

participating_subpops <-
  read_xlsx(here("raw_data/0630.xlsx"),
            sheet = 4,
            range = "a4:d8") %>%
  rename("Measure" = 1) %>%
  rownames_to_column() %>%
  mutate(
    Measure = case_when(
      rowname == 1 ~ "SubPopSeriousMentalIllness",
      rowname == 2 ~ "SubPopSubstanceAbuse",
      rowname == 3 ~ "SubPopHIVAIDS",
      rowname == 4 ~ "SubPopDomesticViolence",
      TRUE ~ Measure
    )
  ) %>%
  select(-rowname) 

participating_subpops_es <- participating_subpops %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Emergency
  ) %>%
  mutate(ProjectType = "Emergency Shelter")

participating_subpops_th <- participating_subpops %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = Transitional
  ) %>%
  mutate(ProjectType = "Transitional Housing")

participating_subpops_sh <- participating_subpops %>%
  pivot_wider(
    id_cols = Measure,
    names_from = Measure,
    values_from = `Safe Haven`
  ) %>%
  mutate(ProjectType = "Safe Haven")

participating_subpops <- 
  rbind(participating_subpops_es, 
        participating_subpops_th, 
        participating_subpops_sh)

rm(participating_subpops_es, participating_subpops_th, participating_subpops_sh)

# Tying things together ---------------------------------------------------

participating <- participating_co_all %>%
  full_join(participating_fam_all, by = "ProjectType") %>%
  full_join(participating_fam_vet, by = "ProjectType") %>%
  full_join(participating_fam_youth, by = "ProjectType") %>%
  full_join(participating_ind_all, by = "ProjectType") %>%
  full_join(participating_ind_vet, by = "ProjectType") %>%
  full_join(participating_ind_youth, by = "ProjectType") %>%
  full_join(participating_subpops, by = "ProjectType")







