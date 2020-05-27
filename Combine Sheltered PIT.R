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

participating_fam_all <- read_xlsx(here("raw_data/0630.xlsx"),
                                   sheet = 1,
                                   range = "a3:c37") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_ind_all <- read_xlsx(here("raw_data/0630.xlsx"),
                                   sheet = 1,
                                   range = "a40:d72") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_co_all <- read_xlsx(here("raw_data/0630.xlsx"),
                                  sheet = 1,
                                  range = "a75:c104") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_fam_vet <- read_xlsx(here("raw_data/0630.xlsx"),
                                   sheet = 2,
                                   range = "a3:c34") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_ind_vet <- read_xlsx(here("raw_data/0630.xlsx"),
                                   sheet = 2,
                                   range = "a37:d67") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_ind_youth <- read_xlsx(here("raw_data/0630.xlsx"),
                                     sheet = 3,
                                     range = "a3:d67") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_fam_youth <- read_xlsx(here("raw_data/0630.xlsx"),
                                     sheet = 3,
                                     range = "a38:c75") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))

participating_subpops <- read_xlsx(here("raw_data/0630.xlsx"),
                                   sheet = 4,
                                   range = "a4:d8") %>%
  rename("Measure" = 1) %>%
  filter(!is.na(Measure))














