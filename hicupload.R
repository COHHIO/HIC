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
library(readxl)

# create two directories: output_data and raw_data. Save the HIC csvs from the 
# CSV Export into the raw_data folder. Replace as you correct things and rerun 
# the export 

# Project file ------------------------------------------------------------
project <- read_csv("raw_data/Project.csv") %>%
  filter(!is.na(ProjectType) & ProjectType %in% c(1:3, 8:11, 13))

# The PIT Count data doesn't come in via the HUD CSV Export in ServicePoint.
# As such, this data comes from a script I wrote on HMIS_COHHIO
# since there's Enrollment data over there. It's a .csv file in the Reports
# folder. Just copy it into the raw_data folder in this project.
pit <- read_csv("raw_data/PIT2020.csv")

project <- project %>% select(-PITCount) %>%
  left_join(., pit, by = "ProjectID") %>%
  mutate(ProjectName = if_else(is.na(ProjectCommonName), 
                               ProjectName, 
                               ProjectCommonName),
         PITCount = if_else(is.na(PITCount), 0, PITCount))

rm(pit)

write_csv(project, "output_data/Project.csv", 
          na = "",  
          quote_escape = "backslash")

# Organization file -------------------------------------------------------
organization <- read_csv("raw_data/Organization.csv") 

write_csv(organization, "output_data/Organization.csv", 
          na = "",  
          quote_escape = "backslash")

# Inventory file ----------------------------------------------------------
inventory <- read_csv("raw_data/Inventory.csv") 

write_csv(inventory, "output_data/Inventory.csv", 
          na = "",  
          quote_escape = "backslash")

# ProjectCoC file ----------------------------------------------------------
geography <- read_csv("raw_data/ProjectCoC.csv")
write_csv(geography, "output_data/ProjectCoC.csv", 
          na = "",  
          quote_escape = "backslash")

# Funder file -------------------------------------------------------------
funder <- read_csv("raw_data/Funder.csv") %>%
  filter(!is.na(Funder) &
           Funder != 46) # SHOULD NOT BE NECESSARY, HDX PROBLEM
write_csv(funder, "output_data/Funder.csv", 
          na = "",  
          quote_escape = "backslash")




