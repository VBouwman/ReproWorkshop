##################################################
#### Repro workshop - fear acquisition ##########
#### Process the data ###########################
#### Author: Vera Bouwman #######################
################################################

#Load packages
library(dplyr)
library(tidyverse)
library(stringr)
library(here)

#Load data
data_acquisition <- read.csv(here("./data/processed/data_eprime.csv"))

# Select acquisition trials on expectancy outcome
acquisition_expectancy <- data_acquisition %>% 
  filter(
    Running %in% c(
    "AcqTrial1A", "AcqTrial2A", 
    "AcqTrial3A", "AcqTrial4A", 
    "AcqTrial5A", "AcqTrial6A")
    ) %>% 
  
  select(
    subject, Procedure, 
    VasScore_verwachting, CSmin1Verwachting.RESP, 
    CSmin2Verwachting.RESP, CSplusVerwachting.RESP
    ) %>% 
  
  unite(
    verwachting.RESP, CSmin1Verwachting.RESP, 
    CSmin2Verwachting.RESP, CSplusVerwachting.RESP
    ) %>% 
  
  mutate(
    verwachting.RESP = str_replace_all(verwachting.RESP, "NA_|_NA", ""), 
    verwachting.RESP = as.numeric(verwachting.RESP), 
    subject = as.numeric(subject), 
    VasScore_verwachting = as.numeric(VasScore_verwachting)
    ) %>% 
  
  mutate(
    CS = as.factor(
      ifelse(str_detect(Procedure, "CSplus"), "CS1", 
             ifelse(str_detect(Procedure, "CSmin1"), "CS2", 
                    ifelse(str_detect(Procedure, "CSminus2"), "CS3", no=NA))))
    ) %>% 
  
  group_by(subject, CS) %>% 
  
  mutate(
    trial = as.factor(seq(from = 1, to = length(subject), by = 1)), 
    Acquisitie_verwachting = ifelse(verwachting.RESP == 1, VasScore_verwachting, no = NA), 
    acq = "Acquisition_verwachting"
    ) %>% 
  
  select(
    subject, CS, trial, 
    Acquisitie_verwachting, acq
    ) %>% 
  
  unite(CS, acq, CS, trial) %>% 
  
  pivot_wider(
    names_from =  CS, values_from = Acquisitie_verwachting
    ) %>% 
  
  select(
    subject, 
    Acquisition_verwachting_CS1_1, Acquisition_verwachting_CS2_1, 
    Acquisition_verwachting_CS3_1, Acquisition_verwachting_CS1_2,
    Acquisition_verwachting_CS2_2, Acquisition_verwachting_CS3_2,
    Acquisition_verwachting_CS1_3, Acquisition_verwachting_CS2_3,
    Acquisition_verwachting_CS3_3, Acquisition_verwachting_CS1_4,
    Acquisition_verwachting_CS2_4, Acquisition_verwachting_CS3_4,
    Acquisition_verwachting_CS1_5, Acquisition_verwachting_CS2_5,
    Acquisition_verwachting_CS3_5, Acquisition_verwachting_CS1_6,
    Acquisition_verwachting_CS2_6, Acquisition_verwachting_CS3_6
    )

write_csv(acquisition_expectancy, here("./data/processed/acquisition_expectancy.csv"))

# Select acquisition trials on fear outcome
acquisition_fear <-data_acquisition %>% 
  filter(
    Running %in% c(
      "AcqTrial1A", "AcqTrial2A", 
      "AcqTrial3A", "AcqTrial4A", 
      "AcqTrial5A", "AcqTrial6A")
    ) %>% 
  
  select(
    subject, Procedure, 
    VasScore_Angst, CSmin1Angst.RESP, 
    CSmin2Angst.RESP,CSplusAngst.RESP
    ) %>% 
  
  unite(
    Angst.RESP, CSmin1Angst.RESP, 
    CSmin2Angst.RESP,CSplusAngst.RESP
    ) %>% 
  
  mutate(
    Angst.RESP = str_replace_all(Angst.RESP, "NA_|_NA", ""), 
    Angst.RESP = as.numeric(Angst.RESP), subject = as.numeric(subject), 
    VasScore_Angst = as.numeric(VasScore_Angst)
    ) %>% 
  
  mutate(
    CS = as.factor(
      ifelse(str_detect(Procedure, "CSplus"), "CS1", 
             ifelse(str_detect(Procedure, "CSmin1"), "CS2", 
                    ifelse(str_detect(Procedure, "CSminus2"), "CS3", no=NA))))
    ) %>%
  
  group_by(subject, CS) %>% 
  
  mutate(
    trial = as.factor(seq(from = 1, to = length(subject), by = 1)), 
    Acquisitie_angst = ifelse(Angst.RESP == 1, VasScore_Angst, no = NA), 
    acq = "Acquisition_angst"
    ) %>%
  
  select(
    subject, CS, trial, 
    Acquisitie_angst, acq
    )%>% 
  
  unite(CS, acq, CS, trial) %>% 
  
  pivot_wider(
    names_from =  CS, 
    values_from = Acquisitie_angst
    ) %>% 

  select(
    subject, 
    Acquisition_angst_CS1_1, Acquisition_angst_CS2_1, 
    Acquisition_angst_CS3_1, Acquisition_angst_CS1_2,
    Acquisition_angst_CS2_2, Acquisition_angst_CS3_2, 
    Acquisition_angst_CS1_3, Acquisition_angst_CS2_3,
    Acquisition_angst_CS3_3, Acquisition_angst_CS1_4,
    Acquisition_angst_CS2_4, Acquisition_angst_CS3_4,
    Acquisition_angst_CS1_5, Acquisition_angst_CS2_5,
    Acquisition_angst_CS3_5, Acquisition_angst_CS1_6,
    Acquisition_angst_CS2_6, Acquisition_angst_CS3_6
    )

write_csv(acquisition_fear, here("./data/processed/acquisition_fear.csv"))