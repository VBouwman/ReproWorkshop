#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
# STANDARDIZED SCRIPT TO READ AND COMBINE INDIVIDUAL E-PRIME TEXT FILES (.TXT)
#
# Created by Stefan Vermeent
# P.C.S.Vermeent@uu.nl

# INSTRUCTIONS:
#
# This script can be used to read in all individual E-prime .txt files and automatically combine them into one
# dataframe. Make sure the working directory of the script is set to the folder that contains all files. The 
# script consists of three parts:

# 1. This is the only part in the script that has to be changed manually. Specify the E-prime levels and trial
#    levels to retain, and (optionally) which variables to include in the final dataset.
# 2. This part consists of a function that translates the .txt files to a tidy dataframe, and a loop that 
#    applies this function to all the .txt files. In the final step, they all get combined into one dataframe.
# 3. An optional part if you want to filter out additional variables. These variables can be specified under
#    step 1.
#    
#------------------------------------------------------------------------------------------------------------
#------------------------------------------------------------------------------------------------------------
library(rprime)
library(tidyverse)
library(stringr)
library(here)

#------------------------------------------------------------------------------------------------------------
##1. EXPERIMENT-SPECIFIC PARAMETERS THAT NEED TO BE SPECIFIED##
#------------------------------------------------------------------------------------------------------------

# Specify which nested levels you want to keep in the final dataset.
# NOTE: If you are not sure, you can first run the script for one file. the preview_levels() function in 
# read_files prints an overview of the levels and the corresponding trial types.
levels_to_retain_study3 <- c(1,2,3,5)

# Specify which trials you want to keep (e.g., here you could drop the instructions or practice trials)
trials_to_retain_study3 <- c("Header","PracticeVAS", "HabTrials", "Habituation", "AcqTrial1A", "AcqTrial2A", "AcqTrial3A", "AcqTrial4A", 
                             "AcqTrial5A", "AcqTrial6A", "SelectCS2CS3", "Test", "Acquisition", "Involuntary", "Demog", "QuestionsExit")


# Optional for step 3: standard E-prime vectors and experiment-specific vectors that you want to filter out or retain:

drop_variables <- c()  

keep_variables_study3 <- c("subject", "Eprime.LevelName", "Sample", "Cycle", "Running", "Procedure", "ITI", "CSplus", "CSmin1", "CSmin2",
                           "VasScore_verwachting", "VasScore_Angst", "VasScore_PreCS1", "VasScore_PreCS2", "VasScore_PreCS3",
                           "VasScore_PostCS1", "VasScore_PostCS2", "VasScore_PostCS3", "VasScore_FilmOnaangenaam", "VasScore_CueOnaangenaam",
                           "OefenVASverwachting.RT", "OefenVASverwachting.RESP" ,"OefenVASAngst.RT", "OefenVASAngst.RESP", "CS1VAS.RT", "CS1VAS.RESP",
                           "CS2VAS.RT", "CS2VAS.RESP", "CS3VAS.RT", 'CS3VAS.RESP', "CSmin1Verwachting.RT", "CSmin1Verwachting.RESP",
                           "CSmin1Angst.RT", "CSmin1Angst.RESP", "CSmin2Verwachting.RT", "CSmin2Verwachting.RESP", "CSmin2Angst.RT", "CSmin2Angst.RESP",
                           "CSplusVerwachting.RT", "CSplusVerwachting.RESP", "CSplusAngst.RT", "CSplusAngst.RESP", "CSplusVerwachting1.RT", "CSplusVerwachting1.RESP",
                           "CSplusAngst1.RT", "CSplusAngst1.RESP", "VasScore_FilmOnaangenaam", "VasScore_CueOnaangenaam", "Geslacht.RESP", "Leeftijd.RESP")


#------------------------------------------------------------------------------------------------------------
## 2. PARSING AND JOINING DATA##
#------------------------------------------------------------------------------------------------------------

# 'file_names' contains all the individual textfiles that are located in the working directory. NOTE: ".txt" 
# doesn't have to be in the filename for this code to work
file_names_study3 <- list.files(path = here("./data/raw/"),pattern = "*.txt")

# A function that goes through all the steps to transform the text file into a tidy dataframe.
read_files <- function(x, y) {
  parse_data <- read_eprime(x) %>%
    FrameList()
  
  preview_levels(parse_data)
  
  individual_data_study3 <- keep_levels(parse_data, levels_to_retain_study3) %>%
    filter_in(., "Running", trials_to_retain_study3) %>%
    to_data_frame() 
  

  individual_tidy_study3 <- individual_data_study3 %>%
    # Make sure every trial line contains the subject number...
    mutate(subject = ifelse(is.na(Subject), Subject[1], Subject)) %>%
    # And drop the original 'Subject' vector...
    select(-Subject) %>%
    # And move the subject vector to the front of the dataset
    .[,c(max(ncol(.)),1:(ncol(.)-1))]
}

# A loop that makes sure that the read_files function is applied to all the individual datafiles in the working directory
output <- vector("list", length(file_names_study3)) 
for (i in seq_along(file_names_study3)) {
  output[[i]] <- read_files(paste0(here("./data/raw/"), file_names_study3[[i]]), full_data)
}

# Combine the individual datasets created in the loop into one full dataset.
data_eprime_study3 <- bind_rows(output)



#------------------------------------------------------------------------------------------------------------
# 3. (OPTIONAL) FILTER RELEVANT VECTORS##
#------------------------------------------------------------------------------------------------------------

tidy_data_eprime_study3 <- data_eprime_study3 %>%
  dplyr::select(one_of(keep_variables_study3)) 

# Save data as CSV file
write_csv(tidy_data_eprime_study3, here("./data/processed/data_eprime_study3.csv"))


