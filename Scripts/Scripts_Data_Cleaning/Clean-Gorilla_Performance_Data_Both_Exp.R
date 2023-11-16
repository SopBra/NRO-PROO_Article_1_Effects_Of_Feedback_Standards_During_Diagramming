############################################################################  --
##                  Data Cleaning Performance Data Study 1:
##          Effects on Monitoring Accuracy and Text Comprehension
##                           Sophia Braumann                                  
## -------------------------------------------------------------------------- --
##
##           !! COLLAPSE ALL LINES TO BETTER NAVIGATE THIS SCRIPT !!
##            (Edit->Folding->Collapse All / alt+cmd+o/ Ctrl+Alt+o)
##
## -------------------------------------------------------------------------- --
## 0. Set-Up                                                                  ----
##    (Install if necessary and) load relevant libraries                      ----
#  Check and perform installation of relevant packages if needed
list_of_packages <- c("tidyverse", # distinct, filter, %>%, and many more
                      "data.table", # to read in all raw data --> alternative to vroom
                      "lubridate"  # to convert the time and date variable
                      )  

# List of relevant packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])] # Check whether all packages are installed
if(length(new_packages)) install.packages(new_packages, type = "binary") # Install them if that is not the case
## Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)

##    Alert about R and package versions                                      ----
## Retrieve the session info of the user for comparing R and package versions
session_info_user <- sessionInfo()

## Compare the R versions
r_version_user <- session_info_user$R.version$version.string
# The author used R version 4.3.0, check if the user uses the same version
paste0(ifelse(grepl('4.3.1', r_version_user),
              'You are using the same version of R under which this script was created. You are good to go :)',
              'You are using a different version of R than the one under which this script was created. 
              Please report malfunctioning of the script based on outdated packages/functions'))

## Compare the package versions
# Load the package versions used by the author at the time of script creation
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_author_cleaning_script_performance_data.txt')

# Function to retrieve the package versions of the user
retrieve_package_versions <- function(incoming_package_info) {
  package_version <- incoming_package_info$Version
  package_version
}

# Retrieve the package versions of the user
package_infos_user <- lapply(session_info_user$otherPkgs, function(x) retrieve_package_versions(x))
package_infos_user<- data.frame(t(sapply(package_infos_user,cbind))) %>% 
  pivot_longer(cols = everything(), names_to = 'Package', values_to = 'Version')
## Only compare packages that are loaded in this script
merged_package_infos <- left_join(package_infos_at_script_creation, 
                                  package_infos_user, by = join_by(Package), 
                                  suffix = c("_Author", "_User"))

# Compare the all the package versions
package_version_altert <- ifelse(all(merged_package_infos$Version_Author == merged_package_infos$Version_User),
                                 paste0('All packages are loaded in the same version as at time of the script creation, you are good to go :)'),
                                 paste0('One or more packages are loaded at a different version on your system compared to the moment of the script creation. If you encounter malfuncting of the script due to outdata package versions, please report back to the author.'))

package_version_altert
merged_package_infos
rm(session_info_user)

## -------------------------------------------------------------------------- --
##    Relevant functions that will be used across the script                  ----
# A function that returns all files in a directory by taking 
#   the path to the directory, a pattern of relevant filenames and a file type
load_all_raw_data_files <- function(incoming_path, relevant_pattern, file_type) {
  # Collect all file names that should be loaded
  filenames <- list.files(path = incoming_path, 
                          pattern = paste0(relevant_pattern,"(.)*.",file_type,"$")) 
  # Load the files by adding the path to the directory to the file names,
  file_frame <- sapply(filenames, function(x) paste0(incoming_path, x), USE.NAMES = FALSE) %>%
    # reading in all columns as characters (to avoid issues with different types) 
    # and collapse all loaded data frames into one
    map_df(~fread(., fill = TRUE, colClasses = 'character')) %>% 
    type.convert(as.is=TRUE) # Convert the columns types back to what they contain
}


# A function to remove white spaces from the variable names
remove_white_spaces <- function(incoming_data) {
  # Remove all white spaces from the variable names and replace them with a _
  names(incoming_data) <- gsub(" ", "_", names(incoming_data), fixed = TRUE)
  # Do the same for the - separator 
  names(incoming_data) <- gsub("-", "_", names(incoming_data), fixed = TRUE)
  incoming_data
}

# A function to clean both data sets of Experiment 1 and 2
clean_gorilla_data <- function(incoming_data) {
  # Remove the white spaces with the remove_white_spaces function above
  incoming_data <- remove_white_spaces(incoming_data)
  # Exclude introduction video files and practice trial files
  incoming_data <- incoming_data %>% filter(!grepl('Alice', Spreadsheet))
  incoming_data <- incoming_data %>% filter(!grepl('Sporten', Spreadsheet))
  incoming_data <- incoming_data %>% filter(!grepl('Sporten', display))
  # Also remove EventIndex == 999 (END OF FILE)
  incoming_data <- incoming_data %>% filter(!Event_Index == "END OF FILE")
  # Also remove BEGIN TASK and END TASK 
  incoming_data <- incoming_data %>% filter(Trial_Number != "BEGIN TASK" | Trial_Number != "END TASK")
  # Remove all empty columns/variables (variables Gorilla added automatically)
  incoming_data <- incoming_data %>% ungroup(.) %>% discard(~all(is.na(.) | . ==""))
  # Remove "Attempt" as it seems to be coded differently across experiments by Gorilla and is not relevant
  incoming_data <- incoming_data %>% select(-Attempt)
  incoming_data <- incoming_data %>% type.convert(as.is=TRUE)
  # Transform Local_Date into a year-month-day variable
  incoming_data <- incoming_data %>% mutate(Local_Date = dmy_hms(Local_Date)) 
}

# Rename levels of Condition (preferably adapt in data cleaning)
rename_conditions <- function(Condition){
  # (And add more spaces where necessary)
  Condition <- gsub(Condition, pattern = "DiagramFeedback", replacement = "Diagramming+Standard")
  Condition <- gsub(Condition, pattern = "PictureMatching", replacement = "No-Diagram-Control")
  Condition <- gsub(Condition, pattern = "PictureMapping", replacement = "No-Diagram-Control")
  Condition <- gsub(Condition, pattern = "DiagramNoStandard", replacement = "Diagramming-Only")
  Condition <- gsub(Condition, pattern = "StandardOnly", replacement = "Standard-Only")
  Condition <- gsub(Condition, pattern = "DiagramNoFeedback", replacement = "Diagramming-Only")
  Condition <- gsub(Condition, pattern = "FeedbackOnly", replacement = "Standard-Only")
  Condition <- gsub(Condition, pattern = "DiagramStandard", replacement = "Diagramming+Standard")
}

##    Set the paths to the required data                                      ----
## Save paths to the different directories where the data is stored

# To the directory of the real data of Experiment 1 and 2:
rel_path_raw_data_study_1 <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Performance_Raw_Data/Study_1/"
rel_path_raw_data_study_2 <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Performance_Raw_Data/Study_2/"

# To the ID key file
rel_path_id_file <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Performance_Raw_Data/"

# Directory for the output files
rel_path_for_output <- "Output/Data_Cleaning_Scripts/Study_1_Gorilla_Performance_Data/"
## -------------------------------------------------------------------------- ----
## 1. Load the raw data files for the experiments in the defined directories  ----
# Use the function "load_all_raw_data_files" above to read-in the different task and questionnaire files
all_data_study_1 <- load_all_raw_data_files(rel_path_raw_data_study_1, "task", "csv")
#questionnaires_study_1 <- load_all_raw_data_files(rel_path_raw_data_study_1, "questionnaire", "csv")  
all_data_study_2 <- load_all_raw_data_files(rel_path_raw_data_study_2, "task", "csv")
all_IDs_random_to_random <- read_delim(paste0(rel_path_id_file, 'all_IDs_random_to_random.txt'))

##    Load the data from the specified directories above                      ----
# First load them all together into a list (and exclude the path from entry name)
#txt_raw_data_list <- sapply(paste0(rel_path_raw_data, selected_txt_filenames_raw_data), fread, fill=TRUE)
#names(txt_raw_data_list) <- str_extract(names(txt_raw_data_list), selected_txt_filenames_raw_data)
#names(txt_raw_data_list) <- gsub(".txt","", names(txt_raw_data_list))
## Then save all list entries/ files as different objects
#list2env(txt_raw_data_list, .GlobalEnv)

## -------------------------------------------------------------------------- --
## 2. Preliminary Data Cleaning                                               ----
##    (Remove white spaces, introduction videos, practice trials, etc.)       ----
## Call the function to the data sets
all_data_study_1 <- clean_gorilla_data(all_data_study_1)
all_data_study_2 <- clean_gorilla_data(all_data_study_2)

##    Add conditions                                                          ----
# to Experiment 1 by renaming the randomizer.. 
all_data_study_1 <- all_data_study_1 %>% rename(Condition = randomiser_81rl)

all_data_study_1$Condition <- rename_conditions(all_data_study_1$Condition)
all_data_study_1$Spreadsheet <- rename_conditions(all_data_study_1$Spreadsheet)
all_data_study_1$Spreadsheet_Name <- rename_conditions(all_data_study_1$Spreadsheet_Name)
all_data_study_1$Screen_Name <- rename_conditions(all_data_study_1$Screen_Name)
# And to Study 2 by adding condition with the respective level as there was only one
all_data_study_2$Condition <- 'Diagramming+Standard'
 
##    Add trial names                                                         ----
# First provide a vector with all trial names
my_trial_names <- c("Beton|Metro|Botox|Muziek|Suez|Geld|ReadyForNext")

# For experiment 1: Simply extract the provided trial names from "display"
all_data_study_1 <- all_data_study_1 %>% 
  mutate(Trial = str_extract(display, my_trial_names)) %>% 
  relocate(Trial, .after = Participant_Private_ID) %>% 
  mutate(Participant = Participant_Private_ID) %>% 
  relocate(Participant) %>% 
  filter(!(is.na(Participant) | is.na(Participant_Private_ID)))

# For experiment/study 2: By creating a trial_id_matching frame first, then merge
trial_id_matching_study_2 <- all_data_study_2 %>% 
  filter(!is.na(Participant_Private_ID)) %>%
  mutate(Trial = str_extract(display, my_trial_names)) %>%
  filter(grepl(my_trial_names, Trial)) %>%
  distinct(Participant_Private_ID, Trial) 
# Now merge that frame back to the data of experiment 2
all_data_study_2 <- right_join(trial_id_matching_study_2, all_data_study_2) %>% 
  relocate(Trial, .after = Participant_Private_ID)

##    Assign the overall Participant ID to the trials of Experiment 2         ----
# First kick out the columns from Study 1
all_IDs_random_to_random <- all_IDs_random_to_random %>% 
  filter(Experiment == 2) %>%
  select(!ends_with('_Random'),-Experiment)

all_data_study_2 <- all_data_study_2 %>% 
  ungroup() %>% 
  rename(Participant_ID = Participant_Private_ID) %>%
  right_join(all_IDs_random_to_random, .) %>% 
  filter(!(is.na(Participant) | is.na(Participant_ID)))

## There are also some dummy trials among the data of the second experiment 
# Recognizable as completed with lorem ipsum reponses 
dummy_trials_to_kick <- all_data_study_2 %>% filter(grepl('Lorem ipsum', Response))
all_data_study_2 <- all_data_study_2 %>% 
  filter(!(Participant_ID %in% dummy_trials_to_kick$Participant_ID))

## 3. Merge data of Study 1 and 2  and do some more formatting and cleaning   ----
# First create a new variable indicating to which experiment the row data belongs
all_data_study_1 <- all_data_study_1 %>% mutate(Experiment = 1) %>% 
  rename(Participant_ID = Participant_Private_ID) %>% 
  relocate(Experiment) # not specifying positions relocates the variable to the front
all_data_study_2 <- all_data_study_2 %>% 
  mutate(Experiment = 2) %>% relocate(Experiment)

# Merge the the data sets
all_data <- full_join(all_data_study_1, all_data_study_2) %>% 
  relocate(Participant, .after = Experiment)
# Check whether merging went well: 
# no extra rows next to data frame lengths should have been added when successful
dim(all_data_study_1)[1] + dim(all_data_study_2)[1] == dim(all_data)[1]

## 4. Create a time difference variable between min and max Local_Date_Time   ----
all_data <- all_data %>% group_by(Participant) %>% 
  mutate(Time_On_Task = difftime(max(Local_Date), min(Local_Date), units='mins')) %>%
  relocate(Time_On_Task, .after = Condition)


## 5  Do some more cleaning and keep only relevant variables                  ----
## Also remove the duplicate row at Test button B
all_data <- all_data %>% 
  filter(!(Screen_Name == 'Test' & Zone_Name == 'buttonB'))

## There are some NA duplicates in Standard only where Trial is NA, remove
all_data <- all_data %>% 
  filter(!(is.na(Trial)))

## Save only relevant columns
data <- all_data %>% select(c(Experiment, Event_Index, Local_Date, Condition, 
                             Participant, Participant_ID, Participant_External_Session_ID,
                             Trial, Participant_Monitor_Size, Screen_Name,
                             Zone_Name, Reaction_Time, Response, Time_On_Task))

## 6. Final cleaning after all data set creations                             ----
## Kick out the unnessessary External ID
data <- data %>% select(-c(Participant_External_Session_ID))

data %>% ungroup() %>% filter(Experiment == 2) %>% distinct(Participant) %>% count()

## 7. Extra: Data preparation for coding                                      ----
coding_preparation <- data %>% ungroup() %>%
  select(Event_Index, Participant_ID, Trial, Screen_Name, Zone_Name, Response) %>%
  filter(grepl("Diagram|Test", Screen_Name)) %>% 
  filter(!(grepl("Diagram", Screen_Name) & Zone_Name == "Zone1"))
## Test data: 
# Filter out test responses
coding_preparation_test <- coding_preparation %>% filter(Screen_Name == "Test") %>%
  select(-c(Screen_Name, Zone_Name)) 
# Create the empty columns to be filled during coding
new_col_coding_tests <- c(paste0("Element_",1:6), paste0("Commission_", 1:4))
# Merge those columns to the data
coding_preparation_test[new_col_coding_tests] <- NA
## Diagram data:
# Filter out test responses
coding_preparation_diagrams <- coding_preparation %>% filter(Screen_Name == "Diagram") %>%
  select(-c(Screen_Name)) %>% add_column(CorrectPoints = NA, ElementNumber = NA, Commission = NA)
# Create the empty columns to be filled during coding

## -------------------------------------------------------------------------- ----
## 8. Save the cleaned data files (uncomment if needed)                       ----
# Save current date to append to output
today <- Sys.Date()
today <- format(today, format="%d%b%Y")

# Save the processed raw data for reation times and time on task
#write_delim(data, paste0(rel_path_for_output,"cleaned_performance_data_", today, ".txt"))

############################################################################  ----              
