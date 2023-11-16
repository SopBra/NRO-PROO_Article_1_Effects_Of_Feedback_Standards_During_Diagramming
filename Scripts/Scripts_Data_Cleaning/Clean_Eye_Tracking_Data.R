############################################################################  --
##                  Data Cleaning Gaze Data Study 2:
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
## Check and perform installation of relevant packages if needed
# Relevant packages
list_of_packages <- c("tidyverse", # distinct, filter, %>%, and many more
                      "tibble",  # to add a column at a specific position
                      "data.table", # for fread, reading in data file
                      "bigreadr" # for reading in the bigger raw data file
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
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_author_cleaning_script_eye_tracking_data.txt')

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

##    Place here some relevant functions for the script                       ----
# A function that loads all files in a given directory into the global environment 
load_all_txt_files <- function(incoming_path,...) {
  # Save all required file names in each directory
  filenames_in_directory <- list.files(paste0("./",incoming_path))
  # Only select the txt files
  selected_txt_filenames <- filenames_in_directory[grepl(".txt", filenames_in_directory)]
  # Load all files together into a list with data.table::fread
  txt_file_list <- sapply(paste0(incoming_path, selected_txt_filenames), fread, fill=TRUE)
  # Exclude the path from entry name and .txt from the file name 
  names(txt_file_list) <- str_extract(names(txt_file_list), selected_txt_filenames)
  names(txt_file_list) <- gsub(".txt","", names(txt_file_list))
  txt_file_list
}

# A function to remove white spaces from the variable names
edit_column_names <- function(incoming_data) {
  # Remove the % of tracking ratio etc
  names(incoming_data) <- gsub("%", "", names(incoming_data), fixed = TRUE)
  # Remove all white spaces from the variable names and replace them with a _
  names(incoming_data) <- gsub(" ", "_", names(incoming_data), fixed = TRUE)
  # Do the same for the - separator 
  names(incoming_data) <- gsub("-", "_", names(incoming_data), fixed = TRUE)
  # Also the : of time variables
  names(incoming_data) <- gsub(":", "_", names(incoming_data), fixed = TRUE)
  # Also remove the square brackets
  names(incoming_data) <- gsub("[", "", names(incoming_data), fixed = TRUE)
  names(incoming_data) <- gsub("]", "", names(incoming_data), fixed = TRUE)
  incoming_data
}

##    Set the paths to the required data and load files                       ----
## Save paths to the different directories where the data is stored
# To the directory of the real raw data:
rel_path_raw_data <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Eye_Tracking_Raw_Data/"

## Set the output directory for files processed in this script
rel_path_for_output <- "Output/Data_Cleaning_Scripts/Study_2_Eye_Tracking_CRVR_Data/"
## -------------------------------------------------------------------------- -----
##    Load all files from the specified directories                           ----
## First store files of each directory into a separate list
# Processed data directory
raw_data_list <- load_all_txt_files(rel_path_raw_data)
# Remove the 'artificial' from the file names
names(raw_data_list) <- gsub('artificial_', '', names(raw_data_list))

## Load all files from the lists into the global environment/ current workspace
list2env(raw_data_list, .GlobalEnv)
rm(raw_data_list)
## -------------------------------------------------------------------------- ----
overall_start <- Sys.time()
## 1. Organize column names and trial numbers properly                        ----
## Convert Trial(-number) from character into integer
# 1. Kick out the "Trial" before the number (i.e, Trial001 --> 001)
eye_tracking_raw_data$Trial <- sub('.*(?=.{3}$)', '', eye_tracking_raw_data$Trial, perl=T)
# 2. Transform into integer
eye_tracking_raw_data$Trial <- as.integer(eye_tracking_raw_data$Trial)

## Remove white spaces from column names for easier handling
eye_tracking_raw_data <- edit_column_names(eye_tracking_raw_data) %>% 
  select(c(Participant:Pupil_Position_Right_Y_px)) %>%
  rename(Tracking_Ratio = Tracking_Ratio_)

## 2. Fix the trial indices for cases with loading or crashing issues         ----
# Runtime up to 25min 
## Create a trial overview (own "Specialized Stats." file) 
#  to check generalizability of computations for enitre dataset.
#  This overview helps to see where trial indecies need to be fixed 
#  after loading issues or crashing of the experiment. 
trial_overview_before_fixing_trial_no <- eye_tracking_raw_data %>% 
  distinct(Stimulus, Trial, Participant)

## Overview of trial-meaning if recording went well:
# Trial  8: Pratice trial Sporten is gezond; 
# Trial  9, 14, 19, 24, 29, 34: Start-screen of the experiment; 
# Trial 10, 15, 20, 25, 30, 35: Text, JoL1, Diagram Completion (combined in trial number) 
# Trial 11, 16, 21, 26, 31, 36: Diagram-Feedback Screen
# Trial 12, 17, 22, 27, 32, 37: JoL2
# Trial 13, 18, 23, 28, 33, 38: Test (Intro and Test combined in each trial number)

## Do the corrections for datasets P01, P02, P04, P11, P14, P16, P18 
#  with the help of the created trial overview, log-files and screen-recordings of experiment

start_time_eye_tracking_raw_data <- Sys.time()
# 223902830: Substract 1 from every Trial number that is greater than 9
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "223902830" & Trial >= 10,
                        Trial - 1,
                        Trial))


# 267292794: Substract 2 from every Trial number that is greater than 9
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "267292794" & Trial >= 10,
                        Trial - 2,
                        Trial))

# 372793729: Exclude Trial 32 
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "372793729" & Trial == 32))
#.. and substract all Trial numbers after with -1
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "372793729" & Trial >= 32,
                        Trial - 1,
                        Trial))
# ..also, expriment got stuck at Trial 14, so remove
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(! (Participant == "372793729" & (Trial >=15 & Trial <=17)))
# and substract Trial number accordingly
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "372793729" & Trial > 14,
                        Trial - 3,
                        Trial))
# Also, one trial was filled in by experimenter as a dummy to check whether 
# next trial would be the right one. 
# Not the case so one trial less and last dummy trial needs to be excluded anyway
# (see screen-recording for proper insight into this issue) 
# --> however keep the fixation in the end!
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "372793729" & Trial >= 35 & Trial <= 40))

# 301499663: remove Trials >=20 (experiment crashed at money trial), 
# it was restarted with Participant_ID 372793729
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "301499663" & Trial >= 20))

# 372793729: remove Trials <= 8 (after restart, kick out second calibration)
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "372793729" & Trial <= 8))
# ...Trial 10 --> 20, 15 --> 25 => so add 10 to Trialnumber
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "372793729", Trial + 10, Trial))
# ...remove I after Participant_ID 
# (not detect string as this takes longer than just replacing by condition..)
eye_tracking_raw_data$Participant[eye_tracking_raw_data$Participant == "372793729"] <- "301499663"

# 590966039: Remove Trial 23
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "590966039" & Trial == 23))
# ... and substract following Trialnumbers with one 
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "590966039" & Trial > 23,
                        Trial - 1, Trial))

# 627738747: Remove Trial 13 
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "627738747" & Trial == 13))
# ... and substract following Trialnumbers with one 
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "627738747" & Trial > 13,
                        Trial - 1,
                        Trial))

# 915972990: Remove Trial 32
eye_tracking_raw_data <- eye_tracking_raw_data %>% filter(!(Participant == "915972990" & Trial == 32))
# ... and substract following Trialnumbers with one 
eye_tracking_raw_data <- eye_tracking_raw_data %>% ungroup() %>%
  mutate(Trial = ifelse(Participant == "915972990" & Trial > 32,
                        Trial - 1,
                        Trial))

end_time_eye_tracking_raw_data <- Sys.time()
time_collapsed_trialFixing <- end_time_eye_tracking_raw_data - start_time_eye_tracking_raw_data
## Save another trial Overview after the update and check if everything is aligned to trial overview above 
trial_overview_after_fixing <- eye_tracking_raw_data %>% distinct(Stimulus, Trial, Participant)


## 3. Select data of diagram study screens & fix the Gorilla-Participant_ID   ----
## Add Screen_Name column after Trial column
# First Empty
eye_tracking_raw_data <- add_column(eye_tracking_raw_data, Screen_Name = NA, .after = "Trial")
# Create a vectors with all Trial Numbers for Feedback Screen and JoL2
feed_screen_vec <- c(11, 16, 21, 26, 31, 36)
# Now fill up Screen_Name column with values for DiagramFeedback 
# if Trial Number is in the respective vectors.
eye_tracking_raw_data$Screen_Name[eye_tracking_raw_data$Trial %in% feed_screen_vec] <- "Diagram_Feedback"
# Check if computations seem right with table()
table(eye_tracking_raw_data$Screen_Name, useNA = "ifany")

## Kick out all Screen_Name == NA, so that only DiagramFeedback trials remain
raw_data_red_2_diagram_feedback <- eye_tracking_raw_data %>% filter(!is.na(eye_tracking_raw_data$Screen_Name)) 
# Check the trial overview again
trial_overview_diagram_feedback <- raw_data_red_2_diagram_feedback %>% 
  distinct(Stimulus, Trial, Screen_Name, Participant) 
## Fix the Participant ID variable. 
# Now the Gorilla-Participant_ID is part of the strings in "Stimulus".
# Extract the Gorilla participant ID from the http Strings in Stimulus 
# (so the last 7 characters)
# (e.g., "https://research.sc/task/3394940" to "3394940")
raw_data_red_2_diagram_feedback$Stimulus <- 
  sub('.*(?=.{7}$)', '', raw_data_red_2_diagram_feedback$Stimulus, perl=T) 

# Added for the artificial data version (manual cleaning above was random due to new data + IDs)
raw_data_red_2_diagram_feedback <- raw_data_red_2_diagram_feedback %>% 
  filter(!grepl('End.rtf|ack.rtf|https:', Stimulus)) 

trial_overview_diagram_feedback$Stimulus <- 
  sub('.*(?=.{7}$)', '', trial_overview_diagram_feedback$Stimulus, perl=T)
# Rename column "Stimulus" to "Participant_ID"
names(raw_data_red_2_diagram_feedback)[names(raw_data_red_2_diagram_feedback) == "Stimulus"] <- "Participant_ID"
names(trial_overview_diagram_feedback)[names(trial_overview_diagram_feedback) == "Stimulus"] <- "Participant_ID"
# Check the Participant_ID variable
table(raw_data_red_2_diagram_feedback$Participant_ID, useNA = "ifany")

## Some participants had only 5 Trials, so kick out the presumed last trial number (NA returned anyway)
# So kick out the screens that just returned NA 
raw_data_red_2_diagram_feedback <- raw_data_red_2_diagram_feedback %>% 
  filter(!(raw_data_red_2_diagram_feedback$Participant == "244334030" & 
             raw_data_red_2_diagram_feedback$Trial == 36))
raw_data_red_2_diagram_feedback <- raw_data_red_2_diagram_feedback %>% 
  filter(!(raw_data_red_2_diagram_feedback$Participant == "223902830" & 
             raw_data_red_2_diagram_feedback$Trial == 36))
## 4. Load & merge the overall participant IDs of data from Gorilla           ----
# Rename "Trial" to "ScreenNumber" in the eye-tracking data
names(raw_data_red_2_diagram_feedback)[names(raw_data_red_2_diagram_feedback) == "Trial"] <- "Screen_Number"

## Rename the data set for better overview
diagram_screen_data_with_trials <- raw_data_red_2_diagram_feedback %>%
  mutate(Participant_ID = Gorilla_ID) %>% 
  filter(!grepl('End.rtf|ack.rtf|https:', Participant_ID)) %>%
  select(-Gorilla_ID)

table(diagram_screen_data_with_trials$Participant_ID %in%  id_key_file_study_2$Participant_ID)

## Retrieve the trial_names from the gorilla ID file
diagram_screen_data_with_trials <- diagram_screen_data_with_trials %>% 
  group_by(Participant_ID) %>% 
  mutate(Trial = unique(id_key_file_study_2$Trial[id_key_file_study_2$Participant_ID == unique(Participant_ID)])) %>% 
  relocate(Trial, .after = Participant_ID)

## 5. Do some data cleaning (i.e., remove non-informative columns)            ----
# Runtime: 21sec
## Exclude the data that is not category "Fixation"
cleaned_raw_data <- diagram_screen_data_with_trials %>% 
  filter(Category_Group == "Eye")

# Remove irrelevant columns/select possibly relevant ones
cleaned_raw_data <- cleaned_raw_data %>% select(Participant, Participant_ID, Trial,
                                            RecordingTime_ms, Time_of_Day_h_m_s_ms,
                                            Screen_Number, Export_Start_Trial_Time_ms,
                                            Export_End_Trial_Time_ms, Color, Tracking_Ratio,
                                            Category_Right, Index_Right, 
                                            Pupil_Size_Right_X_px, Pupil_Size_Right_Y_px,
                                            Pupil_Diameter_Right_mm, Point_of_Regard_Right_X_px,
                                            Point_of_Regard_Right_Y_px, Gaze_Vector_Right_X,
                                            Gaze_Vector_Right_Y, Gaze_Vector_Right_Z,
                                            Eye_Position_Right_X_mm, Eye_Position_Right_Y_mm, 
                                            Eye_Position_Right_Z_mm, Pupil_Position_Right_X_px, 
                                            Pupil_Position_Right_Y_px)

## All columns from PupilSizeRightX_px to PupilPositionRightY_px are characters
# Transform them to numeric variables
cleaned_raw_data <- cleaned_raw_data %>% 
  mutate(across(.col = c(Pupil_Size_Right_X_px:Pupil_Position_Right_Y_px), as.numeric))

## Check whether the NAs introduced by coercion were added to Point of Regard 
# (rest not relevant)
table(is.na(cleaned_raw_data$Point_of_Regard_Right_X_px) | is.na(cleaned_raw_data$Point_of_Regard_Right_Y_px), useNA = "ifany")

## 7. Extract Fixation point data for drift & pupil dilation analysis         ----
## Filter all Trials with "Fixation" in the name from the raw data
fixation_point_data <- eye_tracking_raw_data %>% 
  filter(grepl('Fixation', Stimulus)) # Fixation FixationEnd, FixationBlack
# Also filter out only eye-data (Category_Group == "Eye") again
fixation_point_data <- fixation_point_data %>% 
  filter(fixation_point_data$Category_Group == "Eye") 
# And only include fixations
fixation_point_data <- fixation_point_data %>% dplyr::filter(Category_Right == "Fixation")

# Reduce this data to potentially relevant columns                    
fixation_point_data <- fixation_point_data %>% select(Participant,
                                                      RecordingTime_ms,           
                                                      Stimulus,
                                                      Export_Start_Trial_Time_ms,
                                                      Export_End_Trial_Time_ms,
                                                      Tracking_Ratio,              
                                                      Category_Right,             
                                                      Index_Right,
                                                      Pupil_Size_Right_X_px,
                                                      Pupil_Size_Right_Y_px,         
                                                      Pupil_Diameter_Right_mm,  
                                                      Point_of_Regard_Right_X_px,     
                                                      Point_of_Regard_Right_Y_px)     

## 8. Compute the AOIs                                                        ----
start_time <- Sys.time()
# Runtime can take up to 8 - 10 hours!

## This function checks whether x and y coordinates in the data are within 
# the loaded AOI definitions per trial/diagram.
# The function takes an x-coordinate, a y-coordinate and a trial name,
# it returns the BoxType (i.e., AOI_name) if conditions match, and otherwise remains empty.
compare_AOI_px <- function(value_X, value_Y, trial_name) {
  return_AOI <- my_AOIs$BoxType[my_AOIs$Trial == trial_name & 
                                value_X >= my_AOIs$x_Min & value_X <= my_AOIs$x_Max & 
                                value_Y >= my_AOIs$y_Min & value_Y <= my_AOIs$y_Max] 
    
  # Check if return_AOI is empty
  return_value <- ifelse(!is_empty(return_AOI), return_AOI, 'Not_In_AOI')
}

## Assign the AOI names: go through the data and call the function "compare_AOI_px"
eye_tracking_data_with_AOIs <- cleaned_raw_data %>% 
  ungroup() %>% rowwise() %>% # For every row of cleaned_raw_data, 
  dplyr::mutate(AOI_name = # Assign the value returned by compare_AOI_px to AOI_name by first
                  # Calling compare_AOI_px with the x.. 
                  compare_AOI_px(Point_of_Regard_Right_X_px, 
                                 Point_of_Regard_Right_Y_px, #.. and y values of each row, 
                                 Trial)) # as well as the Trial name of the row

# Relocate the AOI_name column in the dataset
eye_tracking_data_with_AOIs <- eye_tracking_data_with_AOIs %>% relocate(AOI_name, .after = Trial)

## Add a column indicating whether the AOI-Hit was on the own diagram or the diagram standard
eye_tracking_data_with_AOIs <- add_column(eye_tracking_data_with_AOIs, 
                                          Diagram_Type = NA, 
                                          .after = "AOI_name")
# (Select based on string indicating "Own" or "Feed" (Standard) Box
eye_tracking_data_with_AOIs$Diagram_Type[grep("Feed", eye_tracking_data_with_AOIs$AOI_name, fixed = TRUE)] <- "Standard_Diagram"
eye_tracking_data_with_AOIs$Diagram_Type[grep("Own", eye_tracking_data_with_AOIs$AOI_name, fixed = TRUE)] <- "Own_Diagram"
eye_tracking_data_with_AOIs$Diagram_Type[eye_tracking_data_with_AOIs$AOI_name == "Not_In_AOI"] <- "Not_In_AOI"

# Save the end time and check how long the computation of the AOIs took
end_time <- Sys.time()
my_run_time <- end_time - start_time
my_run_time


overall_end <- Sys.time()
overall_time_collapsed <- overall_end - overall_start
overall_time_collapsed
## 9. Save all output files                                                   ----
## Save current date to append to output
today <- Sys.Date()
today <- format(today, format="%d_%b_%Y")

## Save the computed file (uncomment if needed)
#write_delim(eye_tracking_data_with_AOIs, file = paste0(rel_path_for_output, "eye_tracking_data_with_AOIs_", today, ".txt")) 
#write_delim(fixation_point_data, file = paste0(rel_path_for_output, "fixation_point_data_", today, ".txt"))

############################ The End ######################################## ----
