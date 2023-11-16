############################################################################### --
##                                                                              ##
##                    Article 1 - Artificial Data Creation                      ##           
##                                Sophia Braumann                               ##              
##                                                                              ##
############################################################################### --
##                          Overview of This Script                                                                                         
## 0. Set-Up  & Functions             
## 1. Simulate Eye-Tracking Raw Data
## 2. Simulate Gorilla (performance) raw Data
## 3. Simulate the Cleaned Eye-Tracking Data for Analysis
## 4. Simulate Cleaned Gorilla (Performance) Data for Analysis
## 5. Output Generation 
## --------------------------------------------------------------------------- --
##
##           !! COLLAPSE ALL LINES TO BETTER NAVIGATE THIS SCRIPT !!
##            (Edit->Folding->Collapse All / alt+cmd+o/ Ctrl+Alt+o)
##
## --------------------------------------------------------------------------  --
## 0. ------------ Set-Up  & Functions --------------------------------------  ----
##    Set a seed for reproducibility                                           ----
set.seed(070492) # Set a seed so that the same random participant IDs are generated
##    (Install if necessary and) load relevant libraries                       ----
#  Check and perform installation of relevant packages if needed
list_of_packages <- c("tidyverse",
                      "stringi", # for string processing (create random ids etc)
                      #"stringr", # for keeping strings of a string
                      "data.table", # for fread (reading in files with different dim)
                      "readr", # for writing .txt files
                      "lubridate" # modify the time variable
                      )  

# List of relevant packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])] # Check whether all packages are installed
if(length(new_packages)) install.packages(new_packages, repos = list(CRAN="http://cran.rstudio.com/")) # Install them if that is not the case
## Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)

##    Alert about R version                                                    ----
## Retrieve the session info of the user for comparing R and package versions
session_info_user <- sessionInfo()

## Compare the R versions
r_version_user <- session_info_user$R.version$version.string
# The author used R version 4.3.0, check if the user uses the same version
paste0(ifelse(grepl('4.3.1', r_version_user),
              'You are using the same version of R under which this script was last updated. You are good to go :)',
              'You are using a different version of R than the one under which this script was created. Please report malfunctioning of the script based on outdated packages/functions'))

## Compare the package versions
# Load the package versions used by the author at the time of script creation
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_artificial_data_creation.txt')

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

## Tidy up the workspace
rm(package_infos_at_script_creation, package_infos_user, session_info_user)
## -------------------------------------------------------------------------- ##
##    Set the relative paths to the artificial raw data                        ----
# All (artificial) ID files
rel_path_ids_both_studies <- "Data/all_IDs_random_to_random.txt"
# Eye Tracking raw data
rel_path_eye_tracking_raw_data <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Eye_Tracking_Raw_Data/"
# Gorilla raw data
rel_path_gorilla_raw_data_study_study_1 <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Performance_Raw_Data/Study_1/"
rel_path_gorilla_raw_data_study_study_2 <- "Data/Raw_Data_For_Data_Cleaning/Artificial_Performance_Raw_Data/Study_2/"

# Processed data Study 1
rel_path_processed_performance_data <-"Data/Processed_Data_For_Analyses/Artificial_Performance_Data/"
# Processed data Study 2
rel_path_processed_gaze_data <- "Data/Processed_Data_For_Analyses/Artificial_Eye_Tracking_Data/"

## Set path to the output folders
# For the raw data of both studies
rel_path_raw_eye_tracking_data_output <- "Output/Artificial_Data_Creation/Raw_Data/Eye_Tracking_CRVR_Data/"
rel_path_raw_performance_data_output <- "Output/Artificial_Data_Creation/Raw_Data/Performance_Data/"

# For the processed data of both studies
rel_path_processed_eye_tracking_data_output <- "Output/Artificial_Data_Creation/Processed_Data/Eye_Tracking_CRVR_Data/"
rel_path_processed_performance_data_output <- "Output/Artificial_Data_Creation/Processed_Data/Performance_Data/"

## -------------------------------------------------------------------------- ##
##    Run relevant functions for this script                                   ----

## Count the number of decimals of a variable
# From CoderGuy123: https://stackoverflow.com/questions/5173692/how-to-return-number-of-decimal-places-in-r
count_decimals <- function(x) { 
  # length zero input
  if (length(x) == 0) return(numeric())
  
  #count decimals
  x_nchr = x %>% abs() %>% as.character() %>% nchar() %>% as.numeric()
  x_int = floor(x) %>% abs() %>% nchar()
  x_nchr = x_nchr - 1 - x_int
  x_nchr[x_nchr < 0] = 0
  
  x_nchr
}

## A function that generates random values between minimum and maximum value of real data
# (It also takes into account whether decimals were present)
generate_numeric_data <- function(incoming_column) {
  # Save all values that were not NAs
  non_empty_columns <- incoming_column[incoming_column != "" & !is.na(incoming_column)]
  ## First retrieve all entries that are no numbers 
  # In case of this data set, extract all '-', so extract all (Non-)Numbers
  # See https://github.com/rstudio/cheatsheets/blob/main/regex.pdf for Regex help
  incoming_column_numeric <- non_empty_columns[grep("\\d", non_empty_columns)] %>% # Extract all numbers
    type.convert(as.is = TRUE) 
  # Save all non-numbers:
  incoming_column_non_numeric <- incoming_column[grep("\\d", incoming_column, invert = TRUE)]
  
  ## The upcoming part will work with dividing the data set in two to simulate proper values
  ## So check if the data set has an even number of values
  desired_length <- ifelse(length(incoming_column_numeric) %% 2 == 0,
                          length(incoming_column_numeric), # If is does, keep the length
                          length(incoming_column_numeric)-1) # Otherwise subtract 1 value
  
  ## Retrieve some descriptives of the real data to simulate the artificial data with similar patterns
  min_value <-  min(incoming_column_numeric, na.rm = TRUE)
  max_value <- max(incoming_column_numeric, na.rm = TRUE)
  median_value <- median(incoming_column_numeric, na.rm = TRUE)
  upper_value <- (mean(incoming_column_numeric, na.rm = TRUE) - (min_value/2))*2
  
  # Check if the incoming numeric data has decimals (i.e., is integer or double)
  integer_check <- is.integer(incoming_column_numeric)
  
  ## Sample numbers between min and upper value
  first_half <- if(integer_check) { # If values are integers (not decimals)
    # Sample numbers between min and upper value
    sample(min_value:upper_value, size = desired_length/2, replace=TRUE) 
  } else {
    # Otherwise (i.e., values were decimals; double)
    # Sample values between min and upper_value-1, 
    # round those numbers to get rid of the original decimals,
    paste0((sample((round(min_value,0)):(round(upper_value-1,0)), size = desired_length/2, replace=TRUE)), 
           ".", # Add a dot ("."), and add a decimal between 0-9
           (sample(0:99, size = desired_length/2, replace=TRUE)))
  }
  first_half <- as.numeric(first_half) # Transform (back) to numeric
  
  ## Generate the second half of the data by calculating pairwise differences to the mean
  # Check whether the mean needs to be rounded (i.e., was it an integer or a double)
  mean_Value <- ifelse(integer_check == TRUE, # Was the mean an integer?
                       # If the answer is YES: round the mean so that the created values will be integers
                       round(mean(incoming_column_numeric, na.rm = TRUE)),
                       # If the answer is NO: simply calculate the mean
                       mean(incoming_column_numeric, na.rm = TRUE))
                
  ## Create values that amount to the mean when added to a value of the first half divided by 2
  # For every value in the first half, 
  # compute a number that will result in the overall mean: mean = (value_first_half + x) / 2
  # So x = mean_Value*2 - value_first_half
  second_half <- (mean_Value*2 - first_half) 
  
  ## Add the second half to the first half and check decimal places of incoming data for rounding
  artificial_values <- c(first_half, second_half) 
  # How many decimal places did the incoming data maximally have?
  # Call the function count_decimals from above
  decimals_to_round <- max(count_decimals(incoming_column_numeric), na.rm = TRUE)
  # Round the artificial values accordingly
  artificial_values <- round(artificial_values, decimals_to_round)
  
  ## Check if the desired_length was uneven 
  artificial_values <- if(length(non_empty_columns) %% 2 != 0) { # If it was uneven, 
    artificial_values <- c(artificial_values, max_value) # add the maximum value
    artificial_values
  } else { # So the length was even
    # Randomly choose an instance of the artificial values and replace by the maximum
    artificial_values[sample(1:length(artificial_values), size = 1)] <- max_value 
    artificial_values
  }
  
  ## Double check the min and max values
  artificial_values[which(artificial_values < min_value)] <- min_value
  artificial_values[which(artificial_values > max_value)] <- max_value
  
  ## Add the non-numeric values again
  # First add the non-numerics after the artificial values
  artificial_values <- c(artificial_values, incoming_column_non_numeric) 
  # If there were empty values, append them back to the vector
  nas_to_append <- ifelse(length(incoming_column) <= length(artificial_values),
                          0,
                          length(incoming_column) - length(artificial_values))
  artificial_values <- c(artificial_values, rep(x = NA, times = nas_to_append)) 
  # Then shuffle the values 
  artificial_values <- sample(artificial_values) 
  
  ## Return the column with the new values
  artificial_values
}

## Check whether a column only consist of NAs and call two different functions accordingly
call_generator_if_not_na <- function(incoming_column){
  returned_values <- ifelse(all(is.na(incoming_column)), 
                            incoming_column,
                            generate_numeric_data(incoming_column))
  returned_values
}

## A function that modifies existing participant IDs
generate_numeric_IDs <- function(incomming_ids) {
  # Save how many IDs should be generated
  number_ids_to_generate <- length(unique(incomming_ids))
  digits_to_generate <- nchar(as.character(sample(x = incomming_ids, size = 1)))
  lower_bound <- as.numeric(paste(rep(x = 9, times = digits_to_generate-1), collapse = ""))
  upper_bound <- as.numeric(paste(rep(x = 9, times = digits_to_generate), collapse = ""))
  # Generate the ids
  ids_to_return <- sample(lower_bound:upper_bound, size = number_ids_to_generate, replace = FALSE)
  ids_to_return
}

generate_non_numeric_IDs <- function(incomming_ids) {
  # Save how many IDs should be generated
  number_ids_to_generate <- length(incomming_ids) 
  # Sample one ID 
  sample_id <- sample(incomming_ids, size = 1)
  # Save the length of the sample id 
  length_id <- nchar(sample_id, keepNA=TRUE)
  # Generate random strings with the same length as the one of the sample id
  random_ids <- stri_rand_strings(number_ids_to_generate, length_id)
  random_ids
}

## A function that helps reading in all raw data files in a defined directory by taking
#   the path to the directory, a pattern of relevant filenames and a file type
load_all_raw_data_files <- function(incoming_path, relevant_pattern, file_type) {
  # Collect all file names that should be loaded
  filenames <- list.files(path = incoming_path, 
                          pattern = paste0(relevant_pattern,"(.)*.",file_type,"$")) 
  # Load the files by adding the path to the directory to the file names,
  file_frame <- sapply(paste0(incoming_path, filenames), fread, fill=TRUE)
  # Exclude the path from entry name and file_type from the file name 
  names(file_frame) <- paste0("data_", 1:length(file_frame))
  file_frame
}

remove_spaces_from_colum_names <- function(incoming_data) {
  # Remove all white spaces from the variable names and replace them with a _
  names(incoming_data) <-
    gsub(" ", "_", names(incoming_data), fixed = TRUE)
  # Do the same for the - separator
  names(incoming_data) <-
    gsub("-", "_", names(incoming_data), fixed = TRUE)
  # And also for dots .
  names(incoming_data) <-
    gsub(".", "_", names(incoming_data), fixed = TRUE)
  incoming_data
}

## Load all files in a given directory into the global environment 
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

# Check which separator was used and call a respective read.csv function
# !!This function was created with the help of ChatGPT --> i.e., extract the separator
check_csv_separator <- function(incoming_paths_and_file_names) {
  # Read in the first line
  first_line <- readLines(incoming_paths_and_file_names, n = 1)
  extract_separator_indices <- as.vector(gregexpr(";|,|\t",first_line)[[1]])
  sepatator <- substr(first_line, extract_separator_indices, extract_separator_indices)
  sepatator
}

## Load all other file types with grepl specifications
load_set_of_specific_csv_files <- function(incoming_path, chr_pattern_of_interest) {
  # List all csv files names with the specified pattern in the specified directory
  relevant_file_names <- list.files(path = incoming_path,
                                    pattern = paste0(chr_pattern_of_interest, "(.)*.", "csv", "$"))
  # Save the connections to the files to check their separators
  all_paths_and_file_names <- paste0(incoming_path, relevant_file_names)
  
  # Check which separator was used by sampling one of the files
  csv_separator <- check_csv_separator(sample(all_paths_and_file_names, size = 1))
  
  # Load those csv files with the found separator
  relevant_files <- lapply(all_paths_and_file_names, read.csv, sep = csv_separator)
  
  names(relevant_files) <- paste0("data_", seq(1:length(relevant_files)))
  relevant_files
  ## For the purpose of data cleaning, stop here and do not merge those files ##
  # Run through the list and transform all columns to characters
  # (so that same column names that contain different data types can be merged)
  #raw_data_ls <- lapply(relevant_files, function(x) {
  #  x[] <- lapply(x, as.character)
  #  x
  #})
  
  
  # Merge 
  #all_raw_data <- raw_data_ls %>% 
  #  reduce(full_join) %>% 
  #  type.convert(as.is = TRUE) 
}

## -------------------------------------------------------------------------- ##
## 1. -----------  Simulate the Eye Tracking Raw Data ------------------------ ----
##    Load the real data necessary for the eye-tracking data cleaning script   ----
gaze_raw_data_list <- load_all_txt_files(rel_path_eye_tracking_raw_data)
# Remove the 'artificial' from the file names
names(gaze_raw_data_list) <- gsub('artificial_', '', names(gaze_raw_data_list ))
# Then load all files from the list into the global environment/ current workspace
list2env(gaze_raw_data_list, .GlobalEnv)

##    Simulate Eye-Tracking Raw Data                                           ----     

## The real data set is super large, retrieve a random sub-sample 
#  (now set to a fourth of the length of real data)
desired_observations <- round(dim(eye_tracking_raw_data)[1]/4) # Adapt if desired
raw_data_part <- eye_tracking_raw_data %>% slice_sample(n = desired_observations)

## Data set specific: Further reduce size
#  To ensure functioning of the data cleaning and analysis scripts,
#  sample multiple instances of fixation indices (Index.Right)/participant 
raw_data_part <- raw_data_part %>% # (works out by keeping distinct combinations with pupil size)
  distinct(Participant, `Index Right`, `Pupil Position Right X [px]`, .keep_all = TRUE)

## Create a new random date time variable
# First create vectors of possible hours, minutes, seconds and milliseconds to then raw from
possible_hours <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 23))
possible_minutes_and_seconds <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 59)) 
possibile_milliseconds <- paste0(seq(from = 100, to = 999))
# Plug the strings together
artificial_eye_tracking_raw_data <- raw_data_part %>%
  # For each participant,
  group_by(Participant) %>%
  # Randomly draw one number of the possible hour vector and print it followed by a :
  mutate(`Time of Day [h:m:s:ms]` = paste0(sort(sample(possible_hours, size = 1)),
                                           ':')) %>%
  # Then group by participant and trial
  group_by(Participant, Trial) %>%
  # And randomly sample one number (for the artificial minutes) of the minutes and seconds vector for each trial, 
  mutate(`Time of Day [h:m:s:ms]` = paste0(`Time of Day [h:m:s:ms]`, 
                                           sort(sample(possible_minutes_and_seconds, size = 1)),
                                           ':')) %>% # print it followed by a : again.
  # Then re-group the data again by participant, trial and gaze vector
  group_by(Participant, Trial, `Gaze Vector Right X`) %>%
  # and randomly sample one number per gaze index from the minutes and seconds vector (for the artificial seconds),
  mutate(`Time of Day [h:m:s:ms]` = paste0(`Time of Day [h:m:s:ms]`, 
                                           sort(sample(possible_minutes_and_seconds, size = 1)),
                                           ':')) %>% # followed by a : again.
  # Regroup again by participant, trial, and fixation index 
  group_by(Participant, Trial, `Index Right`) %>%
  # and randomly sample one number per fixation index from the possible milliseconds vector
  mutate(`Time of Day [h:m:s:ms]` = paste0(`Time of Day [h:m:s:ms]`,
                                           sort(sample(possibile_milliseconds, size = 1))))

## Check all column names to select columns for the different functions 
colnames(artificial_eye_tracking_raw_data)

# Select all columns with numeric values that should be fed to the generate_numeric_data function
columns_for_numeric_data_generation <- c("RecordingTime [ms]", "Export End Trial Time [ms]", "Tracking Ratio [%]", "Pupil Size Right X [px]", 
                                         "Pupil Size Right Y [px]", "Pupil Diameter Right [mm]", "Point of Regard Right X [px]",
                                         "Point of Regard Right Y [px]", "Gaze Vector Right X", "Gaze Vector Right Y",         
                                         "Gaze Vector Right Z", "Eye Position Right X [mm]", "Eye Position Right Y [mm]",   
                                         "Eye Position Right Z [mm]", "Pupil Position Right X [px]", "Pupil Position Right Y [px]",
                                         "Mouse Position X [px]", "Mouse Position Y [px]")

## Make sure columns are actually numeric
## Apply generate_numeric_data function to specified columns
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  ungroup() %>%
  # Call the function as.numeric to all the columns selected in the previous step
  mutate(across(.cols = all_of(columns_for_numeric_data_generation), as.numeric))

## Apply generate_numeric_data function to specified columns
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  ungroup() %>%
  # Call the function generate_numeric_data() to all the columns selected in the previous step
  mutate(across(.cols = all_of(columns_for_numeric_data_generation), generate_numeric_data))

## Replace the real IDs by artificial ones, based on a key file so that the IDs are the same across data sets
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  # For each participant,
  group_by(Participant) %>%
  # Create a new participant ID by randomly sampling a number from the artificial key file
  mutate(Participant_N = sample(id_key_file_study_2$Participant, size = 1, replace = FALSE)) %>% 
  ungroup() %>%
  # Then remove the original participant ID 
  select(-Participant) %>%
  # And rename the newly created variable by giving it the name of the removed original ID variable
  rename(Participant = Participant_N) %>%
  relocate(Participant) # (Relocate the variable to the front of the data set)

## Modify the Gorilla-ID in the string of Stimulus: e.g.: https://research.sc/task/3424841
# Retrieve the last digits (i.e., the Gorilla-ID), so everything after https://research.sc/task/
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  ungroup %>% 
  # Retrieve the Gorilla ID through the following if-else calls: 
  mutate(Gorilla_ID  = ifelse(grepl('https://research.sc/task/', Stimulus), # If you find this pattern in Stimulus
                              gsub('https://research.sc/task/', '', Stimulus), # Remove this pattern from the string
                              Stimulus)) %>%   # Else, print the other pattern you found again
  relocate(Gorilla_ID, .after = Stimulus) 
## Sample a random ID from the key file
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  group_by(Gorilla_ID) %>% # For each Gorilla ID,
  # Check whether we are at the right row again in the data:
  mutate(Gorilla_ID_new  = ifelse(grepl('https://research.sc/task/', Stimulus), # If you find this pattern in Stimulus
                                  # Sample a random ID from the key file
                                  sample(id_key_file_study_2$Participant_ID, size = 1, replace = FALSE),
                                  NA)) %>% # If you don't find the pattern, just return NA
  ungroup() %>% 
  select(-Gorilla_ID) %>% # Remove the original Gorilla ID
  rename(Gorilla_ID = Gorilla_ID_new) %>% # And assign the original name to the newly created ID
  relocate(Gorilla_ID, .after = Stimulus)

## Remove the Gorilla ID from the Stimulus and paste the new ID there
artificial_eye_tracking_raw_data <- artificial_eye_tracking_raw_data %>% 
  group_by(Stimulus) %>% # For each Stimulus,
  mutate(Stimulus_Temp = ifelse(grepl('https://research.sc/task/', Stimulus), # See if you find this pattern in Stimulus
                                str_extract(Stimulus, 'https://research.sc/task/'), # If so, remove the original IDs
                                # If you don't just return the Stimulus you find (as character so we return the same data type across cases)
                                as.character(Stimulus))) %>% 
  # Also modify Stimulus by
  mutate(Stimulus  = ifelse(grepl('https://research.sc/task/', Stimulus), # checking whether this pattern is in this instance of Stimulus
                            paste0(Stimulus_Temp, Gorilla_ID), # If so, paste the just created Stimulus with the new Gorilla ID
                            Stimulus)) %>% # If you don't see this pattern in Stimulus, just print the Stimulus again (leave it as it is).
  relocate(Gorilla_ID, .after = Stimulus) %>% # Relocate the new Gorilla ID after Stimulus
  arrange(Participant, Trial) # and sort the data based on participants and their trials

##    Write all data files                                                     ----
#write_delim(artificial_eye_tracking_raw_data, file = paste0(rel_path_raw_eye_tracking_data_output, "artificial_eye_tracking_raw_data.txt"))
## 2. ------------ Simulate the Gorilla.sc Raw Data -------------------------- ----
##    Load the real Gorilla raw data                                           ----
# Read in all raw data files in the raw data file directory                
raw_data_list_study_1 <- load_all_raw_data_files(rel_path_gorilla_raw_data_study_study_1, "task", "csv")
raw_data_list_study_2 <- load_set_of_specific_csv_files(rel_path_gorilla_raw_data_study_study_2, "task")
id_key_file_both_studies <- read_delim(rel_path_ids_both_studies) %>%
  select(Participant_ID, Participant_ID_Random)

##    Simulate Gorilla (performance) Raw Data                                  ----

# Select columns that are needed during the data cleaning                     
select_relevant_columns <- function(incoming_Data) {
  incoming_Data <- incoming_Data %>% 
    select( # select the column if the following patterns are part of the column names of the incoming data
      c(grep("Event|Local|Participant|Screen|Zone|Reaction|Response|display|Trial|Attempt|Spreadsheet|randomiser", 
             colnames(incoming_Data))))
  incoming_Data
}
# Apply the column selection function to all data files in the list
artificial_data_list_study_1 <- lapply(raw_data_list_study_1, function(x) select_relevant_columns(x))
artificial_data_list_study_2 <- lapply(raw_data_list_study_2, function(x) select_relevant_columns(x))

## Create artificial gorilla data
create_artificial_gorilla_data <- function(incoming_data) {
  ## Align all the column names by removing the white spaces
  incoming_data <- remove_spaces_from_colum_names(incoming_data)
  ## Filter out the END OF FILE instances to add them back later on
  artificial_raw_data <- incoming_data %>% # Remove them from the data to be modified
    filter(!grepl('TASK', Trial_Number) | grepl('FILE', Event_Index))
  end_of_task_rows <- incoming_data %>% # Save them so they can be merged back later on
    filter(grepl('TASK', Trial_Number) | grepl('FILE', Event_Index))
  
  ## Save possible Monitor sizes, desktop sizes, browsers, etc.
  # (Save the unique possible values, and then save the values again without the missings)
  possible_monitor_sizes <- unique(incoming_data$Participant_Monitor_Size)
  possible_monitor_sizes <- possible_monitor_sizes[possible_monitor_sizes != ""]
  possible_participant_OS <- unique(incoming_data$Participant_OS)
  possible_participant_OS <- possible_participant_OS[possible_participant_OS != ""]
  possible_participant_browser <- unique(incoming_data$Participant_Browser)
  possible_participant_browser <- possible_participant_browser[possible_participant_browser != ""]
  possible_participant_viewport_size <- unique(incoming_data$Participant_Viewport_Size)
  possible_participant_viewport_size <- possible_participant_viewport_size[possible_participant_viewport_size != ""]
  # Reassign those retrieved values randomly
  incoming_data <- incoming_data %>%
    group_by(Participant_Private_ID) %>%
    mutate(Participant_Monitor_Size = sample(possible_monitor_sizes, size = 1)) %>%
    mutate(Participant_OS = sample(possible_participant_OS, size = 1)) %>%
    mutate(Participant_Browser = sample(possible_participant_browser, size = 1)) %>%
    mutate(Participant_Viewport_Size = sample(possible_participant_viewport_size, size = 1)) 
  
  ## Create a new random date time variable
  # Retrieve and save all real year and month combinations of the data collection as possible options
  years_months_data_collection <- unique(str_sub(incoming_data$Local_Date,4,10))
  years_months_data_collection <- years_months_data_collection[years_months_data_collection != ""]
  # Then create vectors with possible days, hours, minutes and seconds
  possible_days <- c(paste0(0, seq(from = 1, to = 9)), seq(from = 10, to = 30))
  possible_hours <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 23))
  possible_minutes_and_seconds <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 59)) # 55 so it would not go over 60
  # Create the Day/Month/Year Hours: (e.g. 03/03/2021 21:) to start with
  incoming_data <- incoming_data %>%
    ungroup() %>%
    group_by(Participant_Private_ID) %>% # For each participant ID,
    # Paste a random selection of 
    mutate(New_Time = paste0(sample(possible_days, size = 1), '/', # a possible day
                             sample(years_months_data_collection, size = 1), ' ', # a possible month and year
                             sample(possible_hours, size = 1))) %>% # and a possible hour
    relocate(New_Time, .after = Local_Date)
  # Create the seconds by grouping based on the new time variable per participant
  distinct_id_time <- incoming_data %>%
    distinct(Participant_Private_ID, New_Time) %>% # Only keep one instance of the new time variable per participant ID.
    group_by(Participant_Private_ID) %>% # Then for each participant ID,
    # Create a minutes variable by creating a sequence starting from a randomly sampled value from possible_minutes_and_seconds 
    mutate(Minutes_To_Remove = seq(from = as.numeric(sample(possible_minutes_and_seconds, size = 1)), 
                                   length.out = length(Participant_Private_ID))) %>%  # with the length of the participant ID.
    mutate(New_Time = ifelse(Minutes_To_Remove <= 9, # Create yet a new variable by checking whether the new minute variable is <= 9
                             paste0(New_Time,':0', Minutes_To_Remove), # If that's the case, add a : and a 0 before the number,
                             paste0(New_Time,':', Minutes_To_Remove))) %>% # otherwise only add a :
    select(-Minutes_To_Remove) # Remove the minute variable again
  # Merge this frame back to the data
  incoming_data <- full_join(distinct_id_time, incoming_data, by = join_by(Participant_Private_ID)) %>%
    select(-New_Time.y) %>% # Remove the old time variable
    rename(New_Time = New_Time.x) %>% # Rename the new variable by giving it the name of the original one
    arrange(Participant_Private_ID, Event_Index) %>% # Sort the data based on participant ID and event index
    group_by(Participant_Private_ID) %>% # Then re-group the data again based on participant IDs.
    # For each participant ID, create a sequence of seconds by randomly sampling a first value from the possible seconds vector
    mutate(Seconds_To_Merge = seq(from = sample(1:10, size = 1), 
                                  # and make the sequence as long as the participant ID at hand.
                                  length.out = length(Participant_Private_ID))) %>% 
    # Again, if the seconds are only one digit, 
    mutate(Seconds_To_Merge = as.character(ifelse(Seconds_To_Merge < 10,
                                                  paste0('0',Seconds_To_Merge), # add a 0 to the number,
                                                  Seconds_To_Merge))) %>% # otherwise leave it as it is.
    mutate(New_Time = paste0(New_Time, ':', Seconds_To_Merge)) %>% # Create the new variable by pasting the new variables together with a :
    select(-c(Local_Date, Seconds_To_Merge)) %>% # Remove and unnecessary variables,
    rename(Local_Date = New_Time) # and rename the new ones with the original names
  ## Transform the newly created time variables to be actual time variables (as data format)
  incoming_data <- incoming_data %>%
    mutate(UTC_Timestamp =  as.numeric(as.POSIXct(Local_Date, format="%d/%m/%Y %H:%M:%S"))) %>% 
    mutate(Local_Timestamp =  as.numeric(as.POSIXct(Local_Date, format="%d/%m/%Y %H:%M:%S"))) %>% 
    mutate(UTC_Date = Local_Date)
  rm(distinct_id_time)
  
  ## Create artificial reaction times grouped by Screen Name and display (i.e., trial type)
  artificial_raw_data$Reaction_Time <- as.numeric(artificial_raw_data$Reaction_Time)
  artificial_raw_data <- artificial_raw_data %>% 
    ungroup() %>%
    mutate(Reaction_Time = call_generator_if_not_na(Reaction_Time)) # call function
  
  ## Create artificial data for the different types of responses (all stored in "Response")
  # By filtering the responses by type
  jol_responses <- artificial_raw_data %>% ungroup() %>% filter(grepl("JoL", Screen_Name)) 
  leftover_data <- artificial_raw_data %>% ungroup() %>% filter(!grepl("JoL", Screen_Name)) 
  leftover_data <- leftover_data %>% filter(!grepl("buttonA|DiagBox", Zone_Name))
  # Change the Diagram and Test answers
  string_response_data <- artificial_raw_data %>% ungroup() %>%
    filter(grepl("buttonA|DiagBox", Zone_Name)) %>% 
    mutate(Word_Count = str_count(Response, '\\w+')) %>% rowwise() %>%
    mutate(Response = ifelse(Word_Count > 0, 
                             stri_rand_strings(Word_Count, sample(2:10, 1, replace=TRUE)),
                             "?")) %>% select(-Word_Count)
  # Change the JOL answers
  jol_missings <- jol_responses %>% filter(is.na(as.numeric(Response)))
  jol_responses <- jol_responses %>% filter(!(is.na(as.numeric(Response))))
  jol_responses$Response <- as.numeric(jol_responses$Response)
  jol_responses <- jol_responses %>%
    mutate(Response = ifelse(all(is.na(Response)),
                             Response,
                             generate_numeric_data(Response)))
  # Merge all the responses back together
  artificial_response_data <- rbind(string_response_data, jol_responses, jol_missings, leftover_data, end_of_task_rows)
  artificial_response_data <- artificial_response_data %>% group_by(Participant_Private_ID) %>%
    arrange(Event_Index, .by_group = TRUE)
  
  ## Create participant IDs based on the key file with random IDs so they are the same across data sets
  # Save the missing IDs first 
  missing_ids <- artificial_response_data %>% 
    filter(is.na(Participant_Private_ID))
  artificial_response_data <- artificial_response_data %>% 
    # Remove the missing data from the data set
    filter(!(is.na(Participant_Private_ID))) %>% 
    # (ungroup the data,) 
    ungroup() %>%
    # Briefly align the name of the Participant_ID variable with the column in the key file
    rename(Participant_ID = Participant_Private_ID) 
  # Merge the key-file to the data 
  artificial_response_data <- left_join(artificial_response_data, id_key_file_both_studies) %>%
    # Remove the real Participant IDs
    select(-Participant_ID) %>%
    # Assign the proper name to the new random IDs
    rename(Participant_Private_ID = Participant_ID_Random) %>%
    # Relocate the new variable to it's proper place
    relocate(Participant_Private_ID, .after = Participant_Public_ID)
  # Append the missings again
  outgoing_data <- rbind(artificial_response_data, missing_ids)
  outgoing_data <- outgoing_data %>% 
    group_by(Participant_Private_ID) %>%
    arrange(Event_Index, .by_group = TRUE)
  outgoing_data
}

# Apply the function to create artificial gorilla data to all files in the list
artificial_data_list_study_1 <- lapply(artificial_data_list_study_1, function(x) create_artificial_gorilla_data(x))
artificial_data_list_study_2 <- lapply(artificial_data_list_study_2, function(x) create_artificial_gorilla_data(x))

# Add "artificial" to data set names and load them into the workspace       
names(artificial_data_list_study_1) <- paste0("study_1_artificial_task_", names(artificial_data_list_study_1))
names(artificial_data_list_study_2) <- paste0("study_2_artificial_task_", names(artificial_data_list_study_2))

## For checking: load all files from the lists into the global environment/ current workspace 
# list2env(artificial_data_list_study_1, .GlobalEnv)
# list2env(artificial_data_list_study_2, .GlobalEnv)

##    Write all artificial data files in the list to the output directory      ----
#mapply(function (x,y) write.csv(x, file = paste0(rel_path_raw_performance_data_output,'Study_1/', y, '.csv'), row.names = F), artificial_data_list_study_1, names(artificial_data_list_study_1))  
#mapply(function (x,y) write.csv(x, file = paste0(rel_path_raw_performance_data_output,'Study_2/', y, '.csv'), row.names = F), artificial_data_list_study_2, names(artificial_data_list_study_2))  

## 3. ------------ Simulate Data for Analysis Script Study 1 ----------------- ----
##    Load the real processed performance data                                 ----
# First store files of the given directory into a list
processed_performance_data_list <- load_all_txt_files(rel_path_processed_performance_data)
# Remove the 'artificial' from the file names
names(processed_performance_data_list) <- gsub('artificial_', '', names(processed_performance_data_list))

# Then load all files from the list into the global environment/ current workspace
list2env(processed_performance_data_list, .GlobalEnv)

## Modify the key file with the random ids based on the data that it needs to be merged with
# To only cotain the Participant IDs
all_participant_ids <- all_IDs_random_to_random %>%
  select(Participant_ID, Participant_ID_Random)

## To only contain the Participants, not trial IDs
all_ids_participants_across_trials <- all_IDs_random_to_random %>%
  distinct(Participant, Participant_Random) 

##    Simulate cleaned Gorilla (performance) data for analysis                 ----
## Merge Key file to the diagram data
artificial_diagram_data <- right_join(all_participant_ids, diagram_data) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-Participant_ID) %>% 
  rename(Participant_ID = Participant_ID_Random) %>% 
  relocate(Experiment)

## Create random strings as responses again based on the Word_Count
# First shuffle the word count
artificial_diagram_data$Word_Count <- sample(artificial_diagram_data$Word_Count)
# Then create the random responses
artificial_diagram_data <- artificial_diagram_data %>%
  rowwise() %>%
  mutate(Response = ifelse(Word_Count == 0,
                           "?",
                           paste(replicate(n = Word_Count, 
                                           stri_rand_strings(1, length = sample(3:6, size = 1), 
                                                             pattern = "[A-Za-z0-9]")), 
                                 collapse = " "))) %>%
  relocate(Response, .after = Zone_Name)

# Then save a subset of non-omissions and shuffle the values per condition
artificial_diagram_data_non_omissions <- artificial_diagram_data %>%
  filter(Word_Count != 0) %>%
  group_by(Condition) %>%
  mutate(Correct_Points = sample(Correct_Points),
         Element_Number = sample(Element_Number),
         Commission = sample(Commission))

# Create the Coded Diagram Cues based on the shuffled random string responses
artificial_diagram_data_omissions <- artificial_diagram_data %>%
  filter(Word_Count == 0) %>%
  mutate(Omission = ifelse(Word_Count == 0,
                           1,
                           0),
         Correct_Points = ifelse(Word_Count == 0,
                                 0,
                                 1),
         Commission = ifelse(Word_Count == 0,
                             0,
                             1),
         Element_Number = ifelse(Word_Count == 0,
                                 NA,
                                 Element_Number))

artificial_diagram_data <- full_join(artificial_diagram_data_non_omissions, artificial_diagram_data_omissions) %>%
  arrange(Experiment, Condition, Participant_ID, Trial, Zone_Name)
# Clean up the workspace to keep a better overview of the created data sets
rm(artificial_diagram_data_non_omissions, artificial_diagram_data_omissions)

##    Simulate the inter-rater-reliability data for the diagrams               ----
## Merge Key file to the diagram data
artificial_irr_diagrams_both_coders <- right_join(all_participant_ids, irr_diagrams_both_coders) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-Participant_ID) %>% rename(Participant_ID = Participant_ID_Random) %>%
  # Create a Word Count variable to randomize the responses and replace them by random strings
  mutate(Word_Count = str_count(Response, '\\w+')) %>% 
  # Relocate the new word count variable
  relocate(Word_Count, .after = Response) %>% 
  # Randomize the Word Count variable
  mutate(Word_Count = sample(Word_Count)) %>% 
  # Generate random response strings based on the randomized word count
  rowwise() %>%
  mutate(Response = ifelse(Word_Count == 0,
                           "?",
                           paste(replicate(n = Word_Count, 
                                           stri_rand_strings(1, length = sample(3:6, size = 1), 
                                                             pattern = "[A-Za-z0-9]")), 
                                 collapse = " "))) %>%
  relocate(Response, .after = Position)

## Split data based on matching and non matching instances across coders
# Filter matching code instances and modify values
artificial_irr_diagrams_both_coders_matching <- artificial_irr_diagrams_both_coders %>%
  ungroup() %>%
  filter(Correct_Coder_1 == Correct_Coder_2 & 
           Element_Number_Coder_1 == Element_Number_Coder_2 & 
           is.na(Element_Number_Coder_1) == is.na(Element_Number_Coder_2) & 
           Commission_Coder_1 == Commission_Coder_2) 

# Filter the non-matching instances with anti join, which returns 
# all rows from artificial_irr_diagrams_both_coders without a match in artificial_irr_diagrams_both_coders_matching.
artificial_irr_diagrams_correct_non_matching <- anti_join(artificial_irr_diagrams_both_coders, artificial_irr_diagrams_both_coders_matching)

# Save possible Element Numbers
possible_element_numbers <- unique(c(artificial_irr_diagrams_both_coders$Element_Number_Coder_1, artificial_irr_diagrams_both_coders$Element_Number_Coder_2))

## Shuffle and randomize the values for the matching instances
artificial_irr_diagrams_both_coders_matching <- artificial_irr_diagrams_both_coders_matching %>%
  mutate(Correct_Coder_1 = sample(0:4, size = 1)) %>%
  mutate(Element_Number_Coder_1 = sample(possible_element_numbers, size = 1)) %>%
  mutate(Commission_Coder_1 = sample(0:4, size = 1)) %>%
  mutate(Correct_Coder_2 = Correct_Coder_1) %>%
  mutate(Element_Number_Coder_2 = Element_Number_Coder_1) %>%
  mutate(Commission_Coder_2 = Commission_Coder_1) 

## Shuffle and randomize the values for the non matching instances
artificial_irr_diagrams_correct_non_matching <- artificial_irr_diagrams_correct_non_matching %>%
  rowwise() %>%
  mutate(Correct_Coder_1 = ifelse(Correct_Coder_1 != Correct_Coder_2,
                                  sample(0:4, size = 1),
                                  Correct_Coder_1)) %>%
  mutate(Correct_Coder_2 = ifelse(Correct_Coder_1 != Correct_Coder_2,
                                  sample(0:4, size = 1),
                                  Correct_Coder_2)) %>%
  mutate(Element_Number_Coder_1 = ifelse(Element_Number_Coder_1 != Element_Number_Coder_2,
                                  sample(possible_element_numbers, size = 1),
                                  Element_Number_Coder_1)) %>%
  mutate(Element_Number_Coder_2 = ifelse(Element_Number_Coder_1 != Element_Number_Coder_2,
                                  sample(possible_element_numbers, size = 1),
                                  Element_Number_Coder_2)) %>%
  mutate(Commission_Coder_1 = ifelse(Commission_Coder_1 != Commission_Coder_2,
                                     sample(0:4, size = 1),
                                     Commission_Coder_1)) %>%
  mutate(Commission_Coder_2 = ifelse(Commission_Coder_1 != Commission_Coder_2,
                                     sample(0:4, size = 1),
                                     Commission_Coder_2)) 


artificial_irr_diagrams_both_coders <- full_join(artificial_irr_diagrams_both_coders_matching, artificial_irr_diagrams_correct_non_matching)

## Clean up the workspace again
rm(artificial_irr_diagrams_both_coders_matching, artificial_irr_diagrams_correct_non_matching)

##    Simulate the inter-rater-reliability data for the tests                  ----
## Merge Key file to the irr test data
artificial_irr_tests_both_coders <- right_join(all_participant_ids, irr_tests_both_coders) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-Participant_ID) %>% rename(Participant_ID = Participant_ID_Random) %>%
  # Create a Word Count variable to randomize the responses and replace them by random strings
  mutate(Word_Count = str_count(Response, '\\w+')) %>% 
  # Randomize the Word Count variable
  mutate(Word_Count = sample(Word_Count)) %>% 
  # Generate random response strings based on the randomized word count
  rowwise() %>%
  mutate(Response = ifelse(Word_Count == 0,
                           "?",
                           paste(replicate(n = Word_Count, 
                                           stri_rand_strings(1, length = sample(3:6, size = 1), 
                                                             pattern = "[A-Za-z0-9]")), 
                                 collapse = " "))) %>%
  relocate(Response, .after = Trial) %>%
  rowwise() %>%
  mutate(across(.col = !c(Participant_ID, Trial, Response), ~ifelse(Word_Count == 0, 0 , .)))
  
## Split the data into matching and non-matching instances
artificial_irr_tests_both_coders_matching <- artificial_irr_tests_both_coders %>%
  filter(Element_1_Coder_1 == Element_1_Coder_2 & 
           Element_2_Coder_1 == Element_2_Coder_2 &
           Element_3_Coder_1 == Element_3_Coder_2 &
           Element_4_Coder_1 == Element_4_Coder_2 &
           Element_5_Coder_1 == Element_5_Coder_2 &
           Comission_Coder_1 == Comission_Coder_2 &
           Total_Score_Coder_1 == Total_Score_Coder_2)
artificial_irr_tests_both_coders_not_matching <- anti_join(artificial_irr_tests_both_coders, artificial_irr_tests_both_coders_matching)

## Generate random values in the data set with matching coder codes
artificial_irr_tests_both_coders_matching <- artificial_irr_tests_both_coders_matching %>%
  ungroup() %>%
  mutate(Total_Score_Coder_1 = generate_numeric_data(Total_Score_Coder_1)) %>%
  mutate(Total_Score_Coder_2 = Total_Score_Coder_1) %>%
  rowwise() %>%
  mutate(Element_1_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_2_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_3_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_4_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_5_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_1_Coder_2 = Element_1_Coder_1) %>%
  mutate(Element_2_Coder_2 = Element_2_Coder_1) %>%
  mutate(Element_3_Coder_2 = Element_3_Coder_1) %>%
  mutate(Element_4_Coder_2 = Element_4_Coder_1) %>%
  mutate(Element_5_Coder_2 = Element_5_Coder_1) %>%
  select(-Word_Count)
  
## Modify the data of the non matching code data
artificial_irr_tests_both_coders_not_matching <- artificial_irr_tests_both_coders_not_matching %>%
  ungroup() %>%
  mutate(Total_Score_Coder_1 = generate_numeric_data(Total_Score_Coder_1)) %>%
  mutate(Total_Score_Coder_2 = generate_numeric_data(Total_Score_Coder_2)) %>%
  rowwise() %>%
  mutate(Element_1_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_2_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_3_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_4_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_5_Coder_1 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_1_Coder_2 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_2_Coder_2 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_3_Coder_2 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_4_Coder_2 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  mutate(Element_5_Coder_2 = ifelse(Word_Count == 0, 0, sample(0:4, size = 1))) %>%
  select(-Word_Count)

artificial_irr_tests_both_coders <- full_join(artificial_irr_tests_both_coders_matching, artificial_irr_tests_both_coders_not_matching)
rm(artificial_irr_tests_both_coders_matching, artificial_irr_tests_both_coders_not_matching)

##    Simulate the JOL and test data of study 1 and 2                          ----
## Add the artificial IDs to the data, with one ID per participant, 
#     so careful with IDs study 2, where there was one ID per trial and six trials per participant
# Merge the key files to the data 
artificial_jol_and_test_data <- inner_join(all_ids_participants_across_trials, jol_and_test_data) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-Participant) %>% rename(Participant = Participant_Random)

## Generate artificial data per condition
artificial_jol_and_test_data <- artificial_jol_and_test_data %>%
  group_by(Experiment, Condition) %>%
  mutate(across(.cols = c(JoL1:Test), generate_numeric_data))

##    Simulate the processed performance data                                  ----  
## Add the artificial IDs to the data, with one ID per participant, 
#     so careful with IDs study 2, where there was one ID per trial and six trials per participant
# Merge the key files to the data
processed_performance_data <- processed_performance_data %>% select(-Participant)
artificial_processed_performance_data <- right_join(all_IDs_random_to_random, processed_performance_data) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-c(Participant_ID, Participant)) %>% rename(Participant = Participant_Random,
                                                     Participant_ID = Participant_ID_Random) %>%
  relocate(Experiment) # More Experiment to the front

# Save possible Monitor sizes 
possible_monitor_sizes <- unique(artificial_processed_performance_data$Participant_Monitor_Size)
# and reassign values randomly
artificial_processed_performance_data <- artificial_processed_performance_data %>%
  group_by(Participant) %>%
  mutate(Participant_Monitor_Size = sample(possible_monitor_sizes, size = 1))

## Create a new random date time variable
years_months_data_collection <- unique(str_sub(artificial_processed_performance_data$Local_Date,1,7))
possible_days <- c(paste0(0, seq(from = 1, to = 9)), seq(from = 10, to = 30))
possible_hours <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 23))
possible_minutes_and_seconds <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 59)) # 55 so it would not go over 60
# Create the Year-Month-Day Hours (e.g. 2020-11-10 03) to start with
artificial_processed_performance_data <- artificial_processed_performance_data %>%
  ungroup() %>%
  group_by(Participant_ID) %>%
  mutate(New_Time = paste0( unique(str_sub(sample(years_months_data_collection, size = 1),1,7)), '-', sample(possible_days, size = 1), ' ', sample(possible_hours, size = 1))) %>%
  relocate(New_Time, .after = Local_Date)
# Create the seconds by grouping based on the new time variable per participant
distinct_id_time <- artificial_processed_performance_data %>%
  distinct(Participant_ID, New_Time, Trial) %>%
  group_by(Participant_ID) %>%
  mutate(Minutes_To_Remove = seq(from = as.numeric(sample(possible_minutes_and_seconds, size = 1)), length.out = length(Participant_ID))) %>%
  mutate(New_Time = ifelse(Minutes_To_Remove <= 9,
                          paste0(New_Time,':0', Minutes_To_Remove),
                          paste0(New_Time,':', Minutes_To_Remove))) %>%
  select(-Minutes_To_Remove)
# ..and merging this frame back to the data
artificial_processed_performance_data <- full_join(distinct_id_time, artificial_processed_performance_data, by = join_by(Participant_ID, Trial)) %>%
  select(-New_Time.y) %>%
  rename(New_Time = New_Time.x) %>%
  arrange(Experiment, Participant_ID, Event_Index) %>%
  group_by(Participant_ID) %>%
  mutate(Seconds_To_Merge = seq(from = sample(1:10, size = 1), length.out = length(Participant_ID))) %>%
  mutate(Seconds_To_Merge = as.character(ifelse(Seconds_To_Merge < 10,
                                   paste0('0',Seconds_To_Merge),
                                   Seconds_To_Merge))) %>%
  mutate(New_Time = paste0(New_Time, ':', Seconds_To_Merge)) %>%
  select(-c(Local_Date, Seconds_To_Merge)) %>% rename(Local_Date = New_Time)
rm(distinct_id_time)

## Modify Response by filtering based on difference Screen Names
# Filter Numeric Scores
artificial_processed_performance_data_NAs <- artificial_processed_performance_data %>%
  filter(is.na(Response))
artificial_processed_performance_data_Numeric <- artificial_processed_performance_data %>%
  filter(grepl('JoL|PictureMatching', Screen_Name) & !is.na(Response))
# Filter Response Strings
artificial_processed_performance_data_Strings <- artificial_processed_performance_data %>%
  filter(grepl('Test|Diagram', Screen_Name) & !is.na(Response))

## Modify the Numeric Score Data
artificial_processed_performance_data_Numeric <- artificial_processed_performance_data_Numeric %>%
  group_by(Condition, Screen_Name) %>%
  mutate(Response = generate_numeric_data(Response)) %>%
  mutate(Zone_Name = ifelse(Screen_Name == 'PictureMatching',
                            Zone_Name,
                            case_match(Response,
                                       '0' ~ 'Zero',
                                       '1' ~ 'One',
                                       '2' ~ 'Two',
                                       '3' ~ 'Three',
                                       '4' ~ 'Four')))
           
## Modify the Response Strings
artificial_processed_performance_data_Strings <- artificial_processed_performance_data_Strings %>%
  ungroup() %>%
  mutate(Word_Count = str_count(Response, '\\w+')) %>%
  rowwise() %>%
  mutate(Response = paste(replicate(n = Word_Count, 
                                    stri_rand_strings(1,
                                                      length = sample(3:6, size = 1), 
                                                      pattern = "[A-Za-z0-9]")), 
                          collapse = " ")) %>%
  select(-Word_Count) 

## Join the two modified data sets again
artificial_processed_performance_data_new_responses <- full_join(artificial_processed_performance_data_Strings, artificial_processed_performance_data_Numeric)
artificial_processed_performance_data <- full_join(artificial_processed_performance_data_NAs, artificial_processed_performance_data_new_responses) %>%
  arrange(Experiment, Participant_ID, Event_Index)
rm(artificial_processed_performance_data_new_responses, artificial_processed_performance_data_Strings, artificial_processed_performance_data_Numeric, artificial_processed_performance_data_NAs)

## Create reaction time based on Experiment, Condition and Screen Name
artificial_processed_performance_data <- artificial_processed_performance_data %>%
  group_by(Experiment, Condition, Screen_Name) %>%
  mutate(Reaction_Time = generate_numeric_data(Reaction_Time))
# Create time on task based on condition
artificial_processed_performance_data_time_on_task <- artificial_processed_performance_data %>%
  ungroup %>%
  distinct(Condition, Participant_ID, Time_On_Task) %>%
  group_by(Condition) %>%
  mutate(Time_On_Task_New = generate_numeric_data(Time_On_Task)) 
# Merge this set back to the main data
artificial_processed_performance_data <- left_join(artificial_processed_performance_data, artificial_processed_performance_data_time_on_task)
rm(artificial_processed_performance_data_time_on_task)

##    Simulate the processed sample stats study 1                              ----
## Merge Key file to the diagram data
artificial_processed_sample_data <- right_join(all_participant_ids, processed_sample_data_exp_1) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-Participant_ID) %>% rename(Participant_ID = Participant_ID_Random) %>%
  relocate(Sex, .after = age) %>%
  # Modify the numeric columns first
  group_by(Condition) %>%
  mutate(across(.cols = c(time_taken_min:age), generate_numeric_data)) %>%
  # Then also shuffel the gender
  ungroup() %>%
  mutate(Sex = sample(Sex))

## Regenerate the random strings of the session ids
artificial_processed_sample_data <- artificial_processed_sample_data %>%
  ungroup() %>%
  mutate(session_id = generate_non_numeric_IDs(session_id) )

## Create a new random date time variable
years_months_data_collection <- unique(str_sub(artificial_processed_sample_data$started_datetime,1,7))
possible_days <- c(paste0(0, seq(from = 1, to = 9)), seq(from = 10, to = 30))
possible_hours <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 23))
possible_minutes_and_seconds <- c(paste0(0, seq(from = 0, to = 9)), seq(from = 10, to = 59)) # 55 so it would not go over 60
# Create the Year-Month-Day Hours (e.g. 2020-11-10 03) to start with
artificial_processed_sample_data <- artificial_processed_sample_data %>%
  ungroup() %>%
  group_by(Participant_ID) %>%
  mutate(started_datetime = paste0(unique(str_sub(sample(years_months_data_collection, size = 1),1,7)), 
                           '-', 
                           sample(possible_days, size = 1), 
                           ' ', 
                           sample(possible_hours, size = 1),
                           ':',
                           sample(possible_minutes_and_seconds, size = 1),
                           ':',
                           sample(possible_minutes_and_seconds, size = 1))) %>%
  type.convert(as.is=TRUE) %>% 
  mutate(started_datetime = ymd_hms(started_datetime)) %>%
  mutate(time_taken_sec = lubridate::seconds(time_taken_sec)) %>%
  mutate(completed_date_time = started_datetime + time_taken_sec)
##    Write all data files                                                     ----
#write_delim(artificial_diagram_data, file = paste0(rel_path_processed_performance_data_output , "artificial_diagram_data.txt"))
#write_delim(artificial_irr_diagrams_both_coders, file = paste0(rel_path_processed_performance_data_output , "artificial_irr_diagrams_both_coders.txt"))
#write_delim(artificial_irr_tests_both_coders, file = paste0(rel_path_processed_performance_data_output , "artificial_irr_tests_both_coders.txt"))
#write_delim(artificial_jol_and_test_data, file = paste0(rel_path_processed_performance_data_output , "artificial_jol_and_test_data.txt"))
#write_delim(artificial_processed_performance_data, file = paste0(rel_path_processed_performance_data_output , "artificial_processed_performance_data.txt"))
#write_delim(artificial_processed_sample_data, file = paste0(rel_path_processed_performance_data_output , "artificial_processed_sample_data.txt"))
## 4. ------------ Simulate Data for Analysis Script Study 2 ----------------- ----
##    Load all real data necessary for the eye-tracking analyses script/Study 2----

# First store files of the given directory into a list
processed_gaze_data_list <- load_all_txt_files(rel_path_processed_gaze_data)
# Remove the 'artificial' from the file names
names(processed_gaze_data_list) <- gsub('artificial_', '', names(processed_gaze_data_list))
# Then load all files from the list into the global environment/ current workspace
list2env(processed_gaze_data_list, .GlobalEnv)

## Create an extra version of the key file without the Gorilla IDs 
key_file_for_artificial_ID_matching_red <- all_IDs_random_to_random %>%
  filter(Experiment == 2) %>%
  rename(Gorilla_ID = Participant_ID, 
         Gorilla_ID_new = Participant_ID_Random)

##    Simulate age and gender                                                  ----
## Re-code Participant ID to P01 to P23 and randomly assign those IDs
no_ids_age_gender_study_2 <- length(age_gender_study_2$Participant)
new_participant_IDs_age_gender_2 <- c(rep(paste0('P0', 1:9)), # first generate P01 to P09, then
                                      rep(paste0('P', 10:no_ids_age_gender_study_2))) # generate the rest (without 0's)
artificial_age_gender_study_2 <- age_gender_study_2
artificial_age_gender_study_2$Participant <- sample(new_participant_IDs_age_gender_2)
# Shuffle the gender
shuffled_gender_2_indices <- sample(length(age_gender_study_2$Gender))
# Re-assign values based on shuffled indices
artificial_age_gender_study_2$Gender <- age_gender_study_2$Gender[shuffled_gender_2_indices]
## Randomly sample values between 18 and 22 and assign them as new Age values
artificial_age_gender_study_2$Age <- sample(18:22, size = length(age_gender_study_2$Age), replace = TRUE)

##    Simulate validation results of eye-tracker                               ----
## Match real ID with artificial ID
## As its only about the existence of the same IDs, shuffle them first before merging
key_file_for_artificial_ID_matching_validation <- key_file_for_artificial_ID_matching_red
key_file_for_artificial_ID_matching_validation$Participant_Random <- sample(key_file_for_artificial_ID_matching_validation$Participant_Random) 
key_file_for_artificial_ID_matching_validation <- key_file_for_artificial_ID_matching_validation %>% 
  distinct(Participant, .keep_all = TRUE)

# Prepare the validation data for merging
artificial_validation_result_overview <- validation_results_overview 
# Merge the artificial IDs to the data
artificial_validation_result_overview <- right_join(key_file_for_artificial_ID_matching_validation, artificial_validation_result_overview) %>%
  select(-c(Participant, starts_with('Gorilla')) )%>%
  rename(Participant = Participant_Random)
  
# Change the columns values
artificial_validation_result_overview <- artificial_validation_result_overview %>% 
  group_by(Participant) %>% 
  mutate(across(starts_with("Right"), ~ ifelse(!is.na(.), generate_numeric_data(.), NA)))

##    Simulate calibration values of eye-tracker                               ----
## Adapt the IDs again
# Use the reduced key file created after loading and shuffle the IDs (again)
key_file_for_artificial_ID_matching_calibration <- key_file_for_artificial_ID_matching_red
key_file_for_artificial_ID_matching_calibration$Participant_Random <- sample(key_file_for_artificial_ID_matching_calibration$Participant_Random)
key_file_for_artificial_ID_matching_calibration <- key_file_for_artificial_ID_matching_calibration %>% 
  distinct(Participant, .keep_all = TRUE)
# Merge the artificial IDs to the data
artificial_calibration_data <- right_join(key_file_for_artificial_ID_matching_calibration, calibration_data) %>%
  select(-c(Participant, starts_with('Gorilla'))) %>%
  rename(Participant = Participant_Random)

## Re-assign new values to the three numeric columns by 
# applying generate_numeric_data function to specified columns
artificial_calibration_data <- artificial_calibration_data %>% 
  mutate(across(.cols = c(Right_Eye_Deviation_X:Tracking_Ratio_Perc), generate_numeric_data)) %>%
  arrange(Participant)

##    Simulate the cleaned eye-tracking data for analysis                      ----
## Adapt the ID variables with the key file again
artificial_gaze_data_with_AOIs <- right_join(all_IDs_random_to_random, gaze_data_with_AOIs) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-c(Participant_ID, Participant)) %>% rename(Participant = Participant_Random,
                                                     Participant_ID = Participant_ID_Random) %>%
  relocate(Experiment) # More Experiment to the front

## Retrieve a sub-sample of the data as it's too big..
desired_observations <- round(dim(artificial_gaze_data_with_AOIs)[1]/4) # Adapt if desired
artificial_gaze_data_with_AOIs <- artificial_gaze_data_with_AOIs %>% ungroup() %>% slice_sample(n = desired_observations)

## Modify existing numeric variables by creating new values based on real parameters
columns_for_numeric_data_generation <- c("Pupil_Size_Right_X_px", 
                                         "Pupil_Size_Right_Y_px", 
                                         "Pupil_Diameter_Right_mm", 
                                         "Point_of_Regard_Right_X_px", 
                                         "Point_of_Regard_Right_Y_px", 
                                         "Gaze_Vector_Right_X", 
                                         "Gaze_Vector_Right_Y", 
                                         "Gaze_Vector_Right_Z", 
                                         "Eye_Position_Right_X_mm", 
                                         "Eye_Position_Right_Y_mm",
                                         "Eye_Position_Right_Z_mm", 
                                         "Pupil_Position_Right_X_px",
                                         "Pupil_Position_Right_Y_px") 

## Apply generate_numeric_data function to specified columns
artificial_gaze_data_with_AOIs <- artificial_gaze_data_with_AOIs %>% 
  mutate(across(.cols = all_of(columns_for_numeric_data_generation), generate_numeric_data))
# Transform all variables to the types they are
artificial_gaze_data_with_AOIs <- artificial_gaze_data_with_AOIs %>% 
  type.convert(as.is = TRUE) 

## Index_Right is actually a sequence of fixation indices. 
#  Sort the variable accordingly
artificial_gaze_data_with_AOIs <- artificial_gaze_data_with_AOIs %>% 
  group_by(Participant_ID) %>% 
  arrange(Index_Right) %>% 
  ungroup()

## Generate data for RecordingTime_ms, Tracking_Ratio, Export_End_Trial_Time_ms per participant
artificial_gaze_data_with_AOIs <- artificial_gaze_data_with_AOIs %>% 
  group_by(Participant_ID, Trial) %>%
  mutate(across(.cols = c(Recording_Time_ms, Tracking_Ratio, Export_End_Trial_Time_ms), generate_numeric_data)) %>%
  relocate(Recording_Time_ms, .after = Diagram_Type) %>%
  relocate(Tracking_Ratio, .after = Color) %>%
  relocate(Export_End_Trial_Time_ms, .after = Export_Start_Trial_Time_ms) 

## Code/lines to write output file in last section of this script

##    Simulate clean fixation point data for analysis                          ----
## Adapt the Participant ID by using the random key file, adapt the keys first
id_keys_for_fixation_data <- all_IDs_random_to_random %>%
  filter(Experiment == 2) %>%
  distinct(Participant, .keep_all = TRUE) %>% select(-Experiment)
# Create a new variable that has randomly a value of random ID
ids_fixation_point_data <- fixation_point_data %>% ungroup %>% distinct(Participant)
ids_fixation_point_data <- ids_fixation_point_data %>% ungroup() %>% 
  mutate(Participant_New = sample(id_keys_for_fixation_data$Participant_Random, size = length(Participant)))
# Merge this set to the main data file 
artificial_fixation_point_data <- full_join(ids_fixation_point_data, fixation_point_data) %>%
  # Remove the real ID and rename the generated IDs respectively
  select(-c(Participant)) %>% rename(Participant = Participant_New) 

## Modify existing numeric variables by creating new values based on real parameters
cols_4_numeric_data_creation_fixation_data <- c("Pupil_Size_Right_X_px", 
                                                "Pupil_Size_Right_Y_px", 
                                                "Pupil_Diameter_Right_mm", 
                                                "Point_of_Regard_Right_X_px", 
                                                "Point_of_Regard_Right_Y_px") 
## Apply generate_numeric_data function to specified columns
artificial_fixation_point_data <- artificial_fixation_point_data %>% 
  mutate(across(.cols = all_of(cols_4_numeric_data_creation_fixation_data), generate_numeric_data)) 

## Index_Right is actually a sequence of fixation indices. 
#  Sort the variable accordingly
artificial_fixation_point_data <- artificial_fixation_point_data %>% 
  group_by(Participant) %>% 
  arrange(Index_Right) %>% 
  ungroup()

## Generate data for RecordingTime_ms, Tracking_Ratio, Export_End_Trial_Time_ms per participant
artificial_fixation_point_data <- artificial_fixation_point_data %>% 
  group_by(Participant) %>%
  mutate(across(.cols = c(Recording_Time_ms, Tracking_Ratio, Export_End_Trial_Time_ms), generate_numeric_data))

##    Simulate the Gorilla coded diagram data matching the eye-tracking data   ----
## Adapt the ID variable by merging the Gorilla IDs from the key file 
id_file_4_diagram_data_2 <- all_IDs_random_to_random %>%
  filter(Experiment == 2) %>%
  select(starts_with('Participant_ID')) 
  
# Join the random key file to the diagram data 
artificial_diagram_data <- right_join(id_file_4_diagram_data_2, diagram_data) %>%
  relocate(Experiment) %>%
  arrange(Experiment) %>%
  select(-Participant_ID) %>%
  rename(Participant_ID = Participant_ID_Random)

## Create random strings as responses again based on the Word_Count
## First shuffle the word count
artificial_diagram_data$Word_Count <- sample(artificial_diagram_data$Word_Count)
artificial_diagram_data <- artificial_diagram_data %>%
  rowwise() %>%
  mutate(Response = ifelse(Word_Count == 0,
                               "?",
                               paste(replicate(n = Word_Count, 
                                               stri_rand_strings(1, length = sample(3:6, size = 1), 
                                                                 pattern = "[A-Za-z0-9]")), 
                                     collapse = " "))) %>%
  relocate(Response, .after = Zone_Name)

## Then save a subset of non-omissions and shuffle the values per condition
artificial_diagram_data_non_omissions <- artificial_diagram_data %>%
  filter(Word_Count != 0) %>%
  group_by(Condition) %>%
  mutate(Correct_Points = sample(Correct_Points),
         Element_Number = sample(Element_Number),
         Commission = sample(Commission))

## Create the Coded Diagram Cues based on the shuffled random string responses
artificial_diagram_data_omissions <- artificial_diagram_data %>%
  filter(Word_Count == 0) %>%
  mutate(Omission = ifelse(Word_Count == 0,
                           1,
                           0),
         Correct_Points = ifelse(Word_Count == 0,
                                 0,
                                 1),
         Commission = ifelse(Word_Count == 0,
                             0,
                             1),
         Element_Number = ifelse(Word_Count == 0,
                                 NA,
                                 Element_Number))
           
artificial_diagram_data <- full_join(artificial_diagram_data_non_omissions, artificial_diagram_data_omissions) %>%
  arrange(Experiment, Condition, Participant_ID, Trial, Zone_Name)

##    Simulate the IRR data for the segments and codes of the verbal reports   ----
## Randomize the IRR data for the segmentation
## Remove 'Opmerkingen'
retro_reports_irr_segmentation <- irr_segmentation %>%
  select(-starts_with('Opmerk')) %>%
  # And create a Word_Count variable for both segment variables
  mutate(Word_Count_Sophia = str_count(Segmenten_Sophia, '\\w+')) %>% 
  mutate(Word_Count_Jael = str_count(Segmenten_Jael, '\\w+')) 

artificial_irr_segmentation <- retro_reports_irr_segmentation %>%
  rowwise() %>% 
  ## Replace the Segmenten_Sophia and Segmenten_Jael by random strings:
  mutate(Segmenten_Sophia = ifelse(is.na(Segmenten_Sophia),
                                   NA,
                                   paste(replicate(n = Word_Count_Sophia,
                                                   stri_rand_strings(1, 
                                                                     length = sample(3:6, size = 1),
                                                                     pattern = "[A-Za-z0-9]")), 
                                         collapse = " "))) %>% 
  mutate(Segmenten_Jael = ifelse(is.na(Segmenten_Jael),
                                 NA,
                                 paste(replicate(n = Word_Count_Jael, 
                                            stri_rand_strings(1, 
                                                              length = sample(3:6, size = 1), 
                                                              pattern = "[A-Za-z0-9]")), 
                                  collapse = " "))) %>% 
  select(-c(Word_Count_Sophia, Word_Count_Jael)) %>% 
  # To make it more realistic, make segments the same where they were coded as such
  mutate(Segmenten_Jael = ifelse(Perspective_Sophia == 'y' & Perspective_Jael == 'y',
                                 Segmenten_Sophia,
                                 Segmenten_Jael)) %>% 
  relocate(Segmenten_Sophia, .after = Instructie_Code) %>% 
  relocate(Segmenten_Jael, .after = Perspective_Jael)

## Group by Instructie_Code and shuffle the Proefpersoon ID
artificial_irr_segmentation <- artificial_irr_segmentation %>% 
  group_by(Instructie_Code) %>%
  mutate(Proefpersoon = sample(Proefpersoon)) %>%
  arrange(Proefpersoon, Instructie_Code)

## Randomize the IRR data for the codes
## Remove everything that starts with opmerking
artificial_irr_coded_segments <- irr_coded_segments %>%
  select(!starts_with('Opmerk'))
## Create random strings for 'Segmenten'
artificial_irr_coded_segments <- artificial_irr_coded_segments %>%
  mutate(Word_Count = str_count(Segmenten, '\\w+')) %>%
  rowwise() %>%
  mutate(Segmenten = paste(replicate(n = Word_Count, 
                                     stri_rand_strings(1, 
                                                       length = sample(3:6, size = 1), 
                                                       pattern = "[A-Za-z0-9]")), 
                           collapse = " "))
# Filter matching code instances and modify values
artificial_irr_coded_segments_matching <- artificial_irr_coded_segments %>%
  filter(Codes_Sophia == Codes_Marloes) %>%
  group_by(Instructie_Code) %>%
  mutate(Codes_Sophia = sample(Codes_Sophia)) %>%
  mutate(Codes_Marloes = Codes_Sophia) %>%
  mutate(Proefpersoon = sample(Proefpersoon)) %>%
  ungroup() %>%
  mutate(No. = sample(No.))

# Filter non matching codes and shuffle
artificial_irr_coded_segments_non_matching <- artificial_irr_coded_segments %>%
  filter(Codes_Sophia != Codes_Marloes) %>%
  group_by(Instructie_Code) %>%
  mutate(Codes_Sophia = sample(Codes_Sophia)) %>%
  mutate(Codes_Marloes = sample(Codes_Marloes)) %>%
  mutate(Proefpersoon = sample(Proefpersoon)) %>%
  ungroup() %>%
  mutate(No. = sample(No.))

artificial_irr_coded_segments <- full_join(artificial_irr_coded_segments_matching, artificial_irr_coded_segments_non_matching) %>%
  arrange(Proefpersoon, Instructie_Code) %>%
  ungroup() %>%
  mutate(No. = row_number())

##    Simulate verbal report data                                              ----
## Replace the real answers by random word strings
artificial_coded_segments <- reports_coded_segments %>%
  mutate(Word_Count = str_count(New_Segment, '\\w+')) %>%  # Used to be 'Segment' with the real data
  rowwise() %>%
  mutate(New_Segment = paste(replicate(n = Word_Count, 
                                       stri_rand_strings(1, length = sample(3:6, size = 1), pattern = "[A-Za-z0-9]")), 
                             collapse = " ")) %>% 
  select(-c(Word_Count)) %>% # Also 'Segment' with the real data
  relocate(New_Segment, .after = Instructie_Code)
## Shuffle the assigned codes
# First shuffle the indices
shuffled_code_indices <- sample(length(artificial_coded_segments$Code))
# Re-assign values based on shuffled indices
artificial_coded_segments$Code <- artificial_coded_segments$Code[shuffled_code_indices]

artificial_coded_segments <- artificial_coded_segments %>% 
  group_by(Instructie_Code) %>%
  mutate(Proefpersoon = sample(Proefpersoon)) %>%
  ungroup() %>%
  mutate(No. = sample(No.)) %>%
  arrange(Proefpersoon, Instructie_Code) %>%
  ungroup() %>%
  mutate(No. = row_number()) 
  
  
##    Write all data files                                                     ----
#write_delim(artificial_age_gender_study_2, file = paste0(rel_path_processed_eye_tracking_data_output, "artificial_age_gender_study_2.txt"))
#write_delim(artificial_validation_result_overview, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_validation_results_overview.txt"))
#write_delim(artificial_calibration_data, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_calibration_data.txt"))
#write_delim(artificial_gaze_data_with_AOIs, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_gaze_data_with_AOIs.txt"))
#write_delim(artificial_fixation_point_data, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_fixation_point_data.txt"))
#write_delim(artificial_diagram_data, file = paste0(rel_path_processed_eye_tracking_data_output ,"artificial_diagram_data.txt"))
#write_delim(artificial_irr_segmentation, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_irr_segmentation.txt"))
#write_delim(artificial_irr_coded_segments, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_irr_coded_segments.txt"))
#write_delim(artificial_coded_segments, file = paste0(rel_path_processed_eye_tracking_data_output,"artificial_reports_coded_segments.txt"))
################################################################################ 