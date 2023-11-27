############################################################################  --
##                  Data Analysis Gaze Data Study 2:
##          Effects on Monitoring Accuracy and Text Comprehension
##                           Sophia Braumann                                  
## -------------------------------------------------------------------------- --
##
##           !! COLLAPSE ALL LINES TO BETTER NAVIGATE THIS SCRIPT !!
##            (Edit->Folding->Collapse All / alt+cmd+o/ Ctrl+Alt+o)
##
## -------------------------------------------------------------------------- --
## 1. Set-Up                                                                  ----
##    (Install if necessary and) load relevant libraries                      ----
#  Check and perform installation of relevant packages if needed
list_packages <- c("tidyverse",  # distinct, filter, %>%, and many more
                   "data.table", # for fread (reading in files with different dim)
                   "irr",        # for calculating the inter-rater reliabilities 
                   "tibble",     # to add a column at a specific position
                   "lubridate",  # to convert the time and date variable
                   "REdaS",      # to transform radian in degrees of visual angle 
                   "stringr",    # for string processing (word count, detect, replace, etc.)
                   "lme4",       # for ML models
                   "lmerTest",   # for p values with lmer
                   "sjstats",    # for partial eta squared & cohens effect size 
                   "sjPlot",     # for printing lmer HTML tables 
                   "emmeans",    # for pairwise comparisons and effect sizes etc.
                   "pbkrtest",   # for p-correction of multiple comparisons with emmeans
                   "kableExtra"  # for kable/HTML tables
                   )
# List of relevant packages
new_packages <- list_packages[!(list_packages %in% installed.packages()[,"Package"])] # Check whether all packages are installed
if(length(new_packages)) install.packages(new_packages, type = "binary", repos = list(CRAN="http://cran.rstudio.com/")) # Install them if that is not the case
## Load the relevant libraries
sapply(list_packages, require, character.only = TRUE)

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
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_author_analysis_script_eye_tracking_data.txt')

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

##    Functions required throughout the script                                ----
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

# A function that calculates descriptives 
calculate_descriptives <- function(incoming_data, outcome_of_interest){
  descOutcome <- incoming_data %>%
    dplyr::summarize(Mean = round(mean(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     SD = round(sd(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     Min = min(x = {{outcome_of_interest}}, na.rm = TRUE),
                     Max = max(x = {{outcome_of_interest}}, na.rm = TRUE))
  descOutcome <- descOutcome %>% distinct(Mean, SD, Min, Max, .keep_all = TRUE)
}

# A function that only calculates mean and SD
calculate_mean_sd <- function(incoming_data, outcome_of_interest){
  descOutcome <- incoming_data %>%
    dplyr::summarize(Mean = round(mean(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                     SD = round(sd(x = {{outcome_of_interest}}, na.rm = TRUE),2))
  descOutcome <- descOutcome %>% distinct(Mean, SD, .keep_all = TRUE)
}

# A function that retrieves model rows with p values <= 0.05 to print in bold 
return_rows_4_bold_print <- function(incoming_data_frame) {
  returningRowVec <- which(incoming_data_frame$p.value <= 0.05)
  returningRowVec
}

# A function that computes and merges standardized effects of pairwise comparisons
# and generates a ready_to_print kable object
return_print_ready_emmeans <- function(incomming_emmeans, fitted_Model) {
  ## Retrieve the effect sizes of the pairwise comparisons with emmeans package
  suppressMessages(# Suppress the unhelpful message of eff_size function
    # Call eff_size functions with emmeans and model objects
    eff_size_frame <- eff_size(incomming_emmeans, sigma = sigma(fitted_Model), 
                               edf = df.residual(fitted_Model)))
  eff_size_frame <- data.frame(eff_size_frame) # Save as data frame
  # Only keep the contrast specification and the effect sizes
  eff_sizes <- eff_size_frame %>% select(contrast, effect.size)
  
  ## Merge the standardized effects to the incomming_emmeans
  # Save the pairwise comparisons as data frame 
  contrast_frame <- summary(incomming_emmeans, as.df = TRUE) 
  # summary output comes as list, transform to data frame
  contrast_frame <- enframe(contrast_frame) %>% unnest(cols = c(value)) %>% 
    rename("contrast" = `1`) %>% # Rename the first column and
    select(c(contrast, estimate, SE, df, t.ratio, p.value)) %>% # select relevant columns
    filter(!is.na(contrast)) %>%
    # Join the the created data frame with the standardized effects 
    full_join(., eff_sizes, by = "contrast") %>%
    # Remove the brackets from the contrasts
    mutate(contrast = str_remove_all(contrast, "[()]"))
  # Create the kable output
  contrast_frame <- contrast_frame %>% kbl(.,digits = c(0,2,2,2,2,3,2)) %>% 
    kable_styling(full_width = FALSE) %>%  
    row_spec(return_rows_4_bold_print(contrast_frame), bold=T)
  contrast_frame
}

##    Set the paths to the required data and output folder                    ----
# To the directory of the required data to run the script:
rel_path_processed_data <- "Data/Processed_Data_For_Analyses/Artificial_Eye_Tracking_Data/"

# To the output folder
rel_path_output_folder <- "Output/Data_Analysis/Study_2_Eye_Tracking_CRVR/"

##    Load all files from the specified directories                           ----
## First store files of each directory into a separate list
# Processed data directory
processed_data_list <- load_all_txt_files(rel_path_processed_data)
# Remove the 'artificial' from the file names
names(processed_data_list) <- gsub('artificial_', '', names(processed_data_list))

## Load all files from the lists into the global environment/ current workspace
list2env(processed_data_list, .GlobalEnv)

## Should be all these files:
#  "age_gender_study_2"          
#  "calibration_data"           
#  "content_diagram_standard"    
#  "diagram_data"     
#  "fixation_point_data"
#  "gaze_data_with_AOIs"  
#  "irr_coded_segments"
#  "irr_segmentation"
#  "my_AOIs"                    
#  "reports_coded_segments"    
#  "transition_table_format"
#  "validation_results_overview"

## -------------------------------------------------------------------------- ----
## 2. Sample descriptives                                                     ----
# Print the sample age and gender distribution
# Age
desc_age_sample_study_2 <- calculate_descriptives(age_gender_study_2, Age)
# Gender
n_gender_sample_study_2 <- age_gender_study_2 %>% count(Gender) %>% 
  mutate('%' = round((n/sum(n))*100,2)) 

##    Calibration data + cut-off                                              ----       
## Note on the AOI definitions in "my_AOIs"
#  The height of the diagram boxes in this experiment adapted to the number of 
#  words entered by the participant. Each row in a diagram box had a height of 
#  20px and (depending on the length of the words) one row was about 3-4 words "wide".
#  The box width stayed constant, so more than (roughly) 4, 8, or 16 words meant 
#  a new text row in the diagram box. AOI definitions (i.e., coordinates of boxes) 
#  were made with (the same) 15 Lorem Ipsum words for all diagrams
#  The actual height of the diagram boxes "Stimulus.Height" was assumed to be 15 words 
#  which corresponded to 4 rows, so 4rows * 20px = Height 80px.
#  (An extra check showed that 16 words also corresponded to 4 rows 
#  with a different set of generated Lorem Ipsum words).

# Check the minimal difference between AOI and stimulus dimension
x_pixel_difference <- my_AOIs$Box_Width - my_AOIs$Stimulus_Width 
y_pixel_difference <- my_AOIs$Box_Height - my_AOIs$Stimulus_Height_15_Words 

## Check what 59 pixels (the minimal difference) would be 
#  converted into degrees of the visual angle.
# Calculate this with the parameters of the experiment set-up, 
# based on: https://osdoc.cogsci.nl/3.3/visualangle/
h <- 29.6        # Monitor height in cm
d <- 70          # Distance between monitor and participant in cm
r <- 1050        # Vertical resolution of the monitor
# The minimum distance between diagram box to AOI border: 
size_in_px <- min(c(x_pixel_difference, y_pixel_difference)) 
# Calculate the number of degrees that correspond to a single pixel 
# (run the provided formula for this)
deg_per_px <- rad2deg(atan2(.5*h, d)) / (.5*r)

# Calculate the size of the stimulus in degrees of the visual angle
size_in_deg <- size_in_px * deg_per_px # 1.341621
calibration_cut_off <- size_in_deg # This is now the calibration cut-off

## The other way around: Enter the maximal calibration value,
#  and check whether the resulting number of pixels sit within the minimum distance 
#  between box and AOI borders.
size_in_deg_check <- round(size_in_deg, 1) # When calibration value 1.3
#  Again, calculate the number of degrees that correspond to a single pixel
deg_per_px_check <- rad2deg(atan2(.5*h, d)) / (.5*r)

#  Calculate the size of the stimulus in degrees
size_in_px_check <- size_in_deg_check / deg_per_px_check 
# The resulting "error-range" in pixels would be 57.16966, this would still be in the 59px (minimal difference) 

## Check which participant IDs have calibration values above 1.3 calibration accuracy...
calibrations_to_be_kicked <- calibration_data %>% 
  filter(Right_Eye_Deviation_X > size_in_deg | Right_Eye_Deviation_Y > size_in_deg)
#  ...and kick them out of the (loaded) eye-tracking data
gaze_data_with_AOIs <- gaze_data_with_AOIs %>% 
  filter(!(Participant %in% calibrations_to_be_kicked$Participant))

##    Tracking ratio                                                          ---- 
# Save participants that had trials with tracking ratios <= 75%
tracking_ratios_2_kick <- gaze_data_with_AOIs %>% 
  filter(!(Tracking_Ratio > 75)) 
# Remove participants with a tracking ratio < 75% from the data
gaze_data_with_AOIs <- gaze_data_with_AOIs %>% filter(Tracking_Ratio > 75)

##    Computation of accuracy and drift                                       ----
## Based on https://www.frontiersin.org/articles/10.3389/fpsyg.2018.00803/full
# A trial consists of the presentation of a single target. 
# We reasoned that the longest fixation (in ms) on a given trial is most 
# likely the fixation that was intended to land on the target. 
# Note that we considered using the fixation that is closest to the 
# centroid of the target, but decided that this would create a selection bias 
# for the most accurate fixations. Indeed, others have used the longest 
# fixation to determine spatial accuracy using similar procedures 
# (e.g., Morgante et al., 2012). doi: 10.1111/j.1532-7078.2011.00089.x
# For a given participant, the script first identified the longest valid fixation for each trial. 
# The location of the longest fixation on each trial was then calculated in pixels. 
# Fixation location was computed by averaging valid data for the entire duration of the fixation, 
# even if that fixation continued after stimulus offset.

# Once the longest valid fixation was identified for each target location, 
# the script computed two measures of data quality per trial: 
# (1) Accuracy: computed as the Euclidean distance between the gaze location 
# and the center position of the target, in degrees of visual angle, 
# (2) Precision, calculated in two ways: the standard deviation (SD), 
# and the root mean square (RMS), both in degrees of visual angle, 
# and calculated in the horizontal and vertical directions 
# (formulas from Holmqvist et al., 2011). 
# Holmqvist, K., Nyström, M., Andersson, R., Dewhurst, R., Jarodzka, H., and Van de Weijer, J. (2011). Eye Tracking: A Comprehensive Guide to Methods and Measures. Oxford: Oxford University Press.
# SD is the calculated as the deviation from the mean location of all samples. 
# RMS is calculated using the distance between successive valid gaze locations within a single fixation. 
# In both cases lower values indicate better precision. 
# Degrees of visual angle were calculated using the participant’s 
# average distance from the screen for the duration of the calibration verification procedure

##    Drift check                                                            
## (Exclude the data of the (two) participants who's data was excluded 
#  based on calibration value)
fixation_point_data <- fixation_point_data %>% 
  filter(!(Participant %in% calibrations_to_be_kicked$Participant))
## Calculate the differences between mean PointOfRegardRight.px of first and last fixation cross
#  Compute the mean difference for the X coordinates 
computed_fixation_drifts <- fixation_point_data %>% 
  select(Participant, Point_of_Regard_Right_X_px, Point_of_Regard_Right_Y_px, Stimulus) %>% 
  group_by(Participant) %>% 
  dplyr::mutate(Mean_Fixation_Drifts_X = 
                  mean(as.numeric(Point_of_Regard_Right_X_px[.$Stimulus == "Fixation.rtf"]), na.rm = TRUE) - 
                  mean(as.numeric(Point_of_Regard_Right_X_px[.$Stimulus == "FixationEnd.rtf"]), na.rm = TRUE))
  # For each participant,
  #  compute the difference between mean Point_of_Regard_Right_X_px 
  #  (where Stimulus is fixation point 1, i.e., "Fixation.rtf")
  #  and mean Point_of_Regard_Right_X_px 
  #  (where Stimulus is fixation point 2 (on same white background; i.e., "FixationEnd.rtf")
  #  (For some reason, Point of Regard is a character now, 
  #  so transform to numeric again for computation).

#  Do the same (mean difference computation) for the Y coordinates
computed_fixation_drifts <- computed_fixation_drifts %>% group_by(Participant) %>% 
  dplyr::mutate(Mean_Fixation_Drifts_Y = 
                  mean(as.numeric(Point_of_Regard_Right_Y_px[.$Stimulus == "Fixation.rtf"]), na.rm = TRUE) - 
                  mean(as.numeric(Point_of_Regard_Right_Y_px[.$Stimulus == "FixationEnd.rtf"]), na.rm = TRUE))

#  Reduce to the relevant values (meanFixationDrifts per participant)
computed_fixation_drifts_red <- computed_fixation_drifts %>% 
  distinct(Participant, Mean_Fixation_Drifts_X,Mean_Fixation_Drifts_Y)

## Print drift for all participants and count how many were > than established 
#  maximal pixel difference 
problematic_difts <- computed_fixation_drifts_red  %>% group_by(Participant) %>%
  filter(Mean_Fixation_Drifts_X > size_in_px | Mean_Fixation_Drifts_Y > size_in_px)
## Calculate the ABSOLUTE mean difference across participants
#  For the X coordinates
desc_X_difts <- computed_fixation_drifts_red  %>% ungroup() %>%
  dplyr::summarize(
    Mean = mean(as.numeric(abs(Mean_Fixation_Drifts_X)), na.rm = TRUE)
    , Median = median(as.numeric(abs(Mean_Fixation_Drifts_X)), na.rm = TRUE)
    , SD = sd(as.numeric(abs(Mean_Fixation_Drifts_X)), na.rm = TRUE)
    , Min = min(as.numeric(abs(Mean_Fixation_Drifts_X)), na.rm = TRUE)
    , Max = max(as.numeric(abs(Mean_Fixation_Drifts_X)), na.rm = TRUE))
desc_X_difts <- as.data.frame(desc_X_difts) # Print the descriptives
# Mean      Median       SD     Min      Max
# 19.74987 11.79814 22.3792 1.02828 97.65511
# And for the Y coordinates
desc_Y_difts <- computed_fixation_drifts_red  %>% ungroup() %>%
  dplyr::summarize(
    Mean = mean(as.numeric(abs(Mean_Fixation_Drifts_Y)), na.rm = TRUE)
    , Median = median(as.numeric(abs(Mean_Fixation_Drifts_Y)), na.rm = TRUE)
    , SD = sd(as.numeric(abs(Mean_Fixation_Drifts_Y)), na.rm = TRUE)
    , Min = min(as.numeric(abs(Mean_Fixation_Drifts_Y)), na.rm = TRUE)
    , Max = max(as.numeric(abs(Mean_Fixation_Drifts_Y)), na.rm = TRUE)
  )
desc_Y_difts <- as.data.frame(desc_Y_difts) # Print the descriptives

## Check the mean difference in degrees of the visual angle for this experiment
#  First retrieve the "size" ("d" in the formula of the link) based on the x and y coordinates:
#  https://matheguru.com/lineare-algebra/abstand-zwischen-zwei-punkten.html
#  Plug in the mean differences into the formula: 
#  x_1 - x_2 = 20.18759 and y_1 - y_2 = 17.87285, so:
size_in_px_drift <- sqrt(desc_X_difts$Mean^2 + desc_Y_difts$Mean^2) 
# = 26.60897 --> use this for size in pixel variable in next formaula
#  Now plug in the experiment settings into the formula 
#  to transform pixels into degrees of the visual angle an vice versa.
#  See for example: https://osdoc.cogsci.nl/3.3/visualangle/
h <- 29.6               # Monitor height in cm
d <- 70                 # Distance between monitor and participant in cm
r <- 1050               # Vertical resolution of the monitor
size_in_px_drift        # Calculated "size"-value from above
#  Calculate the number of degrees that correspond to a single pixel
deg_per_px_drift <- rad2deg(atan2(.5*h, d)) / (.5*r)
#  Calculate the size of the stimulus in degrees of the visual angle
size_in_deg_drift <- size_in_px_drift * deg_per_px_drift 
size_in_deg_drift # 0.6050703
#  -> "There was a mean drift between fixation point 1 and 2 of 0.61 degrees of the visual angle."

##    Accuracy/Deviation reported by eye-tracker for included participants    ----
## Exclude the participants that were kicked out due to calibration or tracking ratio
validation_results_overview <- validation_results_overview %>% 
  filter(!(Participant %in% calibrations_to_be_kicked$Participant |
             Participant %in% tracking_ratios_2_kick$Participant))
mean_accuracy_eye_tracker <- validation_results_overview %>% ungroup() %>%
  mutate(Mean_Deviation_X_Y = (Right_Eye_Deviation_X+as.numeric(Right_Eye_Deviation_Y))/2) %>% 
  distinct(Participant, .keep_all = TRUE) %>%
  summarise(Mean = round(mean(Mean_Deviation_X_Y),2),
            SD = round(sd(Mean_Deviation_X_Y),2))

## 3. Calculate total fixation durations per AOI                              ---- 
#  (Pre-processing)
## Save the Start/First (min) and End/Last (max) of Recording Times 
#  (before any data is kicked out).
# Add a new variable to store the min recording time per trial:
gaze_data_with_AOIs <- gaze_data_with_AOIs %>% 
  group_by(Participant_ID) %>% # For all trials in participants (coded as Participant_ID vs. Participant),
  dplyr::mutate(Min_Recording_Time_Trial = min(Recording_Time_ms)) %>% # Add min recording time
  relocate(Min_Recording_Time_Trial, .after = Recording_Time_ms) # Relocate column
# Equivalently: save the last recording time:
gaze_data_with_AOIs <- gaze_data_with_AOIs %>% 
  group_by(Participant_ID) %>% 
  dplyr::mutate(Max_Recording_Time_Trial = max(Recording_Time_ms)) %>% 
  relocate(Max_Recording_Time_Trial, .after = Min_Recording_Time_Trial) 

## Keep fixations (i.e., remove saccades as coded in column "Category_Right")
fixation_data <- gaze_data_with_AOIs %>% 
  dplyr::filter(Category_Right == "Fixation")

## Use the index of each fixation ("Index_Right") to calculate the sum of 
#  recording time per fixation per participant per trial (Participant_ID).
fixations_with_durations <- fixation_data %>%
  #  --> Participant_ID is the same as group_by(Trial, Participant),
  group_by(Participant_ID, Index_Right) %>% # so for every fixation index in trial in participant,
  mutate(Fixation_Duration = max(Recording_Time_ms) - min(Recording_Time_ms)) %>% 
  # create a new variable by subtracting the first recording time from the last recording time,
  relocate(Fixation_Duration, .after = Diagram_Type) # and relocate the new column for easier checking
#  (Also get the fixation indicies further to the front for better checking of outcomes here)
fixations_with_durations <- fixations_with_durations %>% 
  relocate(Index_Right, .after = Fixation_Duration)

## Sum-Up Single Fixation duration per AOI_Name (including "NoAOIHit")
#  As this is computed with distinct() and hence data is "kicked out",
#  create a temp data set that is subsequently merged to the rest of the data again
fixation_duration_per_AOI.T <- fixations_with_durations %>%
  group_by(Participant_ID, AOI_Name) %>% # For every AOI_Name in Trial in participant, and
  distinct(Index_Right, Fixation_Duration) %>% # for every unique fixation-index 
  # (and Fixation_Duration; that's only important for merging), keep.all might only hamper merging..
  dplyr::mutate(Sum_AOI_Fixation_Duration = sum(Fixation_Duration, na.rm = TRUE)) # sum up the fixation duration
#  Merge this computed temp. file to the actual data
fixation_duration_per_AOI <- full_join(fixations_with_durations, fixation_duration_per_AOI.T) %>%
  relocate(Sum_AOI_Fixation_Duration, .after = Fixation_Duration) # Relocate column to the front
## Compute the fixation duration for the entire screen duration
#  Similar to summing up the fixation duration per AOI_Name, create a temporary data set, where
fixation_duration_per_AOI.Temp <- fixation_duration_per_AOI %>% 
  group_by(Participant_ID) %>% # for all trials in participants (Participant_ID vs. Participant),
  distinct(Index_Right, Fixation_Duration) %>% # and for every unique fixation-index 
  # (and Fixation_Duration; that's only important for merging)
  dplyr::mutate(Sum_Total_Fixation_Duration_Per_Screen = sum(Fixation_Duration, na.rm = TRUE)) # sum up the fixation durations
#  Merge this computed temp file to the actual data,
fixation_duration_per_AOI <- full_join(fixation_duration_per_AOI.Temp, fixation_duration_per_AOI) %>%
  relocate(Sum_Total_Fixation_Duration_Per_Screen, .after = Sum_AOI_Fixation_Duration) # and relocate the new variable

## Save an overview of all fixated AOIs too, then check which ones were not fixated at all
overview_fixated_AOIs <- fixation_duration_per_AOI %>% distinct(Participant, AOI_Name)
table(overview_fixated_AOIs$AOI_Name)
## Relocate the participant column to the beginning of the dataset
fixation_duration_per_AOI <- fixation_duration_per_AOI  %>% 
  relocate(Participant, .before = Participant_ID)

##    Merge Gorilla diagram data to the eye-tracking data                     ----
## Filter out experiment 1 data and 
#  the data of the participants that were kicked out due to bad calibration values
diagram_data_Exp_2 <- diagram_data  %>% 
  filter(Experiment == 2 & diagram_data$Participant_ID %in% fixation_duration_per_AOI$Participant_ID) # 

## Transform Cue Values to one categorical column
#  Add a new column named Cue_Type that receives its levels 
#  "Correct", "Commission", "Omission" based on TRUE or FALSE in respective Box data
# (The levels that are not Correct, Commission or Omission were the "Correct_Not_In_Standard" level)
diagram_data_Exp_2 <- add_column(diagram_data_Exp_2, Cue_Type = "Other", .after  = "Response") 
# ("Other" will remain after having assigned the cue values)
#  "Fill it up" with the corresponding cue values of the different columns 
diagram_data_Exp_2$Cue_Type[diagram_data_Exp_2$Correct_Points == TRUE] <- "Correct"
diagram_data_Exp_2$Cue_Type[diagram_data_Exp_2$Commission == TRUE] <- "Commission"
diagram_data_Exp_2$Cue_Type[diagram_data_Exp_2$Omission == TRUE] <- "Omission"

#  Check the levels of Cue_Type, i.e., how many boxes do not match one of the three cue types?
table(diagram_data_Exp_2$Cue_Type, useNA = "ifany")
## Create a column (Merge_Col) that can be used for merging the diagram_data_Exp_2 and the only_AOI_hit_data
#  by adapting Zone_Names (DiagBox1-4) to make them compatible with the AOI_Names (previous BoxTypes) in eyeTracking Data
# Remove "Diag" so that only Box1, Box2, etc. are left as Zone_Name levels
diagram_data_Exp_2$Zone_Name <- stringr::str_replace_all(diagram_data_Exp_2$Zone_Name, "Diag", "")  
# Rename Zone_Name to MergCol
names(diagram_data_Exp_2)[names(diagram_data_Exp_2) == "Zone_Name"] <- "Merge_Col"                  

# Remove irrelevant columns
diagram_data_Exp_2 <- diagram_data_Exp_2 %>% select(-c(Experiment, Condition))

## Add the same Merge_Col to the eye-tracking data 
#  by modifying the AOI_Name (in a new variable Merge_Col)
fixation_duration_per_AOI <- add_column(fixation_duration_per_AOI, 
                                     Merge_Col = sub('.*(?=.{4}$)', '', fixation_duration_per_AOI$AOI_Name, perl=T), 
                                     .before  = "Participant")
#  Rename "iven" (of remaining "Given")
fixation_duration_per_AOI$Merge_Col[fixation_duration_per_AOI$Merge_Col == "iven"] <- "Given"

## Merge the eye-tracking data with the diagram data
fixation_durations_AOI_diagram <- full_join(fixation_duration_per_AOI, diagram_data_Exp_2)

## Re-arrange the columns more logically
fixation_durations_AOI_diagram <- fixation_durations_AOI_diagram %>% 
  relocate(Trial, AOI_Name, Diagram_Type, Cue_Type, .after = Participant_ID)

## "Fill-up" the data for Cue_Type and Element_Number for the Given Boxes and outside-AOI hits
#  Add the Cue_Type-level "Given_Box"
fixation_durations_AOI_diagram$Cue_Type[grepl("Given", fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Given_Box"
## Add the content of the given boxes (of word count analysis, etc.)
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Muziek" & 
                                         grepl("Given", fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Muziek leren, lezen en spelen"
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Beton" & 
                                         grepl("Given",  fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Een lift renovatie is nodig"
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Botox" & 
                                         grepl("Given",  fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Botox blokkeert aanspaningsignaal tussen zenuwen en huid"
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Suez" & 
                                         grepl("Given",   fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "De handelsroute tussen Jeddah en Rotterdam werd korter"
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Metro" & 
                                         grepl("Given",  fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Rode metro wagons tot zinken gebracht"
fixation_durations_AOI_diagram$Response[fixation_durations_AOI_diagram$Trial == "Geld" & 
                                         grepl("Given",   fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Lottowinnaars zijn vaak niet lang gelukkig"
## Add the levels of Element_Number respectively
fixation_durations_AOI_diagram$Element_Number[grepl("Given", fixation_durations_AOI_diagram$AOI_Name, fixed = TRUE)] <- "Given"

## Also add the Cue Value and Diagram_Type "No AOI Hit again"
fixation_durations_AOI_diagram$Cue_Type[fixation_durations_AOI_diagram$AOI_Name == "Not_In_AOI"] <- "Not_In_AOI"

## Replace the responses where Diagram_Type is the standard diagram with the model responses
fixation_durations_AOI_diagram <- fixation_durations_AOI_diagram %>% 
  rowwise() %>% 
  mutate(Response = ifelse(!grepl("Own|Given", AOI_Name),
                           content_diagram_standard$Content[which(content_diagram_standard$AOI_Name == AOI_Name)],
                           Response))

## Transform relevant columns into factors
fixation_durations_AOI_diagram <- fixation_durations_AOI_diagram %>% 
  group_by(Participant) %>%
  mutate(across(c(Trial, AOI_Name, Diagram_Type, Cue_Type), as.factor)) %>% 
  filter(!is.na(Diagram_Type)) # And remove the missing values

## Add the word count of diagram responses
fixation_durations_AOI_diagram <- fixation_durations_AOI_diagram %>% 
  rowwise() %>%  # In every row,
  mutate(Word_Count = str_count(Response, '\\w+')) %>% # count number of words in Response.
  relocate(Word_Count, .after = Response)              # Relocate new Word_Count in the data

## Add a little table for the word count sentence
word_count_table <- fixation_durations_AOI_diagram %>% 
  distinct(Participant_ID, Diagram_Type, AOI_Name, Word_Count) %>% 
  filter(!Diagram_Type == "Not_In_AOI") %>%
  group_by(Diagram_Type) %>%
  summarise(Mean = round(mean(Word_Count, na.rm = TRUE),2),
            SD = round(sd(Word_Count, na.rm = TRUE),2),
            Min = min(Word_Count, na.rm = TRUE),
            Max = max(Word_Count, na.rm = TRUE))

##    Process data to different formats                                       ----
## Rename/Create different datasets of interest:
#  1. all_fixations_data: multiple rows per fixation (fixation_durations_AOI_diagram renamed)
#  2. rowPerfixation_data <- with summed duration per fixation
#  3. row_per_AOI_data <- summed duration and numbers per AOI_Name etc.

#  1. all_fixations_data: with multiple rows per fixation etc.
all_fixations_data <- fixation_durations_AOI_diagram %>% filter(!Cue_Type == "Other")
#  2. Dataset with one row per fixation
#  Reduce to each unique fixation index within one participant trial
row_per_fixation_data <- fixation_durations_AOI_diagram %>% 
  filter(!Cue_Type == "Other") %>%
  distinct(Participant_ID, Index_Right, .keep_all = TRUE)
row_per_fixation_data <-  row_per_fixation_data %>% 
  select(c(Participant, Participant_ID, Trial, Diagram_Type, Cue_Type, AOI_Name,
           Index_Right, Fixation_Duration, Sum_AOI_Fixation_Duration, 
           Sum_Total_Fixation_Duration_Per_Screen, Screen_Number, 
           Min_Recording_Time_Trial, Max_Recording_Time_Trial, 
           Export_End_Trial_Time_ms, Color, Tracking_Ratio, Response:Omission))


#  3. row_per_AOI_data <- summed duration per AOI_Name etc.
#  Reduce to one AOI_Name (per Trial) per participant
row_per_AOI_data <- fixation_durations_AOI_diagram %>% 
  filter(!Cue_Type == "Other") %>%
  distinct(Participant_ID, AOI_Name, .keep_all = TRUE)
row_per_AOI_data <- row_per_AOI_data %>% 
  select(c(Participant, Participant_ID, Trial, Diagram_Type, Cue_Type, AOI_Name,
           Sum_AOI_Fixation_Duration, Sum_Total_Fixation_Duration_Per_Screen, 
           Screen_Number, Min_Recording_Time_Trial, Max_Recording_Time_Trial, 
           Export_End_Trial_Time_ms, Color, Tracking_Ratio, Response:Omission))

##    Calculate the total Fixation duration for cue type & diagram type       ----
## Add a variable indicating the number of occurrence of one cue (per screen)
row_per_AOI_data.temp <- row_per_AOI_data %>% 
  # Group this subset based on Participant and Trial (Participant_ID), Diagram_Type and Cue_Type
  group_by(Participant_ID, Diagram_Type, Cue_Type) %>% 
  distinct(AOI_Name,Cue_Type) %>% # select only the distinct combinations of AOI_Name and Cue_Type.
  mutate(Cue_Occurrence_Trial = length(Cue_Type)) # length(Cue_Type) = number of distinct combinations of AOI_Name and Cue_Type per Participant and Trial(/Diagram).
#  Merge this computed temp. file to the actual data
row_per_AOI_data <- left_join(row_per_AOI_data, row_per_AOI_data.temp) %>%
  relocate(Cue_Occurrence_Trial, .after = Cue_Type) # and relocate the new variable again

## Fixation duration per Diagram Type                                                                
# Calculate Total Duration of Fixations in Diagrams (AOIs) per Screen (including non-AOI Hits)
row_per_AOI_data <- row_per_AOI_data %>% 
  group_by(Participant_ID, .drop = FALSE) %>%
  dplyr::mutate(Total_Duration_Fixations_Per_Screen = sum(Sum_AOI_Fixation_Duration, na.rm = TRUE)) %>%
  relocate(Total_Duration_Fixations_Per_Screen, .after = Sum_AOI_Fixation_Duration)

## Fixation duration per Cue Type                                                                    
# Sum up fixation duration per Cue Type for each participant
# (Not reported but was relevant before)
row_per_cue_type_data <- row_per_AOI_data %>% 
  # For each Cue_Type in Diagram_Type and per Trial in Participant,
  group_by(Participant_ID, Diagram_Type, Cue_Type, .drop = FALSE) %>% 
  # sum up the fixation duration for each Cue Type and 
  dplyr::mutate(Sum_Fixation_Duration_Cue_Types = sum(Sum_AOI_Fixation_Duration, na.rm = TRUE)) %>% 
  relocate(Sum_Fixation_Duration_Cue_Types, .after = Sum_AOI_Fixation_Duration)  # relocate the new variable.
row_per_cue_type_data <- row_per_cue_type_data %>% 
  relocate(Cue_Type, .after = Diagram_Type) %>% # Also relocate Cue_Type again
  distinct(Participant_ID, Diagram_Type, Cue_Type, .keep_all = TRUE) 
  
## Calculate percentages of fixation durations                                 
#  of Cue- and Diagram Types per total screen duration. 
row_per_cue_type_data <- row_per_cue_type_data %>% 
  group_by(Participant_ID, Diagram_Type, Cue_Type, .drop = FALSE) %>%
  mutate(Fixation_Duration_Cue_Type_Perc =  
           (Sum_Fixation_Duration_Cue_Types/Total_Duration_Fixations_Per_Screen)*100) %>% 
  relocate(Fixation_Duration_Cue_Type_Perc, .after = Sum_Fixation_Duration_Cue_Types)

## Remove columns that are not meaningful anymore to avoid confusion
row_per_cue_type_data <- row_per_cue_type_data %>% select(-c(AOI_Name, Sum_AOI_Fixation_Duration))

## Percentage Check
row_per_cue_type_data %>% 
  group_by(Participant_ID) %>% 
  distinct(Participant_ID, Diagram_Type, Cue_Type, .keep_all = TRUE) %>% 
  mutate(percentageCheck = sum(Fixation_Duration_Cue_Type_Perc, na.rm = TRUE)) %>% 
  distinct(Participant, Participant_ID, percentageCheck)

## Transform sum AOI fixations from milicseconds to seconds
row_per_AOI_data <- row_per_AOI_data %>% 
  mutate(Sum_AOI_Fixation_Duration_Sec = Sum_AOI_Fixation_Duration/1000) %>%
  relocate(Sum_AOI_Fixation_Duration_Sec, .after = Sum_AOI_Fixation_Duration) 

##    Calculate descriptive statistics                                        ----
## Reported in Results - Processing of the diagram-standard (RQ. 2.1) - Table 4 (Column 1)
## Check how many occurrences there are in total per participant
# First check how many cue types were fixated 
n_per_diagram_and_cue_type_gaze_data <- row_per_AOI_data %>% 
  group_by(Diagram_Type, Cue_Type, .drop = TRUE) %>% 
  count(., .drop = FALSE) %>% 
  rename("n Gaze Data" = n)
# Then retrieve the occurrence of cue types from the performance data
n_cue_types_performance <- diagram_data_Exp_2 %>% 
  group_by(Cue_Type) %>% 
  filter(!Cue_Type == "Other") %>% count()
# Add the amount of given boxes
n_given_temp <- as.numeric(diagram_data_Exp_2 %>% 
                             distinct(Participant_ID, Trial) %>% 
                             count())
# Save as data frame, 
n_given_temp <- data.frame(Cue_Type = "Given_Box", n = n_given_temp)
# add to the other cues, 
n_cue_types_performance <- rbind(n_cue_types_performance, n_given_temp) 
# and assign a meaningful label
n_cue_types_performance <- n_cue_types_performance %>% rename("n Performance Data" = n)
# Merge the those numbers
n_per_diagram_and_cue_type <- full_join(n_per_diagram_and_cue_type_gaze_data, 
                                        n_cue_types_performance, 
                                        by = "Cue_Type") %>% 
  filter(!Diagram_Type == "Not_In_AOI")

## Reported in Results - Processing of the diagram-standard (RQ. 2.1) - Table 4 (Column 2)
fixation_duration_single_boxes <- row_per_AOI_data %>% group_by(Diagram_Type, Cue_Type, .drop = TRUE) %>%
  summarise(Mean = round(mean(Sum_AOI_Fixation_Duration_Sec, na.rm	= TRUE),2),
            SD = paste0("(",round(sd(Sum_AOI_Fixation_Duration_Sec, na.rm	= TRUE),2),")"))

## Reported in Results - Processing of the diagram-standard (RQ. 2.1) - Table 4 (Column 3)
## Add dummy 0's for all non-fixated AOIs per participant so all reported numbers add up to 100%
## a. Create a Dummy-Frame with all Diagram+Cue Types:
Diagram_Type <- c(rep("StandardDiagram",4), rep("OwnDiagram",4), "Not_In_AOI")
Cue_Type <- c("Commission","Correct","Given_Box","Omission","Commission","Correct","Given_Box","Omission","Not_In_AOI")
Diagram_Cue_Type_Frame_2_Merge <- as.data.frame(cbind(Diagram_Type,Cue_Type))
# Now add those dummy/empty cue types per Trial
# There should be 9 Cue Type rows per Participant:
# (Own Diagram:) Commission, Correct, Omission, Given;
# (Standard Diagram:) Commission, Correct, Omission, Given;
# None AOI Fixations
## There should be AT LEAST (6*2 =) 12 instances of EACH Cue Type per participant and 6 instances of No-AOI Hit for all participants who completed six trials
# 6 Trials with each Cue Type appearing twice (for own and standard diagram) and always one non-AOI hit per trial = 18 rows

## b. Use the row_per_AOI_data for the fixation durations on box level and join the frame with the data
row_per_AOI_data_with_dummy_0s <- row_per_AOI_data %>% group_by(Participant,Participant_ID) %>% group_modify(~ full_join(.x, Diagram_Cue_Type_Frame_2_Merge))
# Inspect the joint data
table(row_per_AOI_data_with_dummy_0s$Participant, row_per_AOI_data_with_dummy_0s$Cue_Type)

## Add a 0 to the dummy-instances (i.e., were AOI_Name = NA)
row_per_AOI_data_with_dummy_0s$Sum_AOI_Fixation_Duration[is.na(row_per_AOI_data_with_dummy_0s$AOI_Name)] <- 0

## Remove all other variables to avoid confusion
row_per_AOI_data_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% select(Participant, Participant_ID,Trial, Diagram_Type, Cue_Type, AOI_Name, Sum_AOI_Fixation_Duration)

## Calculate Total Duration of Fixations in Diagrams (AOIs) per Screen (including non-AOI Hits)
#  (as preparation for last table column of Table 4)
row_per_AOI_data_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% group_by(Participant_ID) %>%
  dplyr::mutate(Total_Duration_Fixations_Per_Screen = sum(Sum_AOI_Fixation_Duration)) %>%
  relocate(Total_Duration_Fixations_Per_Screen, .after = Sum_AOI_Fixation_Duration)

## Sum up fixation duration per Cue Type for each participant
# (To then calculate percentages)
row_per_AOI_data_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% group_by(Participant_ID, Diagram_Type, Cue_Type) %>% # For each Cue_Type in Diagram_Type and per Trial in Participant,
  dplyr::mutate(Sum_Fixation_Duration_Cue_Types = sum(Sum_AOI_Fixation_Duration, na.rm = TRUE)) %>% # sum up the fixation duration for each Cue Type
  relocate(Sum_Fixation_Duration_Cue_Types, .after = Sum_AOI_Fixation_Duration) %>% # relocate the new variable.
  relocate(Cue_Type, .after = Diagram_Type)

## Calculate % of Cue_Types fixation duration based on total screen duration
row_per_AOI_data_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% 
  group_by(Participant_ID, Diagram_Type, Cue_Type) %>%
  mutate(Fixation_DurationCue_Type_perc =  (Sum_Fixation_Duration_Cue_Types/Total_Duration_Fixations_Per_Screen)*100) %>% 
  relocate(Fixation_DurationCue_Type_perc, .after = Sum_Fixation_Duration_Cue_Types)

## Calculate fixation durations per Diagram Type
row_per_diagram_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% 
  group_by(Participant_ID, Diagram_Type) %>%
  mutate(Fixation_DurationDiagram_Type =  sum(Sum_AOI_Fixation_Duration, na.rm = TRUE)) %>% 
  relocate(Fixation_DurationDiagram_Type, .after = AOI_Name)

## Calculate % of Diagram_Types fixation duration based on total screen duration
row_per_diagram_Type_with_dummy_0s <- row_per_diagram_with_dummy_0s %>% 
  group_by(Participant_ID, Diagram_Type) %>%
  mutate(Fixation_DurationDiagram_Type_perc = (Fixation_DurationDiagram_Type/Total_Duration_Fixations_Per_Screen)*100) %>% 
  relocate(Fixation_DurationDiagram_Type_perc, .after = Fixation_DurationDiagram_Type) %>%
  distinct(Participant_ID, Diagram_Type, Fixation_DurationDiagram_Type, Fixation_DurationDiagram_Type_perc, Total_Duration_Fixations_Per_Screen)

## Save as row per Cue Type data
row_per_cue_type_data_with_dummy_0s <- row_per_AOI_data_with_dummy_0s %>% 
  distinct(Participant_ID, Diagram_Type, Cue_Type, .keep_all = TRUE) 

# Inspect the joint data
table(row_per_cue_type_data_with_dummy_0s$Participant, row_per_cue_type_data_with_dummy_0s$Cue_Type)

## Retrieve the descriptives
desc_row_per_cue_type_data_with_dummy_0s <- row_per_cue_type_data_with_dummy_0s %>% group_by(Diagram_Type, Cue_Type) %>%
  dplyr::summarize(
    Mean = round(mean(Fixation_DurationCue_Type_perc, na.rm = TRUE),2)
    , SD = paste("(",round(sd(Fixation_DurationCue_Type_perc, na.rm = TRUE),2),")", sep = ""))

desc_row_per_diagram_type_with_dummy_0s <- row_per_diagram_Type_with_dummy_0s %>% group_by(Diagram_Type) %>%
  dplyr::summarize(
    Mean = round(mean(Fixation_DurationDiagram_Type_perc, na.rm = TRUE),2)
    , SD = paste("(",round(sd(Fixation_DurationDiagram_Type_perc, na.rm = TRUE),2),")", sep = ""))

## 4. Calculate fixation transitions                                          ----
##    (Using the one fixation per row merged with diagram data)
#  (Reported in Results - Processing of the diagram-standard RQ.2.1 - Table 6)
## Do some preparations with the data
#  Give the dataset a new name, which will also automatically create a backUp
data_4_transition_seeking <- row_per_fixation_data 
#  For some reason, Index_Right is coded as character now. 
#  Transform back to numeric for the subsequent computations
#  (Each fixation that was counted as seperate fixation -> see definition/criteria 
#   has an own index --> Index_Right).
data_4_transition_seeking$Index_Right <- as.numeric(data_4_transition_seeking$Index_Right)

## Explanation of the following part:
#  A transition counts as a transition if:
#  1. (fixationIndex_n - 1 ) + 1 = fixationIndex_n, 
#     which means the fixation indices are a direct sequence (e.g., 2,3 vs. 5,7)
#  2. AOI_Name_n - 1 != AOI_Name_n, 
#     which means that the directly following fixations are not in the same AOI_Name
## n - 1 is accomplished with lag(), so step 2 here would be lag(AOI_Name) != AOI_Name
## General overview
#  1. Between Diagrams
#    a. Correct2Standard (and vice versa)
#      i)   Matching Position
#      ii)  Matching Content
#      iii) Both
#    b. Mistake2Standard (and vv.)
#      b1. Commission
#        i)   Matching Position
#        ii)  Matching Content
#        iii) Both
#      b2. Omission
#        i) Matching Position
#    c. Between Given Boxes
#  2. Within Diagrams
#    a. Own
#    b. Correct

## Filter out empty fixation indices (from the merged diagram data with non-regarded AOIs)
data_4_transition_seeking <- data_4_transition_seeking %>% dplyr::filter(!is.na(Index_Right))
## Also remove Non-AOI-Hits
data_4_transition_seeking <- data_4_transition_seeking %>% filter(!AOI_Name == "Not_In_AOI")
## Also remove the "Other" Cue_Type
data_4_transition_seeking <- data_4_transition_seeking %>% filter(!(Cue_Type == "Other"))
## Relocate Cue_Type to the front for a better overview of computations
data_4_transition_seeking <- data_4_transition_seeking %>% relocate(Cue_Type,.after = AOI_Name)

## Correct the Element_Number of the Standard diagram. 
#  The element number in the standard corresponds to the box number saved in the AOI_Name.
data_4_transition_seeking <- data_4_transition_seeking %>% group_by(AOI_Name) %>% rowwise() %>% 
  mutate(Element_Number = 
           ifelse(grepl("Feed", AOI_Name), # If Feed is in the AOI_Name,
                  # take the last digit in the AOI_Name (e.g., MuziekFeedBox1: 1)
                  sub('.*(?=.{1}$)', '', AOI_Name, perl=T), 
                  Element_Number)) %>% # Otherwise re-print the existing Element Number
  mutate(Element_Number = # If Given is part of the AOI_Name, print Given, 
           ifelse(grepl("Given", AOI_Name), "Given", 
                  Element_Number)) # otherwise re-print the Element Number

## Here starts the transition seeking.
## First, add a new variable that indicates whether a transition is relevant to us.
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # sort (arrange) the fixation indices chronologically
  #  Check if the previous instance of Index_Right lag(Index_Right) is the same as Index_Right when adding 1
  #  and if the previous AOI_Name (lag(AOI_Name)) is the same as AOI_Name of current row.
  #  If both conditions are true, return TRUE, otherwise FALSE
  mutate(Transition_Check = lag(Index_Right+1) == Index_Right & lag(AOI_Name) != AOI_Name) %>%
  relocate(Transition_Check, .after = Index_Right) # (Relocate the new variable to the front)

## (1.) Create a variable that checks whether there was a transition between diagrams.
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Between_Diagrams =  
           Transition_Check == TRUE & #  Check if transition check is true
           # and lag(AOI_Name) included feed and AOI_Name includes own...
           ((grepl("Own", lag(AOI_Name)) & grepl("Feed", AOI_Name)) | 
              (grepl("Feed", lag(AOI_Name)) & grepl("Own", AOI_Name)))) %>% # ...and vice versa
  relocate(Between_Diagrams, .after = Transition_Check) # # (Relocate the new variable)

## (2a.) Add a variable that checks whether there was a transition within the Own Diagram
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>% # sort the fixation indeces chronologically.
  mutate(Within_Own_Diagram = 
           # Was there a transition at all? TRUE iff 
           Transition_Check == TRUE &    # Transition_Check = TRUE AND
           grepl("Own", lag(AOI_Name)) & # "Own" is in previous AND
           grepl("Own", AOI_Name)) %>%   # "Own" is current AOI_Name.
  relocate(Within_Own_Diagram, .after = Between_Diagrams) # Relocate the variable again

## (2b.) Check whether there were transitions within the correct/standard diagram
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>% # sort the fixation indices chronologically
  # Was there a transition at all? Return TRUE iff 
  mutate(Within_Standard_Diagram = Transition_Check == TRUE & # Transition_Check = TRUE AND
           grepl("Feed", lag(AOI_Name)) & # "Feed" is in previous AOI_Name AND
           grepl("Feed", AOI_Name)) %>%   # "Feed" is in current AOI_Name
  relocate(Within_Standard_Diagram, .after = Within_Own_Diagram) # Relocate the variable again

## (2.) Create a variable that has the observations of both within own and within standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>% # sort the fixation indices chronologically.
  # Is Within_Own_Diagram OR Within_Standard_Diagram TRUE?
  mutate(Within_Diagram = Within_Own_Diagram == TRUE | Within_Standard_Diagram == TRUE) %>% 
  relocate(Within_Diagram, .before = Between_Diagrams) # Relocate the variable again

## 1a. Between own correct and standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>% # sort the fixation indices chronologically
  mutate(Own_Correct_2_Standard = Between_Diagrams == TRUE & # Was there a transition between diagrams? AND
           # Did neither the previous, nor the current AOI_Name include "Given"? AND
           (!grepl("Given", lag(AOI_Name)) | !grepl("Given", AOI_Name)) &
           # Did the previous AOI name include "Feed" AND was the current diagram box coded as correct? OR
           ((grepl("Feed", lag(AOI_Name)) & (Cue_Type == "Correct")) |
              # Does the current AOI name include "Feed" AND was the previous box coded as correct?
              (grepl("Feed", AOI_Name) & (lag(Cue_Type) == "Correct")))) %>% 
  # Return TRUE iff all of these questions resolve to TRUE.
  relocate(Own_Correct_2_Standard, .after = Between_Diagrams) #  Relocate the variable again

## 1c. Transitions between diagrams including Given Boxes in own diagram
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # sort the fixation indices chronologically.
  mutate(Own_Given_2_Standard = # Own_Given_2_Standard is TRUE iff
           Between_Diagrams == TRUE & # Between_Diagrams = TRUE AND
           # previous Cue_Type was a Given box AND current AOI_Name includes "Feed", OR
           ((lag(Cue_Type) == "Given_Box" & grepl("Feed", AOI_Name)) | 
              # current Cue_Type is a Given AND previous AOI_Name includes "Feed"
              (Cue_Type == "Given_Box" & grepl("Feed", lag(AOI_Name))))) %>% 
  relocate(Own_Given_2_Standard, .after = Own_Correct_2_Standard) # Relocate the new variable

## 1b.i. Between own commission and standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # sort the fixation indices chronologically.
  mutate(Commission_2_Standard = # Commission_2_Standard = TRUE iff 
           Between_Diagrams == TRUE & # Between_Diagrams = TRUE AND
           # previous Cue_Type was a Commission AND current AOI_Name includes "Feed", # OR
           ((lag(Cue_Type) == "Commission" & grepl("Feed", AOI_Name)) | 
              # current Cue_Type is a commission AND previous AOI_Name included "Feed"
              (Cue_Type == "Commission" & grepl("Feed", lag(AOI_Name))))) %>% 
  relocate(Commission_2_Standard, .after = Own_Given_2_Standard) # Relocate the new variable.

## 1b.ii. Between own omission and standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # sort the fixation indices chronologically.
  mutate(Omission_2_Standard = # Omission_2_Standard = TRUE iff 
           Between_Diagrams == TRUE & # Between_Diagrams = TRUE AND
           # previous Cue_Type was an Omission AND current AOI_Name includes "Feed", OR
           ((lag(Cue_Type) == "Omission" & grepl("Feed", AOI_Name)) |  
              # current Cue_Type is Omission AND previous AOI_Name includes "Feed" 
              (Cue_Type == "Omission" & grepl("Feed", lag(AOI_Name))))) %>%
  relocate(Omission_2_Standard, .after = Commission_2_Standard) # Relocate the new variable.

## 1a.i) Between own correct and standard diagram with matching positions
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Own_Correct_2_Standard_Matching_Pos = # TRUE iff 
           Own_Correct_2_Standard == TRUE & # Own_Correct_2_Standard == TRUE, AND 
           # AOI_Name of previous row ends with the same digit as AOI_Name of this row
           (sub('.*(?=.{1}$)', '', lag(AOI_Name), perl=T) == sub('.*(?=.{1}$)', '', AOI_Name, perl=T))) %>% 
  # (Relocate the new variable behind its upper category Own_Correct_2_Standard
  relocate(Own_Correct_2_Standard_Matching_Pos, .after = Own_Correct_2_Standard) 

## 1a.ii) Between own correct and standard diagram with matching content 
#  (as preparation for actual transition count 1.a.ii)
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Own_Correct_2_Standard_Matching_Cont = # Own_Correct_2_Standard_Matching_Content is TRUE iff
           Own_Correct_2_Standard == TRUE & # Own_Correct_2_Standard == TRUE AND
           # the content of the current diagram box or the box content of the previous fixation was not NA
           (!is.na(lag(Element_Number)) & !is.na(Element_Number)) & # AND
           # stri_detect_coll finds lag(Element_Number) in Element_Number OR
           (stringi::stri_detect_coll(lag(Element_Number), Element_Number) | 
              # vice versa (stri_detect_coll finds Element_Number in lag(Element_Number)).
              stringi::stri_detect_coll(Element_Number, lag(Element_Number)))) %>%
  # (Relocate the new variable to the front of the data somewhere).
  relocate(Own_Correct_2_Standard_Matching_Cont, .after = Own_Correct_2_Standard_Matching_Pos) 

## 1a.ii) Return in article reported fixation transition where ONLY POSITION but not content matched
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Own_Correct_2_Standard_Matching_Pos_Only = # Return TRUE if only position but not content matched 
           Own_Correct_2_Standard_Matching_Cont == FALSE & Own_Correct_2_Standard_Matching_Pos == TRUE) %>%
  # (Relocate the new variable to the front of the data somewhere)
  relocate(Own_Correct_2_Standard_Matching_Pos_Only, .after = Own_Correct_2_Standard_Matching_Pos) 

## 1a.ii) Return in article reported fixation transition where ONLY CONTENT but not position matched
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indeces chronologically
  mutate(Own_Correct_2_Standard_Matching_Cont_Only = # Return TRUE if only content but not position matched 
           Own_Correct_2_Standard_Matching_Cont == TRUE & Own_Correct_2_Standard_Matching_Pos == FALSE) %>%
  # (Relocate the new variable to the front of the data somewhere)
  relocate(Own_Correct_2_Standard_Matching_Cont_Only, .before = Own_Correct_2_Standard_Matching_Cont) 

## 1a.ii) Transitions between own correct and standard with matching POSITION AND CONTENT
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  mutate(Own_Correct_2_Standard_Both_Matching = # Return TRUE if both content and position of Own_Correct_2_Standard matched
           Own_Correct_2_Standard_Matching_Cont == TRUE & Own_Correct_2_Standard_Matching_Pos == TRUE) %>%
  # (Relocate the new variable to the front of the data somewhere)
  relocate(Own_Correct_2_Standard_Both_Matching, .after = Own_Correct_2_Standard_Matching_Cont_Only) 

## 1a.ii) For completeness (i.e., percentages add up to 100%), 
## report transitions between own correct and standard matching NEITHER content NOR position
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  mutate(Own_Correct_2_Standard_No_Matches = # Return TRUE iff 
           Own_Correct_2_Standard == TRUE & # it was a transition between own correct and standard AND
           # there was no position and no content match
           Own_Correct_2_Standard_Matching_Cont == FALSE & Own_Correct_2_Standard_Matching_Pos == FALSE) %>%
  relocate(Own_Correct_2_Standard_No_Matches, .after = Own_Correct_2_Standard_Both_Matching) # Relocate variable to the front

## 1b1.i) Between Commission and standard and matching position 
#  (again, as preparation for matching position only)
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>% # arrange the fixation indices chronologically
  mutate(Commission_2_Standard_Matching_Position = #  TRUE iff 
           Commission_2_Standard == TRUE & # Commission_2_Standard == TRUE 
           #  AND AOI_Name of previous row ends with the same digit as AOI_Name of this row
           (sub('.*(?=.{1}$)', '', lag(AOI_Name), perl=T) == sub('.*(?=.{1}$)', '', AOI_Name, perl=T))) %>%
  relocate(Commission_2_Standard_Matching_Position, .after = Commission_2_Standard) 

## 1b1.ii) Between Commission and standard to "matching content", i.e. wrong paraphrase coded as such
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  #  TRUE iff Commission_2_Standard == TRUE AND
  mutate(Commission_2_Standard_Incorrect_Parafrase = Commission_2_Standard == TRUE & 
           # the content of the current diagram box or the box content of the previous fixation was not NA
           (!is.na(lag(Element_Number)) & !is.na(Element_Number)) & # AND
           # stri_detect_coll finds lag(Element_Number) in Element_Number OR vice versa
           (stringi::stri_detect_coll(lag(Element_Number), Element_Number) | 
                     stringi::stri_detect_coll(Element_Number, lag(Element_Number)))) %>%
  relocate(Commission_2_Standard_Incorrect_Parafrase, .after = Commission_2_Standard)

## Now compute the actually reported transitions where only position or only content matched:
## 1b1.ii Between Commission and standard where ONLY POSITION and not content matched 
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Commission_2_Standard_Position_Matching_Only = # TRUE iff the position but not the content matched
           Commission_2_Standard_Matching_Position == TRUE & Commission_2_Standard_Incorrect_Parafrase == FALSE) %>%
  relocate(Commission_2_Standard_Position_Matching_Only, .after = Commission_2_Standard_Incorrect_Parafrase) 

## 1b1.ii) Between Commission and standard where ONLY CONTENT and not position matched (somehow), i.e. wrong paraphrase, coded as such
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Commission_2_Standard_Wrong_Parafrase_Only = # TRUE iff the content but not the position matched 
           Commission_2_Standard_Matching_Position == FALSE & Commission_2_Standard_Incorrect_Parafrase == TRUE) %>%
  relocate(Commission_2_Standard_Wrong_Parafrase_Only, .after = Commission_2_Standard_Position_Matching_Only) 

## 1b1.ii) Create a variable that is true iff both position AND content match
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Commission_2_Standard_Both_Matching = # TRUE iff the position AND the content matched
           Commission_2_Standard_Matching_Position == TRUE & Commission_2_Standard_Incorrect_Parafrase == TRUE) %>%
  relocate(Commission_2_Standard_Both_Matching, .after = Commission_2_Standard_Wrong_Parafrase_Only) 

## 1b1.ii) For completeness (i.e., percentages add up to 100%), 
## report transitions between own commission and standard matching NEITHER content NOR position
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  mutate(Commission_2_Standard_No_Matches = # TRUE iff 
           Commission_2_Standard == TRUE & # it was a transition between an own commission and the standard AND
           # there was no position and no content match 
           Commission_2_Standard_Matching_Position == FALSE & Commission_2_Standard_Incorrect_Parafrase == FALSE) %>%
  relocate(Commission_2_Standard_No_Matches, .after = Commission_2_Standard_Both_Matching) # Relocate variable to the front

## 1b2. Between Omission and correct diagram to matching position
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Omission_2_Standard_Matching_Position = # TRUE iff:
           Omission_2_Standard == TRUE & # Omission_2_Standard == TRUE
           # AOI_Name of previous row ends with the same digit as AOI_Name of this row
           (sub('.*(?=.{1}$)', '', lag(AOI_Name), perl=T) == sub('.*(?=.{1}$)', '', AOI_Name, perl=T))) %>%
  relocate(Omission_2_Standard_Matching_Position, .after = Omission_2_Standard) 

## For completeness, so that percentages get to 100: 
## Between Omission and mismatching position in standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # arrange the fixation indices chronologically
  mutate(Omission_2_Standard_Mismatching_Position = #  TRUE iff
           Omission_2_Standard == TRUE & # Omission_2_Standard == TRUE 
           #  AND AOI_Name of previous row ends with the same digit as AOI_Name of this row
           (sub('.*(?=.{1}$)', '', lag(AOI_Name), perl=T) != sub('.*(?=.{1}$)', '', AOI_Name, perl=T))) %>%
  relocate(Omission_2_Standard_Mismatching_Position, .after = Omission_2_Standard_Matching_Position) 

## And also for the table:
## 1b. Between own incorrect (omission or commission) and standard
data_4_transition_seeking <- data_4_transition_seeking %>% 
  group_by(Participant_ID) %>% # For every trial in participant,
  arrange(Index_Right, .by_group = TRUE) %>%  # sort the fixation indices chronologically.
  mutate(Own_Mistake_2_Standard = Between_Diagrams == TRUE & # Was there a transition between diagrams? AND
           # Was there either a transition between an own commission or omission and standard?
           (Commission_2_Standard == TRUE | Omission_2_Standard == TRUE)) %>%
           relocate(Own_Mistake_2_Standard, .after = Own_Correct_2_Standard_Matching_Cont)

## To compute percentages print all transition summaries in one object
only_transition_data <- data_4_transition_seeking %>% 
  filter(Transition_Check == TRUE) %>% 
  distinct(Participant_ID, Trial, Index_Right, .keep_all = TRUE)

##    Create the transition table                                             ----
#  Count all instances of TRUE (with sum()) for all relevant columns (between Transition_Check and Within_Standard_Diagram
only_transition_data <- only_transition_data %>% 
  ungroup() %>% 
  summarise(across(Transition_Check:Within_Standard_Diagram, ~sum(.x == TRUE, na.rm = TRUE)))
#  Transpose for better overview
total_transitions <- only_transition_data %>% 
  pivot_longer(everything(), names_to = "Transition_Type", values_to = "n") 

# Add the percentages of totals and add a column with the row numbers for later
transition_frame <- full_join(transition_table_format, total_transitions) %>% 
  select(!starts_with("Code_Level")) %>% 
  mutate("%_total" = round(n/n[Code == "0."]*100,2)) %>% 
  mutate(my_row_numbers = row_number())

# Write a function that performs operations on transition_frame
divide_by_upper_category <- function(Unique_Index){
  transitions <- transition_frame
  index <- Unique_Index
  upper_coding_level <- transitions$Upper_Code_Level[transitions$my_row_numbers == index]
  return_value <- transitions$n[transitions$my_row_numbers == index]/
    transitions$n[transitions$Code == upper_coding_level]
  return_value <- round(return_value*100,2)
  return_value
}

## Compute the percentages per transition category
transition_frame <- transition_frame %>% 
  rowwise() %>% # For each row, 
  mutate("%_within_category" = ifelse(my_row_numbers < 2, # check if
                                      NA, # if that's true, print NA
                                      # if that's not true, call function from above
                                      divide_by_upper_category(my_row_numbers))) 

# Check whether all subcategories add up to 100%         
transition_frame %>% 
  group_by(Upper_Code_Level) %>% 
  filter(!grepl("\\*",Code)) %>%
  summarise(sum(`%_within_category`, na.rm = TRUE))

# Transform the transition frame for printing
transition_frame_2_print <- transition_frame %>% 
  filter(!grepl("\\*",Code)) %>%
  select(Code, Description, n, `%_total`, `%_within_category`) %>%
  rename("Transition Type" = Description,
         "% of Total" = `%_total`,
         "% Within Category" = `%_within_category`)


## Check how many transitions there are per participant
#  (Reported in footnote of Results - Processing of the diagram-standard RQ.2.1 - Table 6)
transitions_per_PP <- data_4_transition_seeking %>% 
  filter(Transition_Check == TRUE) %>% 
  group_by(Participant) %>%
  mutate(No.Transitions = length(Transition_Check == TRUE)) %>% 
  distinct(Participant, No.Transitions) 
## Retrieve descriptives
desc_transitions <- transitions_per_PP %>% ungroup() %>% 
  dplyr::summarize(Mean = round(mean(No.Transitions, na.rm = TRUE),2),
                   SD = paste("(",round(sd(No.Transitions, na.rm = TRUE),2),")", sep = ""),
                   Min = min(No.Transitions, na.rm = TRUE),
                   Max = round(max(No.Transitions, na.rm = TRUE),2),
                   n = n())
desc_transitions <- as.data.frame(desc_transitions) # Print

## 5. Calculate IRR of retrospective reports                                  ----
##    IRR for the segments                                                    ----
## Extract the total number of identified segments per coder/rater
# Coder 1
no_segments_coder_1 <- irr_segmentation %>% count(Perspective_Sophia) %>% 
  filter(Perspective_Sophia == 'n' | Perspective_Sophia == 'y') %>% 
  dplyr::summarize(segments = sum(n))
# Coder 2
no_segments_coder_2 <- irr_segmentation %>% count(Perspective_Jael) %>% 
  filter(Perspective_Jael == 'n' | Perspective_Jael == 'y') %>% 
  dplyr::summarize(segments = sum(n))
## Prepare data to be able to distinguish lower and upper bound of agreement
segments_coder_1_irr <- irr_segmentation %>% count(Perspective_Sophia) %>% 
  filter(Perspective_Sophia == 'n' | Perspective_Sophia == 'y')
segments_coder_2_irr <- irr_segmentation %>% count(Perspective_Jael) %>% 
  filter(Perspective_Jael == 'n' | Perspective_Jael == 'y')
## See Strijbos et. al for rationale of calculation:
# Coder 1
segments_coder_1_irr <- segments_coder_1_irr %>% 
  mutate("total segments" = sum(n)) %>% rowwise() %>% 
  mutate("1 - n/total segments" = round((1 - n/no_segments_coder_1)*100,2)) %>% 
  rename("(Dis-)Agreement" = Perspective_Sophia) 
segments_coder_1_irr$`1 - n/total segments`[1,1] <- 
  paste0("**",segments_coder_1_irr$`1 - n/total segments`[1,1],"**")
# Coder 2
segments_coder_2_irr <- segments_coder_2_irr %>% 
  mutate("total segments" = sum(n)) %>% rowwise() %>% 
  mutate("1 - n/total segments" = round((1 - n/no_segments_coder_2)*100,2)) %>% 
  rename("(Dis-)Agreement" = Perspective_Jael) 
segments_coder_2_irr$`1 - n/total segments`[1,1] <- 
  paste0("**",segments_coder_2_irr$`1 - n/total segments`[1,1],"**")

## Merge outcomes per coder and add kbl formatting for printing html table
all_segment_IRRs <- full_join(segments_coder_1_irr, segments_coder_2_irr, by = "(Dis-)Agreement")
all_segment_IRRs$`(Dis-)Agreement`[all_segment_IRRs$`(Dis-)Agreement` == "n"] <- "Disagreement"
all_segment_IRRs$`(Dis-)Agreement`[all_segment_IRRs$`(Dis-)Agreement` == "y"] <- "Agreement"


##    IRR for the codes                                                       ----
## Calculate the squared kappa
## Calculate the general agreement in %
total_coded_segments <- irr_coded_segments %>% count()
coding_agreement <- data.frame(table(irr_coded_segments$Codes_Sophia == 
                                       irr_coded_segments$Codes_Marloes, useNA = "ifany")) %>%
  mutate("total codes" = as.numeric(total_coded_segments)) %>% 
  rename(Agreement = Var1, n = Freq) %>% 
  mutate("%" = round((n/`total codes`)*100,2))

## And the kappa's
kappas_coded_segments <- kappa2(cbind(irr_coded_segments$Codes_Sophia, 
                                     irr_coded_segments$Codes_Marloes), 
                               weight = "squared")
## Create an output sentence that becomes a footnote to the segment table
kappas_coded_segments <- paste0('Kappa of coded segments was: ', 
                               round(kappas_coded_segments$value,3),
                               ". The general agreement between coders was ", 
                               coding_agreement$`%`[coding_agreement$Agreement == TRUE],
                               "%")


## 6. Multilevel analysis: Models, Pairwise Comparisons                       ----
#  (Using row per AOI data and fixation durations in seconds)
## Remove the no diagram-Hit data
only_AOI_hit_data <- row_per_AOI_data %>% filter(Diagram_Type != "Not_In_AOI")
## Fit the first model with all predictors (Diagram_Type, Cue_Type and their interaction)
#  (all in one with Diagram_Type*Cue_Type)
fixation_duration_mod <- 
  lmer(Sum_AOI_Fixation_Duration_Sec ~ Diagram_Type*Cue_Type + (1 | Participant), 
                                         data = only_AOI_hit_data)
##  Print model summaries
summary(fixation_duration_mod)
confint(fixation_duration_mod) # with confidence intervals
# Print model as 
fixation_duration_output <- tab_model(fixation_duration_mod, show.se = TRUE, show.std = TRUE)

## For completeness, fit all models of the model-building process 
fixation_duration_mod_0 <- lmer(Sum_AOI_Fixation_Duration_Sec ~ 1 + (1 | Participant), data = only_AOI_hit_data)
fixation_duration_mod_1 <- lmer(Sum_AOI_Fixation_Duration_Sec ~ Diagram_Type + Cue_Type + (1 | Participant), data = only_AOI_hit_data)
fixation_duration_all_models <- tab_model(fixation_duration_mod_0, fixation_duration_mod_1, fixation_duration_mod)
# ...and check whether sig. more variance is explained when adding the main effects and interaction
Fixation_Duration_interaction_check <- anova(fixation_duration_mod_1, fixation_duration_mod)

## Pairwise comparisons with effect sizes
# Compute and save the pairwise comparisons
fixation_duration_mod_emm <- emmeans(fixation_duration_mod, list(pairwise ~ Diagram_Type*Cue_Type), adjust = "tukey")
# Retrieve the standardized effects of the pairwise comparisons,
# merge them to fixation_duration_mod_emm and create a ready-to-print kbl output
fixation_duration_mod_emm <- return_print_ready_emmeans(fixation_duration_mod_emm, fixation_duration_mod) %>% 
  scroll_box(width = "100%", height = "400px") %>% 
  footnote(general = "Pairwise Comparisions Average Total Fixation Durations of Single Boxes (sec)", 
           general_title = "")

## 8. Analysis Cued Retrospective Reports (Total number segments, %, etc.)    ----
## Save relevant total scores 
total_segments <- reports_coded_segments %>% ungroup %>% count() %>% as.numeric(.)
total_participants <- reports_coded_segments %>% distinct(Proefpersoon) %>% count() %>% as.numeric(.)

## Create different summaries to merge later
# Retrieve how many segments were produced by each participant
segments_per_participant <- reports_coded_segments %>% 
  group_by(Proefpersoon) %>% # For each participant,
  count(Code) %>% # count occurrence of code-category.
  # Remove duplicate rows (so that all categories occur once per participant)
  distinct(Proefpersoon, Code, .keep_all = TRUE) 

# Establish the order of the codes for comprehensive table output
code_order <- c("1","2","3","4","5","6","7a","7b","8a" ,"8b", "8c", "8d", 
                "9","10a", "10b", "11","12", "13a", "13b", "14","15")

# Retrieve how many segments there were per category
segments_per_category <- segments_per_participant %>%
  # by summing up the number of occurrences for each code per coding-category
  group_by(Code) %>% dplyr::summarize(n_category = sum(n)) %>% 
  # arrange codes based on the established order above
  arrange(factor(Code, levels = code_order)) %>% 
  # Create a % score by dividing the number of segments per category with the total amount of segments
  mutate("% of Total" = round((n_category/total_segments)*100,2))

# Retrieve the average amount of segmentes per participant (per category)
average_per_partipant <- segments_per_participant %>% 
  group_by(Code) %>%
  reframe(calculate_mean_sd(., n)) %>%
  arrange(factor(Code, levels = code_order)) %>% 
  distinct(Code, .keep_all = TRUE) 

# Also retrieve how many participants mentioned the coding-category at least once
mentioned_by_col <- segments_per_participant %>% group_by(Code, .drop = FALSE) %>% 
  count() %>% # Save as natural number..
  mutate("% " = round((n/total_participants)*100,2)) %>% # ..and percentage
  arrange(factor(Code, levels = code_order))

# Filter the data to only include the JOL-screen segments
joL_screen_segments <- reports_coded_segments %>% filter(grepl("2", Instructie_Code))

## Merge these summaries into one frame
segment_summary_both_screens <- full_join(segments_per_category, average_per_partipant) %>%
  full_join(mentioned_by_col)

## Retrieve the same info again
# Save relevant total scores 
total_jol_segments <- joL_screen_segments %>% ungroup %>% count() %>% as.numeric(.)

## Create different summaries to merge later
# Retrieve how many segments were produced by each participant
jol_segments_per_participant <- joL_screen_segments %>% 
  group_by(Code, Proefpersoon, .add = TRUE) %>% # For each participant,
  count(Code) %>% # count occurrence of code-category.
  # Remove duplicate rows (so that all categories occur once per participant)
  distinct(Proefpersoon, Code, .keep_all = TRUE) 

jol_segments_per_category <- jol_segments_per_participant %>%
  # by summing up the number of occurrences for each code per coding-category
  group_by(Code) %>% dplyr::summarize(n_category = sum(n)) %>% 
  # arrange codes based on the established order above
  arrange(factor(Code, levels = code_order)) %>% 
  # Create a % score by dividing the number of segments per category with the total amount of segments
  mutate("% of Total" = round((n_category/total_jol_segments)*100,2))

# Retrieve the average amount of segments per participant (per category)
jol_average_per_partipant <- jol_segments_per_participant %>% 
  group_by(Code, .drop = FALSE) %>%
  dplyr::summarise(Mean = round(mean(n, na.rm = TRUE),2),
                   SD = round(sd(n, na.rm = TRUE),2)) %>%
  arrange(factor(Code, levels = code_order)) %>% 
  distinct(Code, .keep_all = TRUE) 

# Also retrieve how many participants mentioned the coding-category at least once
jol_mentioned_by <- jol_segments_per_participant %>% 
  group_by(Code, .drop = FALSE) %>% 
  count() %>% # Save as natural number..
  mutate("% " = round((n/total_participants)*100,2)) %>% # ..and percentage
  arrange(factor(Code, levels = code_order))

## Create a summary frame of all these frames 
segment_summary_jol_screen <-  full_join(jol_segments_per_category, jol_average_per_partipant) %>%
  full_join(jol_mentioned_by)

## 9. Create formatted output for Rmarkdown file                              ----
##    Sample descriptives                                                     ----
## age 
desc_age_sample_study_2 <- kbl(desc_age_sample_study_2, 
                           caption = "Descriptives of Age") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_left")
## gender
n_gender_sample_study_2 <- kbl(n_gender_sample_study_2, 
                           caption = "Gender Distribution") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_left")

##    Exclusion Criteria and Cases                                            ----
exclusion_overview <- rbind(
  calibrations_to_kick <- c("Calibration Values", # Exclusion bases on calibration values
                          round(calibration_cut_off,2), 
                          round(dim(calibrations_to_be_kicked)[1],0)),
  Tracking_RatiosToKick <- c("Tracking Ratios", "< 75%", # Exclusion based on low tracking ratio
                            round(dim(tracking_ratios_2_kick)[1],0)),
  totalRemaining <- c(" ", # Print how many participants were left
                      " ", 
                      22-dim(calibrations_to_be_kicked)[1]+dim(tracking_ratios_2_kick)[1])) %>%
  kbl(col.names = c(" ", "Cut-Off", "n"), caption = "Exclusion Criteria and Cases") %>%
  kable_paper("striped", full_width = F) %>%
  pack_rows("Exclusion of", 1, 2) %>%
  pack_rows("Gaze Data of Participants Analysed", 3,3) %>%
  kable_styling(full_width = FALSE, position = "float_left") 
  
##    IRR Segments and Coding                                                 ----
## Segments
all_segment_IRRs <- all_segment_IRRs %>% kbl(col.names = gsub(".x+|.y+", "", names(.)),
                                         caption = "Agreement of raters for segmentation of verbal reports for coding",
                                         align = "lcccccc") %>%
  kable_paper("striped", full_width = F) %>%
  add_header_above(c(" " = 1, "Perspective Coder 1" = 3, "Perspective Coder 1" = 3)) %>% 
  column_spec(4, border_right=T) 

##    Descriptive Statistics Average Fixation Duration                        ----
# Recode levels of the diagram type for the overall diagram rows
desc_row_per_diagram_type_with_dummy_0s$Diagram_Type <- recode(desc_row_per_diagram_type_with_dummy_0s$Diagram_Type, 
                                                         Standard_Diagram = "Standard", 
                                                         Own_Diagram = "Own Diagram", 
                                                         Not_In_AOI = "Outside Diagrams/AOIs") 
fixation_duration_single_boxes$Diagram_Type <- recode(fixation_duration_single_boxes$Diagram_Type, 
                                                   Not_In_AOI = "Outside Diagrams/AOIs")
desc_row_per_cue_type_data_with_dummy_0s$Diagram_Type <- recode(desc_row_per_cue_type_data_with_dummy_0s$Diagram_Type, 
                                                         Not_In_AOI = "Outside Diagrams/AOIs")
# Merge all descriptive frames
fixation_descriptives <- full_join(n_per_diagram_and_cue_type, fixation_duration_single_boxes) %>%
  full_join(., desc_row_per_cue_type_data_with_dummy_0s, by = c("Diagram_Type", "Cue_Type")) %>%
  rename(Mean = Mean.y, SD = SD.y) %>%
  full_join(., desc_row_per_diagram_type_with_dummy_0s, by = c("Diagram_Type", "Mean", "SD")) %>% 
  arrange(factor(Diagram_Type, levels = c("Own Diagram", "Own_Diagram", "Standard", "Standard_Diagram","Outside Diagrams/AOIs")))
# Now add NAs where necessary for nicer printing
fixation_descriptives$Cue_Type[fixation_descriptives$Cue_Type == "Not_In_AOI"] <- " "
fixation_descriptives$Diagram_Type[fixation_descriptives$Diagram_Type == "Standard_Diagram" | 
                                  fixation_descriptives$Diagram_Type == "Own_Diagram"] <- " "
# Do the rest with a function
remove_NAs <- function(Incoming_Column) {
  Incoming_Column[is.na(Incoming_Column)] <- " " 
  Incoming_Column
}
fixation_descriptives$Cue_Type <- remove_NAs(fixation_descriptives$Cue_Type)
fixation_descriptives$`n Gaze Data` <- remove_NAs(fixation_descriptives$`n Gaze Data`)
fixation_descriptives$`n Performance Data` <- remove_NAs(fixation_descriptives$`n Performance Data`)
fixation_descriptives$Mean.x <- remove_NAs(fixation_descriptives$Mean.x)
fixation_descriptives$SD.x <- remove_NAs(fixation_descriptives$SD.x)

## Format the table
fixation_descriptives <- fixation_descriptives %>% kbl(col.names = gsub(".x", "", names(.)), 
    caption = "Descriptive Statistics of Total Fixation Durations per Diagram and Cue Type") %>%
  kable_paper("striped", full_width = TRUE) %>%
  add_header_above(c(" " = 2, 
                     "No. of diagram boxes in eye-tracking data/ No. of diagram boxes in performance data" = 2, 
                     "Average Total Fixation Duration of Single Box Per Cue Type (seconds)" = 2, 
                     "% Total Fixations on Screen relative to screen duration" = 2)) 

## Also add the word count as little table
word_count_table$Diagram_Type <- recode(word_count_table$Diagram_Type, 
                          Standard_Diagram = "Standard", 
                          Own_Diagram = "Own Diagram")
word_count_table_kbl <- kbl(word_count_table, caption = "Word Count of Diagram Responses") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE)

##    Fixation Transition Table                                               ----
transition_frame_2_print$Code <- gsub("\\*+", " ", transition_frame_2_print$Code)
transition_frame_2_print$Code[transition_frame_2_print$Code == "0."] <- " "
transition_frame_2_print <- transition_frame_2_print %>% 
  relocate(Code, .after = `Transition Type`) %>% 
  kbl(caption = "Average Number of Transitions per Trial and Transition Type",
      align = "llccc") %>%
  kable_paper("striped", full_width = TRUE) %>%
  pack_rows("Total Transitions", 1, 1) %>%
  pack_rows("Between Diagrams", 2, 17) %>%
  pack_rows("Within Diagrams", 18, 20) %>%
  add_indent(c(3:17, 19,20)) %>% 
  add_indent(c(4:7, 9:16)) %>% 
  add_indent(c(4:7,10:13, 15,16)) 

##    Cued Retrospective Verbal Reporting                                     ----
segment_summary <- full_join(segment_summary_both_screens, segment_summary_jol_screen, by = "Code")
total_n_segments <- segment_summary_both_screens %>% ungroup() %>% summarise(n = sum(n_category, na.rm = TRUE))
segment_summary <- segment_summary %>% kbl(col.names = gsub(".x|.y|_catego|of Total", "", names(.)), 
                                           caption = "Average Frequencies and Proportion of Code Categories 
                                           per Participant and Entire Sample") %>%
  kable_paper("striped", full_width = TRUE) %>%
  add_header_above(c(" " = 1, 
                     "Total" = 2, 
                     "Average frequency per participant" = 2, 
                     "Mentioned by" = 2,
                     "Total" = 2, 
                     "Average frequency per participant" = 2, 
                     "Mentioned by" = 2)) %>%
  add_header_above(c(" " = 1, 
                     "During Diagram Study & JOL Screen" = 6, 
                     "During JOL Screen Only" = 6)) %>% 
  column_spec(7, border_right=T) %>%
  scroll_box(width = "100%", height = "500px") 

## 10.Tidy up and save the workspace                                          ----
rm(computed_fixation_drifts, desc_X_difts, desc_Y_difts,
   fixation_data, fixation_duration_per_AOI.T, fixations_with_durations, fixation_duration_per_AOI.Temp,
   row_per_AOI_data.temp, n_given_temp, n_per_diagram_and_cue_type_gaze_data, n_cue_types_performance,
   segments_per_participant, segments_per_category, average_per_partipant, mentioned_by_col,
   jol_segments_per_participant, jol_segments_per_category, jol_average_per_partipant,jol_mentioned_by,
   total_transitions, only_transition_data,
   segment_summary_both_screens, segment_summary_jol_screen)

## Save current date to append to output
today <- Sys.Date()
today <- format(today, format="%d_%b_%Y")

#save.image(file = paste0(rel_path_output_folder, "data_analysis_study_2_eye_tracking_CRVR_", today, ".RData"))

############################################################################  ----