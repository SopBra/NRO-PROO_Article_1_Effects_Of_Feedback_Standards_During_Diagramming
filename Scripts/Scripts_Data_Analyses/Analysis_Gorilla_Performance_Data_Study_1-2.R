############################################################################  --
##                  Data Analysis Performance Data Study 1:
##          Effects on Monitoring Accuracy and Text Comprehension
##                           Sophia Braumann                                  
## -------------------------------------------------------------------------- --
##
##           !! COLLAPSE ALL LINES TO BETTER NAVIGATE THIS SCRIPT !!
##            (Edit->Folding->Collapse All / alt+cmd+o/ Ctrl+Alt+o)
##
## -------------------------------------------------------------------------- --
## 0. Set the paths to the required data and load files                       ----
## Save paths to the different directories where the data is stored
# To the directory of the artificial data:
rel_path_artificial_performance_data <- "Data/Processed_Data_For_Analyses/Artificial_Performance_Data/"

# Path to the output folder
rel_path_for_output <- "Output/Analysis_Scripts/Study_1_Gorilla_Performance_Data/Performance_Data/"
## -------------------------------------------------------------------------- ----
## 1. Set-Up                                                                  ----
##    (Install if necessary and) load relevant libraries                      ----
#  Check and perform installation of relevant packages if needed
list_of_packages <- c("tidyverse",  # distinct, filter, %>%, and many more,
                      "irr",        # for the inter-rater-reliabilities
                      "data.table", # for fread (reading in files with different dim)
                      "Hmisc",      # for gamma correlations
                      "lme4",       # for ML models
                      "lmerTest",   # for p values with lmer
                      "sjstats",    # partial eta squared and cohens effect size in repeated masures ML
                      "sjPlot",     # for printing lmer HTML tables 
                      "emmeans",    # for pairwise comparisons and effect sizes etc.
                      "kableExtra" # for more kable options
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
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_author_analysis_script_performance_data.txt')

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

##    Relevant functions used throughout the script                           ----
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


# A function that can compute and retrieve the gamma correlation coefficient 
#   from the long summary output of rcorr.cens
calculate_gammas <- function(incoming_column_1, incoming_column_2){
  # Generate gamma statistic only
  gamma <- rcorr.cens(incoming_column_1, incoming_column_2, outx = TRUE)[2]
  # Note that DescTools::GoodmanKruskalGamma(x = incoming_column_1, y = incoming_column_2) provides exactly the same output
  gamma <- round(gamma,4)
  gamma
}

# A function that can compute cue diagnosticities
#   (i.e., person correlations between Cue Value and Test Scores)
calculate_pearsons <- function(DiagramCue, TestOrJOL) {
  outgoingData <- cor(DiagramCue, TestOrJOL, use = "everything", method = "pearson")
  outgoingData
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
# Another one that rounds min and max in case of the correlations
calculate_descriptives_of_corrs <- function(incoming_data, outcome_of_interest){
  # To prevent Inf returns when there are no missings, 
  # only run the function if there are correlations to compute per participant
  # So check if all entries of ourcome_of_interest are NA
desc_outcome <- incoming_data %>% 
  dplyr::summarize(Mean = round(mean(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                   SD = round(sd(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                   Min = round(min(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                   Max = round(max(x = {{outcome_of_interest}}, na.rm = TRUE),2),
                   NAs = sum(is.na({{outcome_of_interest}}))) %>% 
  distinct(Mean, SD, Min, Max, .keep_all = TRUE)
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

## Retrieve p-values from a model summary 
retrieve_Value <- function(model_object, PredictorName) {
  model_summary <- summary(model_object)
  output <- as.data.frame(model_summary$coefficients)
  pValue <- output$`Pr(>|t|)`[rownames(output) == PredictorName]
  pValue
  
}

# A function that retrieves model rows with p values <= 0.05 to print in bold 
return_rows_4_bold_print <- function(incoming_data_frame) {
  returningRowVec <- which(incoming_data_frame$p.value <= 0.05)
  returningRowVec
}

# A function that retrieves the p-value from an anova model comparision 
# (and prints whether this was significant)
retrieve_anova_output <- function(anova_modelFrame) {
  # The anova comparison for ML models prints Pr(>Chisq) in the summary, 
  # the non-ML model summary Pr(>F). Adapt: If "Pr(>Chisq)" in incoming frame
  if ("Pr(>Chisq)" %in% names(anova_modelFrame)) {
    returnObj <- data.frame("Interpretation" = ifelse(anova_modelFrame$`Pr(>Chisq)`[2] <= 0.05, 
                                                      "explained", "did not explain"),
                            "Stat" = "_Chi<sup>2</sup>_",
                            "Estimate" = round(anova_modelFrame$Chisq[2],2),
                            "df" = paste0("(",anova_modelFrame$Df[2],")"),
                            "sign" = ifelse(anova_modelFrame$Chisq[2] <= 0.05, "<", "="),
                            "p" = format(round(anova_modelFrame$`Pr(>Chisq)`[2],3), nsmall = 3))
  } else {
    returnObj <- data.frame("Interpretation" = ifelse(anova_modelFrame$`F`[2] <= 0.05, 
                                                      "explained", "did not explain"),
                            "Stat" = "_F_",
                            "Estimate" = round(anova_modelFrame$`F`[2],2),
                            "df" = paste0("(",anova_modelFrame$Df[2],")"),
                            "sign" = ifelse(anova_modelFrame$`F`[2] <= 0.05, "<", "="),
                            "p" = format(round(anova_modelFrame$`F`[2],3), nsmall = 3))
  }
  
  final_return <- c(returnObj$Interpretation, 
                    paste0(returnObj$Stat,returnObj$df," = ",returnObj$Estimate," _p_ ", returnObj$sign," ",returnObj$p))
}

## __________________________________________________________________________ ##
##    Load all files of the specified directory                               ----
# First store files of the given directory into a list
processed_performance_data_list <- load_all_txt_files(rel_path_artificial_performance_data)

# Remove the 'artificial' from the file names
names(processed_performance_data_list) <- gsub('artificial_', '', names(processed_performance_data_list))

# Then load all files from the list into the global environment/ current workspace
list2env(processed_performance_data_list, .GlobalEnv)

## Should be these files:
# "jolAndTestDataExp1and2.txt"
# "diagram_dataExp1and2.txt"
# "processedprocessed_performance_data.txt"


## -------------------------------------------------------------------------- ----
## 1. Sample                                                                  ----
##    Descriptive statistics                                                  ----
# Age
desc_age_sample_exp_1_n_80 <- calculate_descriptives(processed_sample_data_exp_1, age)
# Gender
n_gender_sample_exp_1_n_80 <- processed_sample_data_exp_1 %>% count(Sex) %>% 
  mutate('%' = round((n/sum(n))*100,2)) 

## One participant needs to leave because of short time on task. 
# Calculate descriptives for final sample
processed_sample_data_exp_1_final <- processed_sample_data_exp_1 %>% 
  filter(!Time_On_Task < 2)
desc_age_sample_exp_1_n_79 <- calculate_descriptives(processed_sample_data_exp_1_final, age)
# Gender
n_gender_sample_exp_1_n_79 <- processed_sample_data_exp_1_final %>% 
  count(Sex) %>% 
  mutate('%' = round((n/sum(n))*100,2)) 

##    Distributions per condition + drop-out                                  ----
# Create a general frame to print with the RMarkdown summary
n_per_condition <- processed_performance_data %>% 
  group_by(Experiment, Condition) %>% 
  filter(!Time_On_Task < 2) %>%
  distinct(Participant, .keep_all = TRUE) %>% 
  summarise(n = n())
# Rename the conditions for printing
n_per_condition$Condition <- rename_conditions(n_per_condition$Condition)

# Create a temporary drop-out summary to merge to the n per condition frame
drop_out_2_merge <- processed_consort_data_study_1 %>% 
  group_by(Condition) %>% 
  summarise(Drop_Out = sum(Drop_Out)) %>%
  mutate(Experiment = 1) %>% 
  relocate(Experiment)
drop_out_2_merge$Condition <- rename_conditions(drop_out_2_merge$Condition)

# Merge the two frames
n_per_condition <- full_join(n_per_condition, drop_out_2_merge) %>%
  filter(Experiment == 1) %>% ungroup() %>% 
  select(-c(Experiment))
## Note: Drop-out for Experiment 2 added in the summary output

##    Time on task                                                            ----
desc_time_on_task <- processed_performance_data %>% 
  distinct(Participant_ID, .keep_all = TRUE) %>%
  group_by(Experiment, Condition) %>%
  filter(!Time_On_Task < 2) %>%
  reframe(calculate_descriptives(., Time_On_Task)) %>%
  distinct() %>% # Remove duplicate rows due to the call of summarise() twice
  mutate(Min = round(Min, 2)) %>%
  mutate(Max = round(Max, 2))
desc_time_on_task$Condition <- rename_conditions(desc_time_on_task$Condition)

##    Remove the participants that completed the task in under 4 minutes      ----
participants_to_kick <- processed_sample_data_exp_1 %>% 
  filter(Time_On_Task < 2) %>% 
  distinct(Participant_ID)
jol_and_test_data  <- jol_and_test_data %>% 
  filter(Participant != as.numeric(participants_to_kick$Participant_ID))

## Add to the drop-out frame 
# Save the condition name and the number of instances where the time on task <2min
added_drop_out <- processed_sample_data_exp_1 %>% 
  filter(Time_On_Task < 2) %>% 
  distinct(Condition) %>% 
  mutate(n = n()) %>% 
  rename_conditions(.)
# Add (visibly) to n_per_condition
n_per_condition$Drop_Out[n_per_condition$Condition == added_drop_out[1]] <- 
  paste0(n_per_condition$Drop_Out[n_per_condition$Condition == added_drop_out[1]],"+1*")

## 2. Calculate IRR                                                           ----
## Calculate Kappa's
# Based on: https://www.datanovia.com/en/lessons/inter-rater-reliability-analyses-quick-r-codes/
# Cohen’s Kappa: It can be used for either two nominal or two ordinal variables. It accounts for strict agreements between observers. 
#                It is most appropriate for two nominal variables.
# Weighted Kappa: It should be considered for two ordinal variables only. It allows partial agreement.

## Calculate the irr for the total test scores
irr_test_total_scores <- kappa2(cbind(irr_tests_both_coders$Total_Score_Coder_1, irr_tests_both_coders$Total_Score_Coder_2), weight = "squared") 

## Calculate the total correct scores, as those will be used for the analysis
irr_diagram_data_row_per_trial <- irr_diagrams_both_coders %>% group_by(Participant_ID, Trial) %>% 
  mutate(Total_Correct_Coder_1 = sum(Correct_Coder_1))
irr_diagram_data_row_per_trial <- irr_diagram_data_row_per_trial %>% 
  group_by(Participant_ID, Trial) %>% 
  mutate(Total_Correct_Coder_2 = sum(Correct_Coder_2)) %>%
  distinct(Participant_ID, Trial, Total_Correct_Coder_1, Total_Correct_Coder_2)

## IRR for the...
# ...total correct boxes per diagram
irr_diagram_total <- kappa2(cbind(irr_diagram_data_row_per_trial$Total_Correct_Coder_1, irr_diagram_data_row_per_trial$Total_Correct_Coder_2), weight = "squared") 
# "squared": disagreements are weighted according to their squared distance from perfect agreement
# The weighted Kappa coefficient with '"squared"' weights equals the product moment correlation under certain conditions. 
#   Own weights could be specified by supplying the function with a numeric vector of weights, 
#   starting from perfect agreement to worst disagreement. 
#   The length of this vector must equal the number of rating categories.

# ...all single correct boxes
irr_diagram_correct <- kappa2(cbind(irr_diagrams_both_coders$Correct_Coder_1, irr_diagrams_both_coders$Correct_Coder_2), weight = "squared") 
# ...all element numbers
irr_diagram_element_no <- kappa2(cbind(irr_diagrams_both_coders$Element_Number_Coder_1, irr_diagrams_both_coders$Element_Number_Coder_2), weight = "squared") 
# ...all commission errors
irr_diagram_commission <- kappa2(cbind(irr_diagrams_both_coders$Commission_Coder_1, irr_diagrams_both_coders$Commission_Coder_2), weight = "squared") 

## 3. Prepare data/variables for analysis                                     ----                                           
##    Compute Monitoring Accuracy                                             ----
# First the monitoring accuracy bias: JoL2 - Test Score of each trial (row)
jol_and_test_data <- jol_and_test_data %>% 
  group_by(Participant) %>%
  mutate(Mon_Acc_Bias = JoL2 - Test)
# Then Absolute Monitoring Accuracy as the unsigned difference JoL2-Test
jol_and_test_data <- jol_and_test_data %>% 
  group_by(Participant) %>%
  mutate(Abs_Mon_Acc = abs(JoL2 - Test))
##    Create pooled factors for Diagramming Yes/No and Standard Yes/No        ----
## First rename the levels of Condition
jol_and_test_data <- jol_and_test_data %>% 
  ungroup() %>% 
  mutate(Condition = rename_conditions(Condition))
# First for factor "Diagramming" (i.e., participants completed diagrams)
jol_and_test_data <- jol_and_test_data %>% 
  ungroup() %>% 
  mutate(Diagramming = ifelse(# If Condition: Diagram+Standard OR Diagramming-Only,
    Condition == "Diagramming+Standard"| Condition == "Diagramming-Only", 
    "Yes", # Assign Yes as level to Diagramming,
    "No")) %>% # otherwise assign No as level of diagramming
  relocate(Diagramming, .after = Condition)

# Then for factor "Standard" (i.e., participants received a diagram-standard)
jol_and_test_data <- jol_and_test_data %>% 
  ungroup() %>% 
  mutate(Standard = ifelse(
    Condition == "Diagramming+Standard"| Condition == "Standard-Only", 
    "Yes", 
    "No")) %>%
  relocate(Standard, .after = Condition)

##    Compute Relative Monitoring Accuracy                                    ----
# Relative Monitoring Accuracy is computed by calculating 
#     gamma correlations between JOL2 and Test across the six trials

# Apply function to compute gamma correlations to the trials of each participant
jol_and_test_data <- jol_and_test_data %>% 
  group_by(Participant) %>% # For every participant, 
  mutate(Rel_Mon_Acc = calculate_gammas(JoL2, Test)) %>% # call the gamma calculating function from above,
  relocate(Rel_Mon_Acc,.after = Abs_Mon_Acc)

##    Save a row per participant data set                                     ----
row_per_participant_data <- jol_and_test_data %>% 
  distinct(Participant, .keep_all = TRUE) %>% 
  select(Experiment, Participant, Condition, Diagramming, Standard, Rel_Mon_Acc)
##    Compute Cue Value/Diagnosticity/Utilitzation                            ----
# Add and ID for each participant of Experiment 2
# (the current IDs reflect single trials and not participants)
## Merge the ids for experiment 2 to the diagram data
diagram_data <- right_join(all_ids_to_merge, diagram_data) %>%
  relocate(Experiment) %>% 
  arrange(Experiment) 
 
##    Compute Cue Values 
# Correct Boxes
diagram_data <- diagram_data %>% 
  group_by(Participant, Trial) %>%
  mutate(Cue_Value_Correct = sum(Correct_Points))
# Commissions
diagram_data <- diagram_data %>% 
  group_by(Participant, Trial) %>%
  mutate(Cue_Value_Commission = sum(Commission))
# Omissions
diagram_data <- diagram_data %>% 
  group_by(Participant, Trial) %>%
  mutate(Cue_Value_Omission = sum(Omission))

## Rename the levels of condition before merging
diagram_data <- diagram_data %>% 
  ungroup() %>%
  mutate(Condition = rename_conditions(Condition))

## Save as row per trial data
diagram_data_row_per_trial <- diagram_data %>% 
  distinct(Participant, Trial, .keep_all = TRUE) %>% 
  select(-c(Zone_Name, Response, Word_Count, 
            Correct_Points, Element_Number, Commission, Omission))

## There were coding mistakes: Maximum score is 4, not 5, correct this
diagram_data_row_per_trial$Cue_Value_Correct[diagram_data_row_per_trial$Cue_Value_Correct == 5] <- 4

##    Merge Diagram Data and Performance data                                 ----
diagram_data_row_per_trial <- left_join(diagram_data_row_per_trial, jol_and_test_data)

## Calculate the cue diagnosticity and cue utilization
# Cue Diagnosticity
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Diag_Correct = calculate_pearsons(Cue_Value_Correct, Test))
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Diag_Commission = calculate_pearsons(Cue_Value_Commission, Test))
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Diag_Omission = calculate_pearsons(Cue_Value_Omission, Test))

# Cue Utilization
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Util_Correct = calculate_pearsons(Cue_Value_Correct, JoL2))
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Util_Commission = calculate_pearsons(Cue_Value_Commission, JoL2))
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% 
  group_by(Participant) %>% 
  mutate(Cue_Util_Omission = calculate_pearsons(Cue_Value_Omission,JoL2))

## 4. Descriptive statistics all relevant outcomes (Experiment 1 and 2)       ----
##    Per pooled condition                                                    ----
## Create a function for this to not do the same stuff for all outcomes
calculate_descriptives_factors <- function(incoming_data, DV_name, outcome_of_interest) {
  desc_diagramming <- incoming_data %>% group_by(Experiment, Diagramming) %>% 
    calculate_descriptives(outcome_of_interest = {{outcome_of_interest}}) %>% 
    filter(Experiment == 1) 
  # Rename the pooled factor to be able to merge the descriptives later
  desc_diagramming <- desc_diagramming %>% 
    rename(Factor = Diagramming) 
  
  # For standard as factor
  desc_standard <- incoming_data %>% group_by(Experiment, Standard) %>% 
    calculate_descriptives(outcome_of_interest = {{outcome_of_interest}}) %>% 
    filter(Experiment == 1) 
  # Rename the pooled factor to be able to merge the descriptives later
  desc_standard <- desc_standard %>% 
    rename(Factor = Standard) 
  # Merge the two descriptive data frames
  descriptives_factor <- full_join(desc_diagramming, desc_standard)
  descriptives_factor
}

## Apply to all outcomes of interest 
# Abs. Monitoring Accuracy
descriptives_abs_MA_factor <- 
  calculate_descriptives_factors(jol_and_test_data, "Abs. Monitoring Acc.", Abs_Mon_Acc)
# Rel. Monitoring Accuracy (with row per participant data because of correlations)
row_per_participant <- jol_and_test_data %>% 
  select(c(Experiment:Diagramming, Rel_Mon_Acc)) %>% 
  distinct(Participant, .keep_all = TRUE)
descriptives_rel_MA_factor <- 
  calculate_descriptives_factors(row_per_participant, "Rel. Monitoring Acc.", Rel_Mon_Acc)
# Test Scores (with row per trial data again)
descriptives_testScore_factor <- 
  calculate_descriptives_factors(jol_and_test_data, "Test Scores", Test)
# JOL1
descriptives_JoL1_factor <- calculate_descriptives_factors(jol_and_test_data, "JoL1", JoL1)
# JOL2
descriptives_JoL2_factor <- calculate_descriptives_factors(jol_and_test_data, "JoL2", JoL2)

jol_and_test_data %>% group_by(Experiment, Condition) %>% 
  summarise(Mean = round(mean(Abs_Mon_Acc, na.rm= TRUE),2))

##    Per condition                                                           ----
## Calculate descriptives per condition using a function to avoid redundant lines 
calculate_descriptives_per_condition_and_outcome <- function(incoming_data, outcome_of_interest) {
  desc_condition <- incoming_data %>% 
    group_by(Experiment, Condition) %>% # For each experiment and condition
    calculate_descriptives(outcome_of_interest = {{outcome_of_interest}})
  desc_condition
}
# Apply to outcomes of interest
desc_abs_MA_condition <- calculate_descriptives_per_condition_and_outcome(jol_and_test_data, Abs_Mon_Acc)
desc_rel_MA_condition <- calculate_descriptives_per_condition_and_outcome(row_per_participant, Rel_Mon_Acc)
desc_test_condition <- calculate_descriptives_per_condition_and_outcome(jol_and_test_data, Test)
desc_JOL1_condition <- calculate_descriptives_per_condition_and_outcome(jol_and_test_data, JoL1)
desc_JOL2_condition <- calculate_descriptives_per_condition_and_outcome(jol_and_test_data, JoL2)

# Apply to outcomes of interest
desc_abs_MA_condition$Condition <- rename_conditions(desc_abs_MA_condition$Condition)
desc_rel_MA_condition$Condition <- rename_conditions(desc_rel_MA_condition$Condition)
desc_test_condition$Condition <-  rename_conditions(desc_test_condition$Condition)
desc_JOL1_condition$Condition <-  rename_conditions(desc_JOL1_condition$Condition)
desc_JOL2_condition$Condition <-  rename_conditions(desc_JOL2_condition$Condition)

##    JOL - Test ≤ 1                                                          ----
# Filter out the data of experiment 1 and count the number of observations
total_trials_study_1 <- jol_and_test_data %>% filter(Experiment == 1) %>% 
  ungroup() %>% 
  count()
# Now do the same but count how many participant had a monitoring accuracy <= 1
jols_below_equal_one_study_1 <- jol_and_test_data %>% 
  ungroup() %>% 
  filter(Experiment == 1 & Abs_Mon_Acc <= 1) %>% 
  count()
# Save the percentage of this number with the total score
jols_below_equal_one_study_1_perc <-round((jols_below_equal_one_study_1/total_trials_study_1)*100,2)

##    For the Cue Values, Cue Diagnosticty and Cue Utilization                ----
## Rename the levels of the Conditions for nicer printing
diagram_data_row_per_trial$Condition <- rename_conditions(diagram_data_row_per_trial$Condition)
# Reformat the grouping structure of the data frame:
# Retrieve the descriptives per Experiment and Condition
diagram_data_row_per_trial <- diagram_data_row_per_trial %>% group_by(Experiment, Condition)
## For all Cue Values, Diagnosticities and Utilizations of Correct, Commission and Omission boxes
descCue_Value_Correct    <- calculate_descriptives_of_corrs(diagram_data_row_per_trial, Cue_Value_Correct)
descCue_Value_Commission <- calculate_descriptives_of_corrs(diagram_data_row_per_trial, Cue_Value_Commission)
descCue_Value_Omission   <- calculate_descriptives_of_corrs(diagram_data_row_per_trial, Cue_Value_Omission)

## Reduce the diagram data to row per participant for the correlation scores
diagram_data_row_per_participant <- diagram_data_row_per_trial %>% 
  distinct(Participant, .keep_all = TRUE) %>% 
  select(c(Experiment, Participant, Condition, Standard, Diagramming,
           Cue_Diag_Correct, Cue_Diag_Commission, Cue_Diag_Omission, 
           Cue_Util_Correct, Cue_Util_Commission, Cue_Util_Omission))

# Redoe the grouping to prevent error messages with the function
diagram_data_row_per_participant <- diagram_data_row_per_participant %>% 
  group_by(Experiment, Condition)
## Cue Diagnosticities 
descCue_Diag_Correct    <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Diag_Correct)
descCue_Diag_Commission <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Diag_Commission)
descCue_Diag_Omission   <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Diag_Omission)
## Cue_Utilizations
descCue_Util_Correct    <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Util_Correct)
descCue_Util_Commission <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Util_Commission)
descCue_Util_Omission   <- calculate_descriptives_of_corrs(diagram_data_row_per_participant, Cue_Util_Omission)

## 5. Multilevel analyses                                                     ----
## Rename the condition levels again
jol_and_test_data$Condition <- rename_conditions(jol_and_test_data$Condition)
## Kick out the data of the second experiment
jol_and_test_data_study_1 <- jol_and_test_data %>% filter(Experiment == 1)

# Reorder the factor levels to have the Control Group as Reference
jol_and_test_data_study_1$Condition <- fct_relevel(jol_and_test_data_study_1$Condition, "No-Diagram-Control")

##    Absolute Monitoring Accuracy                                            ----
jol_and_test_data_study_1$Diagramming <- as.factor(jol_and_test_data_study_1$Diagramming)
jol_and_test_data_study_1$Standard <- as.factor(jol_and_test_data_study_1$Standard)
abs_mon_acc_full_model <- lmer(Abs_Mon_Acc ~ Diagramming*Standard + (1|Participant), 
                            data = jol_and_test_data_study_1)
abs_mon_acc_main_effects <- lmer(Abs_Mon_Acc ~ Diagramming+Standard + (1|Participant), 
                              data = jol_and_test_data_study_1)
# Print the output
abs_MA_models <- tab_model(abs_mon_acc_full_model, abs_mon_acc_main_effects, 
                          show.se = TRUE, show.std = TRUE)

## Run model comparison with anova() 
model_comparison_MA_pooled_fac <- anova(abs_mon_acc_full_model, abs_mon_acc_main_effects)
# and save the outcome + interpretation for printing with function above
p_model_comparison_MA_pooled_fac <- retrieve_anova_output(model_comparison_MA_pooled_fac)

# Per condition
abs_mon_acc_conditions <- lmer(Abs_Mon_Acc ~ Condition + (1|Participant), 
                             data = jol_and_test_data_study_1)
abs_mon_acc_conditions_mod <- tab_model(abs_mon_acc_conditions, 
                                      show.se = TRUE, show.std = TRUE)  

##    Pairwise comparisons                                                    
emm_MA <- emmeans(abs_mon_acc_conditions, list(pairwise ~ Condition), adjust = "tukey") 
# Retrieve the standardized effects of the pairwise comparisons,
# merge them to emm_Ma and create a ready-to-print kbl output
emm_MA <- return_print_ready_emmeans(emm_MA, abs_mon_acc_conditions)

##    Relative Monitoring Accuracy                                            ----
# Fit the model with the interaction
rel_mon_acc_full_mod <- lm(Rel_Mon_Acc ~ Diagramming*Standard, 
                         data = row_per_participant_data)
# Fit the model without the interaction
rel_mon_acc_red_mod <- lm(Rel_Mon_Acc ~ Diagramming+Standard, 
                        data = row_per_participant_data)
# Compare the fit of the two models
p_model_comparison_rel_MA <- anova(rel_mon_acc_full_mod, rel_mon_acc_red_mod)
# ..and save the outcome + interpretation for printing with function above
p_model_comparison_rel_MA <- retrieve_anova_output(p_model_comparison_rel_MA)

# Print the output
rel_acc_models <- tab_model(rel_mon_acc_red_mod, rel_mon_acc_full_mod, 
                           show.se = TRUE, show.std = TRUE)

##    Text Comprehension                                                      ----
# Only Diagramming and Standard as predictors
test_main_effects <- lmer(Test ~ Diagramming+Standard + (1|Participant), 
                         data = jol_and_test_data_study_1)

# Main Effects and interaction
test_interaction_models <- lmer(Test ~ Diagramming*Standard + (1|Participant), 
                               data = jol_and_test_data_study_1)
# Print the output
test_models <- tab_model(test_main_effects,test_interaction_models, show.se = TRUE, show.std = TRUE)  

## Run model comparison with anova() 
model_comparison_test_pooled_fac <- anova(test_main_effects, test_interaction_models)
# and save the outcome + interpretation for printing with function above
p_model_comparison_test_pooled_fac <- retrieve_anova_output(model_comparison_test_pooled_fac)

# Also per condition
test_conditions <- lmer(Test ~ Condition + (1|Participant), data = jol_and_test_data_study_1)
test_conditions_mod <- tab_model(test_conditions, show.se = TRUE, show.std = TRUE)  

##    Pairwise comparisons                                                    
emm_test <- emmeans(test_conditions, list(pairwise ~ Condition), adjust = "tukey") 
# Generate formatted output with standardized effects again (see function above)
emm_test <- return_print_ready_emmeans(emm_test, test_conditions)

##    JoL Analysis + Interaction plot                                         ----
## Transform data from wide to long format so that
#  JoL-Instance becomes one column and the values of JoL1 and JoL2 another
jol_data_long <- jol_and_test_data_study_1 %>% 
  select(Participant, Condition, JoL1, JoL2) %>% 
  pivot_longer(., 
               cols = c(JoL1, JoL2), 
               names_to = "JoL_Instance", 
               values_to = "JoL_Response")

## Run the multilevel analysis
# Fit the a multilevel model
jol_model_full <- lmer(JoL_Response ~ JoL_Instance*Condition + (1|Participant), 
                            data = jol_data_long)
joL_model_main_effects <- lmer(JoL_Response ~ JoL_Instance + Condition + (1|Participant), 
                              data = jol_data_long)
# Print the output
jol_tab_model <- tab_model(jol_model_full, show.se = TRUE, show.std = TRUE)

# Compare the two fitted models with anova()
jol_model_comparison <- anova(joL_model_main_effects, jol_model_full)
# and save the outcome + interpretation for printing with function above
p_jol_model_comparison <- retrieve_anova_output(jol_model_comparison)

## Pairwise comparisons 
##    Pairwise comparisons                                                    
emm_JOL <- emmeans(jol_model_full, list(pairwise ~ JoL_Instance*Condition), adjust = "tukey") 
# Generate formatted output with standardized effects again (see function above)
emm_JOL <- return_print_ready_emmeans(emm_JOL, jol_model_full)
# Add a scroll_box to the output as it's a long table
emm_JOL <- emm_JOL  %>%
  scroll_box(width = "100%", height = "400px")

## Create an interaction plot by plotting the mean difference between JoL1 and JoL2
# First retrieve the mean and standard deviation per condition 
desc_JOL <- jol_data_long %>% group_by(Condition, JoL_Instance) %>% 
  dplyr::summarize(Mean = round(mean(JoL_Response, na.rm = TRUE),2),
                   SD = round(sd(JoL_Response, na.rm = TRUE),2))
# Rename the levels of condition again
desc_JOL$Condition <- rename_conditions(desc_JOL$Condition) %>% 
  recode(`No-Diagram-Control` = "No-Diagram (Control)")
# Rename JOL for consistency with article
desc_JOL$JoL_Instance <- recode_factor(desc_JOL$JoL_Instance, JoL1 = "JOL1", JoL2 = "JOL2")

# Then plot the interaction
jol_interaction_plot <- desc_JOL %>% 
  ggplot(aes(x = JoL_Instance, 
             y = Mean, 
             group = Condition, 
             color = Condition, 
             ymin = 1, 
             ymax = 4)) +
  geom_errorbar(aes(ymin = Mean - SD, 
                    ymax = Mean + SD),
                width = .1) +
  geom_line(linewidth = 2, aes(linetype=Condition)) + # Add different line types
  # Choose line types manually
  scale_linetype_manual(values=c("dashed", "solid","dotted", "dotdash")) + 
  # Show them in legend respectively (and without the stripe)
  guides(color = guide_legend(override.aes = list(linetype = c(2, 1, 3, 4)))) +
  scale_color_brewer(palette="Paired") +
  ylab("JOL Score")+
  theme_bw()+ # Remove grey background
  theme(legend.key.width = unit(2,"cm"), # Change length of lines in legend
        axis.title.x=element_blank(), # Remove x-axis label
        text=element_text(family="Times New Roman", size=18)) #Times New Roman, 12pt


## 7. Compare Cue Values, Cue Diagnosticity & Cue Utilization                 ----
##    Between Conditions of Experiment 1                                      ----
# Filter the data of experiment 1
diagram_data_row_per_trial_study_1 <- diagram_data_row_per_trial %>% filter(Experiment == 1)
diagram_data_row_per_participant_study_1 <- diagram_data_row_per_participant %>% filter(Experiment == 1)
##    Run model with condition experiment for each outcome and adjust alpha   ----
# Cue Values
correct_cue_value_mod_study_1 <- lm(Cue_Value_Correct ~ Condition, data = diagram_data_row_per_trial_study_1)
commission_cue_value_mod_study_1 <- lm(Cue_Value_Commission ~ Condition, data = diagram_data_row_per_trial_study_1)
omission_cue_value_mod_study_1 <- lm(Cue_Value_Omission ~ Condition, data = diagram_data_row_per_trial_study_1)
# Print the output
cue_value_summary_study_1 <- tab_model(correct_cue_value_mod_study_1, commission_cue_value_mod_study_1, omission_cue_value_mod_study_1)
# Adjust all p values (for completeness, all previously non-significant anyway)
cue_values_study_1_adjusted_p <- round(p.adjust(p = c(retrieve_Value(correct_cue_value_mod_study_1, "Condition"),
                                                 retrieve_Value(commission_cue_value_mod_study_1, "Condition"),
                                                 retrieve_Value(omission_cue_value_mod_study_1, "Condition"))),3)
# Cue Diagnosticity and Cue Utilizations
correct_cue_diag_mod_study_1 <- lm(Cue_Diag_Correct ~ Condition, data = diagram_data_row_per_participant_study_1)
commission_cue_diag_mod_study_1 <- lm(Cue_Diag_Commission ~ Condition, data = diagram_data_row_per_participant_study_1)
omission_cue_diag_mod_study_1 <- lm(Cue_Diag_Omission ~ Condition, data = diagram_data_row_per_participant_study_1)
correct_cue_util_mod_study_1 <- lm(Cue_Util_Correct ~ Condition, data = diagram_data_row_per_participant_study_1)
Commission_cue_util_mod_study_1 <- lm(Cue_Util_Commission ~ Condition, data = diagram_data_row_per_participant_study_1)
omission_cue_util_mod_study_1 <- lm(Cue_Util_Omission ~ Condition, data = diagram_data_row_per_participant_study_1)
# Print the output separately for Diagnosticity and Utilization
cue_diag_summary_study_1 <- tab_model(correct_cue_diag_mod_study_1, commission_cue_diag_mod_study_1, omission_cue_diag_mod_study_1)
cue_util_summary_study_1 <- tab_model(correct_cue_util_mod_study_1, Commission_cue_util_mod_study_1, omission_cue_util_mod_study_1)
# Adjust all p values (for completeness, all previously non-significant anyway)
cue_corrs_adjusted_p_study_1 <- round(p.adjust(p = c(retrieve_Value(correct_cue_diag_mod_study_1, "Condition"),
                                           retrieve_Value(commission_cue_diag_mod_study_1, "Condition"),
                                           retrieve_Value(omission_cue_diag_mod_study_1, "Condition"),
                                           retrieve_Value(correct_cue_util_mod_study_1, "Condition"),
                                           retrieve_Value(Commission_cue_util_mod_study_1, "Condition"),
                                           retrieve_Value(omission_cue_util_mod_study_1, "Condition"))),3)

##    Between Studies/Experiments                                             ----
## Compare outcomes and correct alpha for p-values for multiple comparisons
# Cue Values
correct_cue_value_mod <- lm(Cue_Value_Correct ~ Experiment, data = diagram_data_row_per_trial)
commission_cue_value_mod <- lm(Cue_Value_Commission ~ Experiment, data = diagram_data_row_per_trial)
omission_cue_value_mod <- lm(Cue_Value_Omission ~ Experiment, data = diagram_data_row_per_trial)
# Print the output
cue_value_summary <- tab_model(correct_cue_value_mod, commission_cue_value_mod, omission_cue_value_mod)
# Adjust all p values (for completeness, all previously non-significant anyway)
cue_values_adjusted_p <- round(p.adjust(p = c(retrieve_Value(correct_cue_value_mod, "Experiment"),
                                           retrieve_Value(commission_cue_value_mod, "Experiment"),
                                           retrieve_Value(omission_cue_value_mod, "Experiment"))),3)

# Cue Diagnosticity and Cue Utilizations
correct_cue_diag_mod <- lm(Cue_Diag_Correct ~ Experiment, data = diagram_data_row_per_participant)
commission_cue_diag_mod <- lm(Cue_Diag_Commission ~ Experiment, data = diagram_data_row_per_participant)
omission_cue_diag_mod <- lm(Cue_Diag_Omission ~ Experiment, data = diagram_data_row_per_participant)
correct_cue_util_mod <- lm(Cue_Util_Correct ~ Experiment, data = diagram_data_row_per_participant)
commission_cue_util_mod <- lm(Cue_Util_Commission ~ Experiment, data = diagram_data_row_per_participant)
omission_cue_util_mod <- lm(Cue_Util_Omission ~ Experiment, data = diagram_data_row_per_participant)
# Print the output separately for Diagnosticity and Utilization
cue_diag_summary <- tab_model(correct_cue_diag_mod, commission_cue_diag_mod, omission_cue_diag_mod)
cue_util_summary <- tab_model(correct_cue_util_mod, commission_cue_util_mod, omission_cue_util_mod)
# Adjust all p values (for completeness, all previously non-significant anyway)
cue_corrs_adjusted_p <- round(p.adjust(p = c(retrieve_Value(correct_cue_diag_mod, "Experiment"),
                                    retrieve_Value(commission_cue_diag_mod, "Experiment"),
                                    retrieve_Value(omission_cue_diag_mod, "Experiment"),
                                    retrieve_Value(correct_cue_util_mod, "Experiment"),
                                    retrieve_Value(commission_cue_util_mod, "Experiment"),
                                    retrieve_Value(omission_cue_util_mod, "Experiment"))),3)



## 8. Format all output tables for the RMardown Summary file                  ----
##    Sample Stats                                                            ----
## age 
desc_age_sample_exp_1_n_80 <- kbl(desc_age_sample_exp_1_n_80, 
                          caption = "Descriptives of Age n = 80") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_left")
## gender
n_gender_sample_exp_1_n_80 <- kbl(n_gender_sample_exp_1_n_80, 
                           caption = "Gender Distribution n = 80") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_left")

## age
desc_age_sample_exp_1_n_79 <- kbl(desc_age_sample_exp_1_n_79, 
                               caption = "Descriptives of Age n = 79") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_right")
## gender
n_gender_sample_exp_1_n_79 <- kbl(n_gender_sample_exp_1_n_79, 
                               caption = "Gender Distribution n = 79") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_right")

## Merge drop out and time on task tables
desc_conditions <- full_join(n_per_condition, desc_time_on_task) %>% 
  group_by(Condition) %>%
  rename(Final = "n") %>% 
  relocate(Final, .after = Drop_Out) %>% 
  relocate(Experiment) 

## Manually modify the n and drop-out of Experiment 2 (based on log data)
desc_conditions$Drop_Out[desc_conditions$Experiment == 2] <- "0+(2*)"
desc_conditions$Final[desc_conditions$Experiment == 2] <- "22(20)"

# Format the kable table
desc_conditions <- kbl(desc_conditions, 
                       caption = "n (Previous Drop-Out & Final) and Time On Task per Condtition",  
                       align = "clcccccc") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "float_left") %>%
  add_header_above(c(" " = 2,"n" = 2, "Time On Task" = 4))

##    Descriptive statictics all outcomes                                     ----
## Per Pooled Factor
desc_pooled_factors <- rbind(
  cbind(descriptives_abs_MA_factor[,-1],
        descriptives_rel_MA_factor[,-1],
        descriptives_testScore_factor[,-1],
        descriptives_JoL1_factor[,-1],
        descriptives_JoL2_factor[,-1])) %>% 
    kbl(col.names = gsub("\\...\\d+", "", names(.)), 
        caption = "Descriptive Per Factors - Pooled Conditions") %>%
  remove_column(c(6,11,16, 21)) %>% 
  kable_paper("striped", full_width = TRUE) %>%
  add_header_above(c(" " = 1, "Abs. Monitoring Acc." = 4, "Rel. Monitoring Acc." = 4, "Text Comprehension" = 4, "JOL1" = 4, "JOL2" = 4)) %>% 
  pack_rows("Diagramming", 1, 2) %>%
  pack_rows("Standard", 3, 4) %>%
  scroll_box(width = "100%", height = "300px") %>%
  kable_styling(full_width = TRUE)

## Per Condition
desc_Conditions <- rbind(
  cbind(desc_abs_MA_condition[,-1],
        desc_rel_MA_condition[,-1],
        desc_test_condition[,-1],
        desc_JOL1_condition[,-1],
        desc_JOL2_condition[,-1])) %>%
  kbl(col.names = gsub("\\...\\d+", "", names(.)), 
      caption = "Descriptive Per Condition") %>%
  kable_paper("striped", full_width = TRUE) %>%
  remove_column(c(6,11,16, 21)) %>%
  add_header_above(c(" " = 1, "Abs. Monitoring Acc." = 4, "Rel. Monitoring Acc." = 4, "Text Comprehension" = 4, "JOL1" = 4, "JOL2" = 4)) %>% 
  pack_rows("Study 1", 1, 4) %>%
  pack_rows("Study 2", 5, 5) %>%
  scroll_box(width = "100%", height = "300px") %>%
  kable_styling(full_width = TRUE)

##    Inter-Rater-Reliabilities                                               ----
## Create a formatted table for the IRRs
irr_diagram_total <- c('Total Correct Scores', round(irr_diagram_total$value,2))
irr_diagram_correct <- c('Correct Box Scores', round(irr_diagram_correct$value,2))
irr_diagram_element_no <- c('Element Numbers', round(irr_diagram_element_no$value,2))
irr_diagram_commission <- c('Commission Errors', round(irr_diagram_commission$value,2))
irr_test_total_scores <- c('Test Total Scores', round(irr_test_total_scores$value,2))

all_IRRs <- kbl(rbind(irr_diagram_total, 
                     irr_diagram_correct, 
                     irr_diagram_element_no, 
                     irr_diagram_commission,
                     irr_test_total_scores,
                     deparse.level = 0),
               caption = "Inter-Rater-Reliabilities of Diagram and Test Responses (squared kappa's)") %>%
  kable_paper("striped", full_width = F) %>%
  kable_styling(full_width = FALSE, position = "right") %>%
  pack_rows("Diagram Responses", start_row = 1, end_row = 4) %>%
  pack_rows("Test Responses", start_row = 5, end_row = 5) 

##    Descriptives Cue Values, Cue Diagnosticity and Cue Utilization          ----
desc_Cues <- rbind(
  cbind(descCue_Value_Correct, descCue_Value_Commission, descCue_Value_Omission),  
  cbind(descCue_Diag_Correct, descCue_Diag_Commission, descCue_Diag_Omission),
  cbind(descCue_Util_Correct, descCue_Util_Commission, descCue_Util_Omission)) %>% 
  kbl(col.names = gsub("\\...\\d+", "", names(.)),
  caption = "Descriptive Statistics Cue Values, Cue Diagnosticity and Cue Utilization",
  align = "crccccccccccccccc") %>%
  remove_column(c(8,9,15,16)) %>%
  kable_paper("striped", full_width = F) %>%
  add_header_above(c(" " = 2, "Correct" = 5, "Commission" = 5, "Omission" = 5)) %>% 
  column_spec(2, border_right=T) %>%
  column_spec(7, border_right=T) %>%
  column_spec(12, border_right=T) %>%
  pack_rows("Cue Value", 1, 3) %>%
  pack_rows("Cue Diagnosticity", 4, 6) %>%
  pack_rows("Cue Utilization", 7, 9) %>%
  scroll_box(width = "100%", height = "300px")

## 9. Clean up and save workspace                                             ----
rm(processed_consort_data_study_1, drop_out_2_merge,
   correct_cue_value_mod, commission_cue_value_mod, omission_cue_value_mod,
   correct_cue_diag_mod, commission_cue_diag_mod, omission_cue_diag_mod,
   correct_cue_util_mod, commission_cue_util_mod, omission_cue_util_mod,
   total_trials_study_1, jols_below_equal_one_study_1, descriptives_abs_MA_factor,
   descriptives_rel_MA_factor, descriptives_testScore_factor,
   descriptives_JoL1_factor, descriptives_JoL2_factor,
   desc_abs_MA_condition, desc_rel_MA_condition, desc_test_condition, 
   desc_JOL1_condition, desc_JOL2_condition,
   irr_diagram_total, irr_diagram_correct, irr_diagram_element_no, irr_diagram_commission, irr_test_total_scores,
   descCue_Value_Correct, descCue_Value_Commission, descCue_Value_Omission,
   descCue_Diag_Correct, descCue_Diag_Commission, descCue_Diag_Omission,
   descCue_Util_Correct, descCue_Util_Commission, descCue_Util_Omission)

## Save current date to append to output
today <- Sys.Date()
today <- format(today, format="%d%b%Y")

#save(OBJECTNAME_YOU_WANT_TO_SAVE, file = paste0(rel_path_for_output, "data_analysis_processed_performance_data_", today, ".RData"))

############################################################################  ----