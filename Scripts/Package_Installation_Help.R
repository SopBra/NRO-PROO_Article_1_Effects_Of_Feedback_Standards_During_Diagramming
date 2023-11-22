############################################################################  --
##    This is an optional installation help of the required packages when 
##                    knitting .Rmd files for the first time
##
##                           Sophia Braumann                                  
##                       Last Updated: December 2023                          
############################################################################# -- 
##    (Install if necessary and) load relevant libraries                           
#  Check and perform installation of relevant packages if needed
list_of_packages <- c("base64enc", "digest", "evaluate", "glue", 
                      "highr", "htmltools", "jsonlite", "knitr", 
                      "magrittr", "markdown", "mime", "rmarkdown", 
                      "stringi", "xfun", "yaml", 
                      "rmdformats", # Until here, all required for knitting the output with RMarkdown
                      "tidyverse",  # for dplyr with distinct, filter, %>%, and many more
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
                      "kableExtra",  # for kable/HTML tables
                      "Hmisc"      # for gamma correlations
                      ) 

# List of relevant packages
new_packages <- list_of_packages[!(list_of_packages %in% installed.packages()[,"Package"])] # Check whether all packages are installed
if(length(new_packages)) install.packages(new_packages, type = "binary") # Install them if that is not the case
## Load the relevant libraries
sapply(list_of_packages, require, character.only = TRUE)


##    Altert about R and package versions                                     ----
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
package_infos_at_script_creation <- read_delim('Data/Session_Info_At_Script_Creation/package_infos_author.txt')

# Function to retrieve the package versions of the user
retrieve_package_versions <- function(incoming_package_info) {
  package_version <- incoming_package_info$Version
  package_version
}

# Retrieve the package versions of the user
package_infos_user <- lapply(session_info_user$otherPkgs, function(x) retrieve_package_versions(x))
package_infos_user<- data.frame(t(sapply(package_infos_user,cbind))) %>% 
  pivot_longer(cols = everything(), names_to = 'Packages', values_to = 'Versions')

# Compare the all the package versions
package_version_altert <- ifelse(all(package_infos_user$Packages == package_infos_at_script_creation$Packages &
                                       package_infos_user$Versions == package_infos_at_script_creation$Versions),
                                 paste0('All packages are loaded in the same version as at time of the script creation, you are good to go :)'),
                                 paste0('One or more packages are loaded at a different version on your system compared to the moment of the script creation. If you encounter malfuncting of the script due to outdata package versions, please report back to the author'))

package_version_altert

############################################################################# -- 

