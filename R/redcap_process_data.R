#### REDCap Pre-processing Data ####


#' Pre-processing data exported from REDCap for use in R
#'
#' This function is designed to apply basic routine cleaning procedures to a .csv file extracted from REDCap with
#' an accompanying R script without manual specification and coding. This includes removing the duplicate ".factor" variables
#' from the dataset while retaining the REDCap-supplied variable labels. Returns a cleaned dataframe object.
#'
#' @param folder_name_with_redcap_files = the name of the folder containing both the REDCap data file and the REDCap R script
#'                                 (which should be under the current project's directory)
#' @param redcap_data_file_name = the name of the REDCap data file, including the ".csv" file type
#' @param redcap_r_script_name = the name of the REDCap R script file, including the ".R" file type
#' @param create_combined_factor_variables = TRUE/FALSE as to whether or not variables with similar names should be combined
#'                                    into a single overall variable if the sub-variables are found to be mutually exclusive
#'
#' @import here stringr Hmisc
#'
#' @export



##############
## Function ##
##############

## A function to process the REDCap data
redcap_process_data <- function(folder_name_with_redcap_files = "data",
                                redcap_data_file_name = "data-file-name.csv",
                                redcap_r_script_name = "r-script-name.R",
                                create_combined_factor_variables = TRUE){

  ## Load packages
  require(here)
  require(stringr)
  require(Hmisc)



  ###########################################
  ## Run the R Script and Read in the Data ##
  ###########################################

  ## These steps are inspired by the following link:
  ## https://stackoverflow.com/questions/26245554/execute-a-set-of-lines-from-another-r-file

  ## Identify the location of the REDCap R script
  loc_script <- here::here(folder_name_with_redcap_files, redcap_r_script_name)
  loc_file <- here::here(folder_name_with_redcap_files, redcap_data_file_name)

  ## Read in the R script, excluding the first few lines that clear the workspace
  lines <- scan(file = loc_script,
                what = character(),
                sep = "\n",
                skip = 3,
                quiet = TRUE)

  ## Which row contains the data import statement?
  row_data <- which( apply(X = as.matrix(lines),
                           MARGIN = 1,
                           FUN = function(x){
                             return( stringr::str_detect(string = x,
                                                         pattern = "read.csv") )
                           } ) )

  ## Replace the data file location
  lines[row_data] <- stringr::str_replace(string = lines[row_data],
                                          pattern = redcap_data_file_name,
                                          replacement = loc_file)
  rm(row_data)

  ## Create a text connection
  tc <- textConnection(lines)

  ## Source the text connection
  source(tc)
  close(tc)



  #############################
  ## Process the REDCap Data ##
  #############################

  ## Identify all of the variable groups
  vars_factor <- names(data)[ stringr::str_detect(string = names(data), pattern = ".factor") ]
  vars <- stringr::str_remove(string = vars_factor, pattern = ".factor")
  vars_other <- names(data)[ !names(data) %in% c(vars, vars_factor) ]
  var_groups <- unique(gsub("\\___.*","",vars))

  ## Identify the variable label groups
  labs_all <- Hmisc::label(data)
  labs_vars <- labs_all[ names(data) %in% vars ]
  labs_final <- labs_all[ names(data) %in% c(vars_other, vars) ]

  ## Code to create combined variables for the factors that are mutually exclusive
  if( create_combined_factor_variables ){
    vars_to_remove <- c()
    for (var in var_groups){
      if(length(vars[var == gsub("\\___.*","",vars)]) >1) {
        if(max(rowSums(data[,vars[var == gsub("\\___.*","",vars)]])) == 1){
          for (choice in vars[var == gsub("\\___.*","",vars)]){
            #print(choice)
            label <- gsub(").*","",gsub(".*=","",labs_vars[vars == choice]))
            print(label)
            data[data[,choice]==1,var] <- label
            vars_to_remove <- c(vars_to_remove, choice)
          }
          data[,var] <- factor(data[,var])
        }
      }
  }

  ## Deleting variables that were replaced by a combined variable
  data <- data[, ! names(data) %in% vars_to_remove]

  }

  ## Update labels for the factor variables
  for (i in 1:length(vars)){
    Hmisc::label(data[, vars_factor[i]]) <- labs_vars[i]
  }

  ## Replacing "Checked" and "Unchecked" with "Yes" and "No"
  for (i in 1:length(vars_factor)){
    if (setequal(levels(data[, vars_factor[i]]),c("Unchecked","Checked"))){
      levels(data[, vars_factor[i]]) <- c("No","Yes")
    }
  }

  ## Replacing the original variables with Redcap's new factor variables
  data[, vars] <- data[, vars_factor]

  ## Deleting the duplicate factor variables
  data <- data[, ! names(data) %in% vars_factor]

  ## Return the processed data
  return(data)
  rm(data)

}



