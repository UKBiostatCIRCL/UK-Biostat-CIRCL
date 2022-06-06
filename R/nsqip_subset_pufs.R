#### NSQIP Dataset Pull, Select, Filter, and Merge ####

#' Importing the raw National Surgical Quality Improvement Program (NSQIP) Dataset
#'
#'
#' This function is designed to specify a directory with multiple NSQIP datasets to be filtered and merged.
#' Due to the size of many NSQIP datasets, importing each is likely to cause out-of-memory errors on most personal computers.
#' This function is intended to be run prior to running the nsqip_clean_data() function. Exports a merged dataframe object.
#'
#' @param puf_folder_path a text string representing the path of the folder containing the PUF text files; the text string must not end with "/"!
#' @param puf_years the years to include in the extract
#' @param cpt_codes the CPT codes to include in the extract. If NULL, then return all codes.
#'            Based on the following fields: "CPT", "OTHERCPT1" - "OTHERCPT10", and "CONCPT1" - "CONCPT10"
#' @param cpt_codes_primary_only if TRUE, then only consider cases where one of the "cpt_codes" is the primary CPT code (the "CPT" field).
#'                        Ignore if cpt_codes = NULL.
#' @param icd10_codes a character vector of ICD-10 codes to subset the extract to. Ignore when NULL.
#'              Based on the following fields: "PODIAG10", "PODIAG_OTHER10", "REOPOR1ICD101", "REOPOR2ICD101", "READMRELICD101", "READMUNRELICD101",
#'                                             "READMRELICD102", "READMUNRELICD102", "READMRELICD103", "READMUNRELICD103", "READMRELICD104", "READMUNRELICD104",
#'                                             "READMRELICD105", "READMUNRELICD105"
#' @param icd9_codes a character vector of ICD-9 codes to subset the extract to (these become rare in more recent years). Ignore when NULL.
#'             Based on the following fields: "PODIAG", "PODIAG_OTHER", "REOPORICD91", "REOPOR2ICD91", "READMRELICD91", "READMUNRELICD91", "READMRELICD92",
#'                                            "READMUNRELICD92", "READMRELICD93", "READMUNRELICD93", "READMRELICD94", "READMUNRELICD94", "READMRELICD95",
#'                                            "READMUNRELICD95"
#' @param icd_codes_primary_only if TRUE, then only use the "PODIAG10" and "PODIAG" fields for filtering.
#' @param surgical_specialty a character vector of surgical specialty to subset to (based on the "SURGSPEC" field).
#'
#'
#' @import janitor data.table plyr dplyr here
#'
#'
#' @export

## The function
nsqip_subset_pufs <- function(puf_folder_path = "Raw Files",
                              puf_years = 2012:2020,
                              cpt_codes = NULL,
                              cpt_codes_primary_only = TRUE,
                              icd10_codes = NULL,
                              icd9_codes = NULL,
                              icd_codes_primary_only = TRUE,
                              surgical_specialty = NULL){

  ## Packages needed for the function
  require(janitor)
  require(data.table)
  require(plyr)
  require(dplyr)
  require(here)

  ## A list of columns containing cpt codes
  cpt_columns <- c("cpt","concpt1","concpt2","concpt3","concpt4","concpt5","concpt6","concpt7","concpt8","concpt9","concpt10",
                   "othercpt1","othercpt2","othercpt3","othercpt4","othercpt5","othercpt6","othercpt7","othercpt8","othercpt9","othercpt10")

  ## A list of columns containing icd10 codes
  icd10_columns <- c("podiag10", "podiagother10","reopor1icd101","reopor2icd101","readmrelicd101",
                     "readmunrelicd101","readmrelicd102","readmunrelicd102","readmrelicd103","readmunrelicd103",
                     "readmrelicd104","readmunrelicd104","readmrelicd105","readmunrelicd105")

  ## A list of columns containing icd9 codes
  icd9_columns <- c("podiag","podiagother","reoporicd91","reopor2icd91","readmrelicd91",
                    "readmunrelicd91","readmrelicd92","readmunrelicd92","readmrelicd93","readmunrelicd93",
                    "readmrelicd94","readmunrelicd94","readmrelicd95","readmunrelicd95")

  ## This list matches a year to a file name (used for filtering by year)
  # file_dictionary <- list(
  #   list(year = 2019, file = "Raw Files\\acs_nsqip_puf19.txt"),
  #   list(year = 2018, file = "Raw Files\\acs_nsqip_puf18_v2.txt"),
  #   list(year = 2017, file = "Raw Files\\acs_nsqip_puf17.txt"),
  #   list(year = 2016, file = "Raw Files\\acs_nsqip_puf16.txt"),
  #   list(year = 2015, file = "Raw Files\\acs_nsqip_puf15_v2.txt"),
  #   list(year = 2014, file = "Raw Files\\acs_nsqip_puf14.txt"),
  #   list(year = 2013, file = "Raw Files\\acs_nsqip_puf13.txt"),
  #   list(year = 2012, file = "Raw Files\\acs_nsqip_puf12.txt"))
  file_dictionary <- list(
    list(year = 2020, file = paste0(puf_folder_path, "/acs_nsqip_puf20.txt")),
    list(year = 2019, file = paste0(puf_folder_path, "/acs_nsqip_puf19.txt")),
    list(year = 2018, file = paste0(puf_folder_path, "/acs_nsqip_puf18_v2.txt")),
    list(year = 2017, file = paste0(puf_folder_path, "/acs_nsqip_puf17.txt")),
    list(year = 2016, file = paste0(puf_folder_path, "/acs_nsqip_puf16.txt")),
    list(year = 2015, file = paste0(puf_folder_path, "/acs_nsqip_puf15_v2.txt")),
    list(year = 2014, file = paste0(puf_folder_path, "/acs_nsqip_puf14.txt")),
    list(year = 2013, file = paste0(puf_folder_path, "/acs_nsqip_puf13.txt")),
    list(year = 2012, file = paste0(puf_folder_path, "/acs_nsqip_puf12.txt")))

  ## An empty data frame to store the data (with the names of the most recent year)
  dat <- janitor::clean_names(fread(file = file_dictionary[[1]]$file,
                                    sep = "\t",
                                    header = T,
                                    nrows = 0))
  names(dat) <- gsub("\\_","", names(dat))

  ## This loop checks whether to download each file (determined by puf_years), standardizes the column names,
  ## filters the data and merges it all together.
  for (i in 1:length(file_dictionary)){

    if (file_dictionary[[i]]$year %in% puf_years){
      dat_year <- janitor::clean_names(fread(file = file_dictionary[[i]]$file,
                                             sep = "\t",
                                             header = T))
      names(dat_year) <- gsub("\\_","", names(dat_year))
      dat_year$pufyear <- file_dictionary[[i]]$year

      ## Merging the data together
      dat <- plyr::rbind.fill(dat, dat_year)

      ## Removing an unneeded data set from memory
      rm(dat_year)

      ## This filters the data by cpt codes only in the "cpt" column
      if (!is.null(cpt_codes) & cpt_codes_primary_only == TRUE){
        dat <- dat %>%
          filter(cpt %in% cpt_codes)
      }

      ## This filters the data by cpt codes in all the cpt columns
      if (!is.null(cpt_codes) & cpt_codes_primary_only == FALSE){
        dat <- dat %>%
          filter(if_any(all_of(cpt_columns),
                        function(x) x %in% cpt_codes))
      }

      ## This filters the data by icd (9 or 10) codes found only in the pontiag or pontiag10 columns
      if ((!is.null(icd9_codes) | !is.null(icd10_codes)) & icd_codes_primary_only == TRUE){
        dat <- dat %>%
          filter(if_any(c(podiag,podiag10),
                        function(x) x %in% c(icd9_codes,icd10_codes)))
      }

      ## This filters the data by icd (9 or 10) codes found in all the icd columns
      if ((!is.null(icd9_codes) | !is.null(icd10_codes)) & icd_codes_primary_only == FALSE){
        dat <- dat %>%
          filter(if_any(all_of(c(icd9_columns,icd10_columns)),
                        function(x) x %in% c(icd9_codes,icd10_codes)))
      }

      ## This filters the data by surgical specialty
      if(! is.null(surgical_specialty)){
        dat <- dat %>%
          filter(surgspec %in% surgical_specialty)
      }

      ## Print the progress of the merge/subset
      print( paste0("Imported and merged ", i, " of ", length(file_dictionary), " possible PUFs" ) )

    }
  }

  ## Return the subsetted and merged data
  return(dat)

}
