#### Function to Create Overall and Stratified Tables ####

#' Creating Overall and Stratified Tables
#'
#' This function is designed to create a summary table using lists of continuous and categorical variables (specified).
#' Overall or stratified tables can be constructed or if a stratification variable is included. Additionally,
#' p-values are output using either ANOVA (for continuous variables) or Chi-square (for categorical variables) tests.
#'
#' @param dataset A data.frame object containing, at minimum, all of the variables contained in the table.
#' @param strata.variable = the name of the categorical/pordinal variable that you wish to stratify on. May also be left as NULL for an overall table.
#' @param variables = a vector of the variable names you wish to include as rows in the table.
#' @param variable.labels = an optional vector of labels that will be displayed instead of the variable name, Must be the same length as the variables vector.
#' @param factor.variables = an optional vector of variable names that are numeric but should be coereced into categorical.
#' @param factors_reported.categorical.analyzed.ordinal = an optional vector of variable names where frequencies are reported but the variables are analyzed as ordinal.
#' @param exact.variables = an optional vector of variable names where Fisher's exact should be used.
#' @param exact.variables_detect.and.add.to.list = TRUE/FALSE whether or not to run a chi-square test on the categorical variables and if a warning is produced run Fisher's exact.
#' @param categorical.variables_force.exclusion.from.exact.analysis = an optional vector of variable names that will be coerced into the chi-square test.
#' @param categorical.variables_report.all.levels = if TRUE, then both levels of two-level categorical variables will be reported
#' @param non.normal.variables = an optional vector of variable names where a non-parametric test will be used instead of a t-test/one-way ANOVA.
#' @param all.patients.row.name = a character string that will be displayed in the top row of the table which typically contrains the strata-specific frequencies.
#' @param descriptors_include = TRUE/FALSE to include the statistic description after the variable label in the rows of the table.
#' @param descriptor_categorical = a character string of the descriptor to include after categorical variables.
#' @param descriptor_continuous_default = a character string of the descriptor to include after continuous variables.
#' @param descriptor_continuous_nonnormal = a character string of the descriptor to include after non-normal continuous variables.
#' @param digits_pvalue = a numeric value for the number of digits to report for p-values.
#' @param digits_categorical = a numeric value for the number of digits to report for percentages.
#' @param digits_continuous = a numberic value for the number of digits to report for numeric values.
#' @param missing.data_include.as.list = TRUE/FALSE for whether or not to generate a character string to describe the variables with missing data.
#' @param missing.data_include.as.separate.table = TRUE/FALSE for whether or not to generate a separate table summarizing the variables with missing data.
#' @param missing.data_add.to.table = TRUE/FALSE whether or not to add an additional row below a variable for the purposes of reporting missing values.
#' @param missing.data_row.label =  a character string for the row label that will be used for missing data rows
#' @param missing.data_value.prefix = a character string for the pre-fix to include before the missing data frequencies in the body of the table.
#'
#' @keywords Summary Table
#'
#' @import tableone stringr testit
#'
#'
#'
#' @export







#####################################################################
## Function to Combine Overall and Stratified Tables from tableone ##
#####################################################################

Overall.And.Stratified = function(dataset = dat,
                                  strata.variable = NULL,
                                  variables,
                                  variable.labels = NULL,
                                  factor.variables = NULL,
                                  factors_reported.categorical.analyzed.ordinal = NULL,
                                  exact.variables = NULL,
                                  exact.variables_detect.and.add.to.list = TRUE,
                                  categorical.variables_force.exclusion.from.exact.analysis = NULL,
                                  categorical.variables_report.all.levels = FALSE,
                                  non.normal.variables = NULL,
                                  all.patients.row.name = "All Patients",
                                  descriptors_include = TRUE,
                                  descriptor_categorical = ", N (%)",
                                  descriptor_continuous_default = ", Mean (SD)",
                                  descriptor_continuous_nonnormal = ", Median [Q1,Q3]",
                                  digits_pvalue = 3,
                                  digits_categorical = 1,
                                  digits_continuous = 2,
                                  missing.data_include.as.list = TRUE,
                                  missing.data_include.as.separate.table = TRUE,
                                  missing.data_add.to.table = FALSE,
                                  missing.data_row.label = "Missing Obs.",
                                  missing.data_value.prefix = "N="){

  require(tableone)
  require(stringr)
  require(testit)



  ###################
  ## Overall Table ##
  ###################

  ## Create the table
  tab = CreateTableOne(vars = variables,
                       factorVars = factor.variables,
                       data = dataset,
                       test = TRUE)

  ## Convert the table to a dataframe
  tab = data.frame( print(tab,
                          explain = FALSE,
                          printToggle = FALSE,
                          varLabels = FALSE,
                          dropEqual = TRUE,
                          exact = exact.variables,
                          nonnormal = non.normal.variables,
                          pDigits = digits_pvalue,
                          catDigits = digits_categorical,
                          contDigits = digits_continuous,
                          noSpaces = TRUE,
                          showAllLevels = categorical.variables_report.all.levels),
                    stringsAsFactors = FALSE)




  ######################
  ## Stratified Table ##
  ######################

  ## Assume unequal variances no matter how many levels there are in the strata
  tmp_var.equal = FALSE

  ## Create a fake strata variable if one wasn't provided
  if( is.null(strata.variable) ){
    strata_var <- sample(x = c(0, 1),
                         size = nrow(dataset),
                         replace = TRUE)
    strata_var_name <- "Strata"
    dataset[, strata_var_name ] <- strata_var
  } else{
    strata_var <- dataset[, strata.variable ]
    strata_var_name <- strata.variable
  }

  ## Only do if the stratified table is requested
  if( length(strata_var) > 0 ){

    ## Create the stratified table
    tab.strat = CreateTableOne(vars = variables,
                               factorVars = factor.variables,
                               strata = strata_var_name,
                               data = dataset,
                               argsExact = list(simulate.p.values = TRUE, B = 10000),
                               argsNormal = list(var.equal = tmp_var.equal),
                               test = TRUE)

    ## Identify variables where an exact analysis is necessary
    if( exact.variables_detect.and.add.to.list == TRUE ){

      ## Are there any categorical variables?
      if( !identical(x = tab.strat$MetaData$vars, y = tab.strat$MetaData$varNumerics)  ){

        ## Extract the variables
        cat.vars = tab.strat$MetaData$vars
        num.vars = tab.strat$MetaData$varNumerics
        cat.vars = cat.vars[ !cat.vars %in% num.vars ]
        rm(num.vars)

        ## For each categorical variable, see if the chi-square test results in a warning
        chi.square.warning = rep(NA, length(cat.vars))
        for(i in 1:length(chi.square.warning)){
          chi.square.warning[i] = has_warning( chisq.test( table(dataset[,strata_var_name], dataset[,cat.vars[i]]) ) )
        }
        rm(i)

        ## Do any variables need an exact analysis?
        if( sum(chi.square.warning) > 0 ){

          ## Include the variables with warnings as exact variables
          exact.variables = c(exact.variables, cat.vars[ chi.square.warning ])

          ## Remove duplicates
          exact.variables = unique(exact.variables)

          ## Remove variables that were requested to be excluded
          exact.variables = exact.variables[ !exact.variables %in% categorical.variables_force.exclusion.from.exact.analysis ]
        }
        rm(cat.vars, chi.square.warning)
      }
    }

    ## Re-run the table to include exact analyses
    tab.strat = CreateTableOne(vars = variables,
                               factorVars = factor.variables,
                               strata = strata_var_name,
                               data = dataset,
                               argsNormal = list(var.equal = tmp_var.equal),
                               test = TRUE)

    ## Covert the table to a dataframe
    tab.strat.df = data.frame( print(tab.strat,
                                     explain = FALSE,
                                     printToggle = FALSE,
                                     varLabels = FALSE,
                                     dropEqual = TRUE,
                                     exact = exact.variables,
                                     nonnormal = non.normal.variables,
                                     pDigits = digits_pvalue,
                                     catDigits = digits_categorical,
                                     contDigits = digits_continuous,
                                     noSpaces = TRUE,
                                     showAllLevels = categorical.variables_report.all.levels),
                               stringsAsFactors = FALSE)

    ## Extract the test information
    test.info = data.frame(Variable = row.names(tab.strat.df), Test = tab.strat.df$test, stringsAsFactors = FALSE)
    test.info = test.info[ !test.info$Variable == "n", ]
    test.info = test.info[ test.info$Variable %in% variables, ]

    ## Idenfity the test used
    test.info$Test[ test.info$Test == "" & test.info$Variable %in% tab.strat$MetaData$varNumerics ] = "Default Continuous"
    test.info$Test[ test.info$Test == "" & test.info$Variable %in% tab.strat$MetaData$vars[ !tab.strat$MetaData$vars %in% tab.strat$MetaData$varNumerics ] ] = "Default Categorical"
    row.names(test.info) = NULL

    ## Simulate the Fisher's exact p-value if it failed
    if( sum( tab.strat.df$p[ tab.strat.df$test == "exact" ] == "NA" ) > 0 ){

      ## Identify the variables that failed
      failed.exact = row.names(tab.strat.df)[ tab.strat.df$p == "NA" & tab.strat.df$test == "exact" ]

      ## Re-calculate the Fisher's exact p-value using simulation
      for(i in 1:length(failed.exact)){

        ## Calculate the p-value
        p.val = fisher.test(x = table( dataset[,strata_var_name], dataset[, failed.exact[i] ] ),
                            simulate.p.value = TRUE,
                            B = 10000)$p.value
        p.val = round(p.val, digits_pvalue)

        ## Edit p-values of 0
        if( p.val == 0 ){
          zeros = paste( rep("0", (digits_pvalue-1)), collapse = "" )
          p.val = paste0("<0.", zeros, "1") }
        else{ p.val = as.character(p.val) }

        ## Add extra 0's if the p-value was rounded too much
        if( nchar(p.val) < (digits_pvalue + 2) ){ p.val = str_pad(string = p.val,
                                                                  width = (digits_pvalue + 2),
                                                                  side = "right",
                                                                  pad = "0") }

        ## Replace the p-value
        tab.strat.df$p[ row.names(tab.strat.df) == failed.exact[i] ] = p.val
      }
      rm(i, failed.exact, p.val)
    }

    ## Remove the test column
    tab.strat.df = tab.strat.df[, !names(tab.strat.df) == "test"]

    ## Combine the overall and stratified tables
    tab = merge(x = tab, y = tab.strat.df, by = "row.names", sort = FALSE)
    rm(tab.strat, tab.strat.df)
  }




  #########################################
  ## Edit the Factor Variable Row Labels ##
  #########################################

  ## Are there any factor/multi-level categorical variables?
  if( nrow(tab) > (length(variables) + 1) ){

    ## Identify the row labels
    classify.row.labels = data.frame(Row.Label = tab$Row.names,
                                     Associated.Variable = NA,
                                     New.Label = NA,
                                     stringsAsFactors = FALSE)

    ## Fill in the first row
    classify.row.labels$Associated.Variable[1] = classify.row.labels$New.Label[1] = "n"

    ## Identify which variable each row is associated with
    for(i in 2:nrow(classify.row.labels)){
      if( classify.row.labels$Row.Label[i] %in% variables ){
        classify.row.labels$Associated.Variable[i] = classify.row.labels$Row.Label[i]
        classify.row.labels$New.Label[i] = classify.row.labels$Row.Label[i] }
      else{ classify.row.labels$Associated.Variable[i] = classify.row.labels$Associated.Variable[(i-1)] }
    }
    rm(i)

    ## For variables with more than 1 row, update the labels
    for(i in 1:nrow(classify.row.labels)){
      if( is.na(classify.row.labels$New.Label[i]) ){

        ## Identify the (non-missing) levels of the variable
        if( is.factor(dataset[, classify.row.labels$Associated.Variable[i] ]) ){ temp.levels = levels(dataset[, classify.row.labels$Associated.Variable[i] ] )}
        else{
          temp.levels = unique( dataset[, classify.row.labels$Associated.Variable[i] ] )
          temp.levels = temp.levels[ !is.na(temp.levels) ]
          temp.levels = temp.levels[ order(temp.levels) ]
        }

        ## Identify which level the current row is
        level.number = ( i - min( which( classify.row.labels$Associated.Variable == classify.row.labels$Associated.Variable[i] ) ) )

        ## Fill in the appropriate level
        classify.row.labels$New.Label[i] = temp.levels[ level.number ]
      }
    }
    rm(i, temp.levels, level.number)

    ## Replace the labels
    tab$Row.names = classify.row.labels$New.Label
    rm(classify.row.labels)
  }




  #########################################
  ## Analyze Factor Variables as Ordinal ##
  #########################################

  ## Treat p-values as character
  tab$p = as.character( tab$p )

  ## Find the p-values and replace them
  if( length(factors_reported.categorical.analyzed.ordinal) > 0 ){

    for(i in 1:length(factors_reported.categorical.analyzed.ordinal)){

      ## Is the variable actually a factor?
      if( is.factor( dataset[,factors_reported.categorical.analyzed.ordinal[i] ] ) ){
        ## Calculate the p-value
        p.value = kruskal.test(x = as.numeric( dataset[,factors_reported.categorical.analyzed.ordinal[i] ] ), g = dataset[, strata_var_name] )$p.value
      }
      else{ p.value = kruskal.test(x = dataset[,factors_reported.categorical.analyzed.ordinal[i] ], g = dataset[, strata_var_name] )$p.value }

      ## Round the p-value
      p.value = round(p.value, digits_pvalue)

      ## Replace the p-value if it's too small
      if( p.value == 0 ){
        zeros = paste( rep("0", (digits_pvalue-1)), collapse = "" )
        p.value = paste0("<0.", zeros, "1") }

      ## Replace the p-value
      tab$p[ tab$Row.names == factors_reported.categorical.analyzed.ordinal[i] ] = p.value
    }
    rm(i, p.value)
  }




  ###########################
  ## Report Missing Values ##
  ###########################

  ## Are missing values requested?
  if( missing.data_add.to.table == TRUE | missing.data_include.as.list == TRUE | missing.data_include.as.separate.table == TRUE ){

    ## Are there actually any missing values?
    counts_missing.data = apply(X = matrix(variables), MARGIN = 1, FUN = function(x){ return( sum( is.na(dataset[,x]) ) ) } )


    if( sum(counts_missing.data > 0, na.rm = TRUE) > 0 ){

      # Summarize the overall missing data info
      missing = data.frame(Variable = variables, Missing_Overall = counts_missing.data, stringsAsFactors = FALSE)

      ## Only keep variables with missing data
      missing = missing[ missing$Missing_Overall > 0, ]
      rm(counts_missing.data)

      ## Include missing values for the strata if they exist
      if( !is.null(strata.variable) ){

        ## Identify the strata levels if they are requested
        strata.levels = unique( dataset[, strata_var_name] )
        strata.levels = strata.levels[ !is.na(strata.levels) ]
        strata.levels = strata.levels[ order(strata.levels) ]

        ## Create empty columns for the strata-specific missing data counts
        missing[ , paste0("Missing_Strata_", strata.levels) ] = NA

        ## Remove rows without missing observations
        missing = missing[ missing$Missing_Overall > 0, ]

        ## Fill in the strata-specific missing values
        for(i in 1:nrow(missing)){
          for(k in 1:length(strata.levels)){
            missing[i,(k+2)] = sum( is.na( dataset[ dataset[, strata_var_name] == strata.levels[k], missing$Variable[i]] ), na.rm = TRUE )
          }
        }
        rm(i,k)
      }

      ## Edit the row names
      row.names(missing) = NULL

      ## Create a missing data table
      temp = data.frame(Variable = variables, Label = variable.labels)
      temp = temp[ temp$Variable %in% missing$Variable, ]
      missing.data_table = merge(x = missing, y = temp, by = "Variable", all.x = TRUE, all.y = FALSE)
      missing.data_table = missing.data_table[ , c("Variable","Label", names(missing.data_table)[ !names(missing.data_table) %in% c("Variable","Label")] )]
      rm(temp)

      ## Add to the table
      if( missing.data_add.to.table == TRUE ){

        ## Format all of the columns as character
        for(i in 1:ncol(tab)){ tab[,i] = as.character(tab[,i]) }; rm(i)

        ## Add a missing row below the variable and fill it in
        for(i in 1:nrow(missing)){

          ## Identify the variable names in the rows names (these will change as missing data rows are added)
          table.row.labels = apply(X = matrix(tab$Row.names), MARGIN = 1, FUN = function(x){ unlist( str_split(string = x, pattern = fixed(" (")) )[1] } )

          ## Identify where to place the new missing data row
          identify.rows = data.frame(Sequence = 1:length(table.row.labels),
                                     Row.Label = table.row.labels,
                                     Is.It.A.Variable = table.row.labels %in% variables,
                                     stringsAsFactors = FALSE)

          ## Identify the rows lower than the current variable
          identify.rows$Is.It.Lower = (identify.rows$Sequence > which( table.row.labels == missing$Variable[i] ) )

          ## Identify the row
          if( sum(identify.rows$Is.It.A.Variable == TRUE & identify.rows$Is.It.Lower == TRUE) == 0 ){ row.to.use = (nrow(tab) + 1) }
          else{ row.to.use = min( identify.rows$Sequence[ identify.rows$Is.It.A.Variable == TRUE & identify.rows$Is.It.Lower == TRUE ] ) }

          ## Create an empty row
          missing.row = tab[1,]

          ## Fill in the overall information for the missing data row
          missing.row$Row.names = missing.data_row.label
          missing.row$Overall = paste0(missing.data_value.prefix, missing$Missing_Overall[i])

          ## Fill in the strata information if it exists
          if( ncol(missing) > 2 ){
            for(k in 1:length(strata.levels)){
              missing.row[1, (k+2)] = paste0(missing.data_value.prefix, missing[i, (k+2)])
            }
            rm(k)
          }

          ## Add the row to the table
          if( row.to.use > nrow(tab) ){ tab = rbind(tab, missing.row) }
          else{
            temp.tab = rbind(tab[1:(row.to.use-1),], missing.row)
            tab = rbind( temp.tab, tab[row.to.use:nrow(tab), ] )
            rm(temp.tab)
          }
        }
        rm(i, missing.row, row.to.use)
      }

      ## Create a text string listing the missing data
      if( missing.data_include.as.list == TRUE ){

        missing.data_list = "The following variables had missing observations: "

        for(i in 1:nrow(missing.data_table)){

          ## If there's more than 1 missing observation, then add an "s"
          if( missing.data_table$Missing_Overall[i] == 1 ){
            temp = paste0(missing.data_table$Label[i], " had N=", missing.data_table$Missing_Overall[i], " missing observation")
          }
          else{ temp = paste0(missing.data_table$Label[i], " had N=", missing.data_table$Missing_Overall[i], " missing observations") }

          ## Add a period to the end
          if( i == nrow(missing.data_table) ){ temp = paste0(temp,".") }
          else if( (i == (nrow(missing.data_table) - 1)) & nrow(missing) > 2  ){ temp = paste0(temp, ", and ") }
          else if( (i == (nrow(missing.data_table) - 1)) & nrow(missing) == 2  ){ temp = paste0(temp, " and ") }
          else{ temp = paste0(temp, ", ") }

          ## Add to the overall string
          missing.data_list = paste0(missing.data_list, temp)
        }
        rm(i, temp)
      }
    }
    else{ missing.data_list = "No variables in the above table contained missing observations."; missing.data_table = "No missing observations!" }
  }




  ##############################
  ## Edit the Variable Labels ##
  ##############################

  ## Treat the variable labels as character
  tab$Row.names = as.character( tab$Row.names )

  ## Edit the all patients row label
  tab$Row.names[ tab$Row.names == "n" ] = all.patients.row.name

  ## Check that the labels are valid
  if( length(variable.labels) == length(variables) ){

    ## Create a dataframe summarizing the variable labels
    labs = data.frame(Variable = variables, Labels = variable.labels, stringsAsFactors = FALSE)

    ## Bring over test info
    labs = merge(x = labs, y = test.info, by = "Variable", sort = FALSE)

    ## Bring over descriptors
    labs$Descriptor = apply(X = matrix(labs$Test), MARGIN = 1,
                            FUN = function(x){
                              if( x %in% c("Default Categorical","exact") ){ return(descriptor_categorical) }
                              else if( x == "Default Continuous" ){ return(descriptor_continuous_default) }
                              else if( x == "nonnorm" ){ return(descriptor_continuous_nonnormal) }
                              else{ return(NA) } } )

    ## Create the final labels
    if( descriptors_include == TRUE ){
      labs$Label_Final = apply(X = cbind(labs$Labels, labs$Descriptor), MARGIN = 1, FUN = function(x){ return( paste0(x[1], x[2]) ) } )
    }
    else{ labs$Label_Final = labs$Labels }

    ## Replace the labels in the table
    for(i in 1:nrow(labs)){
      tab$Row.names[ tab$Row.names == labs$Variable[i] ] = labs$Label_Final[i]
    }
    rm(i, labs)
  }
  else{ return("variable.labels is not the same length as the variables!") }

  ## Remove the row names
  row.names(tab) = NULL





  ########################################
  ## Remove the Fake Strata Information ##
  ########################################

  ## Only perform these steps if a strata variable wasn't provided
  if( is.null(strata.variable) ){

    ## Remove the strata columns and p-value column from the overall table
    tab <- tab[, c(1, 2) ]

    ## Update the test.info object
    test.info <- "No statistical tests were performed"

  }





  ########################
  ## Return the Results ##
  ########################

  ## Simplify things and just return all items
  return( list(Table = tab,
               Test.Info = test.info,
               Missing.Data_List = missing.data_list,
               Missing.Data_Table = missing.data_table) )

}




