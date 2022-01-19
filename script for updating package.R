
## Method based on https://hilaryparker.com/2014/04/29/writing-an-r-package-from-scratch/




#### Procedure for adding new functions to package ####

# Add your newly-written R script to the path: UKBiostatCIRCL/R

# Be sure your script file includes the following information. A template is provided below, between the brackets.

{

#' A Cat Function
#'
#' This function allows you to express your love of cats.
#' @param love Do you love cats? Defaults to TRUE.
#' @keywords cats
#' @export
#' @examples
#' cat_function()

}


# Import the following libraries
library(roxygen2)
library(devtools)


# Run the following line of code. This re-compiles all functions within this folder into a single package.
# ONLY FILES WITH THE DOCUMENTATION BLOCK (ABOVE) ARE COMPILED.

document("./R")


## You've now added your function to the UKBiostatCIRCL package!!!!
