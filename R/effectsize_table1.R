#### Function to create effect sizes in table1 ####

#' Adding Effect Sizes to table1
#'
#' This function allows you to add a column calculating effect sizes to the table1 function. The calculation is based on
#' eta-squared for continuous variables and Cramer's phi for categorical variables.
#'
#' @param x This input is not necessary if used as an added function in the extra.cols argument in table1.
#' The input will automatically be detected by the function.
#'
#' @keywords Effect Size
#'
#' @examples
#' data("sleep")
#'
#' table1::table1(~ extra | group, data = sleep, extra.cols = c(`Effect Size`= esize_table1))
#'
#' @export


esize_table1 <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, use eta-squared
    es <- effectsize::eta_squared(oneway.test(y ~ g, var.equal = FALSE))$Eta2
  } else {
    # For categorical variables, use Cramer's V/phi
    es <- rcompanion::cramerV(x = table(y, g))
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "<", format.pval(es, digits=3, eps=0.001)))
}
