#### Function to create degrees of freedom in table1 ####

#' Adding Degrees of Freedom to table1
#'
#' This function allows you to add a column calculating degrees of freedom to the table1 function. The calculation is based on
#' a one-way ANOVA for continuous variables and chi-squared for categorical variables.
#'
#' @param x This input is not necessary if used as an added function in the extra.cols argument in table1.
#' The input will automatically be detected by the function.
#'
#' @keywords Degrees of freedom
#'
#' @examples
#' data("sleep")
#'
#' table1::table1(~ extra | group, data = sleep, extra.cols = c(`DF`= df_table1))
#'
#' @export


df_table1 <- function(x, ...) {
  x <- x[-length(x)]  # Remove "overall" group
  # Construct vectors of data y, and groups (strata) g
  y <- unlist(x)
  g <- factor(rep(1:length(x), times=sapply(x, length)))
  if (is.numeric(y)) {
    # For numeric variables, perform an ANOVA
    p <- oneway.test(y ~ g, var.equal = FALSE)$parameter
  } else {
    # For categorical variables, perform a chi-squared test of independence
    p <- chisq.test(table(y, g))$parameter
  }
  # Format the p-value, using an HTML entity for the less-than sign.
  # The initial empty string places the output on the line below the variable label.
  c("", sub("<", "<", format.pval(p, digits=3, eps=0.001)))
}
