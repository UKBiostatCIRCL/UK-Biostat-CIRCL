#'
#' This function is designed to extract the key information from an LM object and compile it into a tidy dataframe
#' for use in R Markdown reports. Information includes coefficient estimates, standard errors,
#' Z statistics, and p-values.
#'
#' @param mod_glm This is an object of class 'lm'.
#'
#' @param dat This is the dataframe used in fitting the lm object.
#'
#' @keywords Tidy Output
#'
#' @import parameters
#'
#' @examples
#' mod_lm = lm(y ~ x, data = dat)
#' lm_output = create_lm_output_table(mod_lm, dat)
#'
#' @export



create_lm_output_table = function(mod_lm, dat){
  t = mod_lm$coefficients/parameters::standard_error(mod_lm)$SE
  p = (1 - pnorm(abs(t), 0, 1))*2 # We are using two-tailed z test

  lm_output = data.frame(mod_lm$coefficients, parameters::standard_error(mod_lm)$SE,
                         t, p)
  colnames(lm_output) = c("Coefficient", "Std. Errors", "t", "p-value")

  return(lm_output)
}
