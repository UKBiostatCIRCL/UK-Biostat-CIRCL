#### Function to create tidy GLM output table ####


#' Creating Tidy Output Table from GLM Object
#'
#' This function is designed to extract the key information from a GLM object and compile it into a tidy dataframe
#' for use in R Markdown reports. Information includes coefficient estimates, standard errors, odds ratios,
#' Z statistics, and p-values.
#'
#' @param mod_glm This is an object of class 'glm'.
#'
#' @param dat This is the dataframe used in fitting the glm object.
#'
#' @keywords Tidy Output
#'
#' @import parameters
#'
#' @examples
#' x = rnorm(100, 0, 1)
#'
#' xb = 3 + 4.2 * x
#'
#' p = 1/(1 + exp(-xb))
#'
#' y = rbinom(n = 100, size = 1, prob = p)
#'
#' dat = data.frame(x, y)
#'
#' mod_glm = glm(y ~ x, data = dat, family = "binomial")
#'
#' glm_output = create_glm_output_table(mod_glm, dat)
#'
#' @export



create_glm_output_table = function(mod_glm, dat){
  z = mod_glm$coefficients/parameters::standard_error(mod_glm)$SE
  p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test

  glm_output = data.frame(mod_glm$coefficients, parameters::standard_error(mod_glm)$SE,
                          exp(mod_glm$coefficients), z, p)
  colnames(glm_output) = c("Coefficient","Std. Errors", "Odds Ratio", "Z","p-value")

  return(glm_output)
}
