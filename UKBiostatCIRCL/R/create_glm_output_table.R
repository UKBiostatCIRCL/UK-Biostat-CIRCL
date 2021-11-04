#### Function to create tidy GLM output table ####


#' Creating Tidy Output Table from GLM Object
#'
#' This function is designed to extract the key information from a GLM object and compile it into a tidy dataframe
#' for use in R Markdown reports. Information includes coefficient estimates, standard errors, odds ratios,
#' Z statistics, and p-values.
#' @param mod_glm This is an object of class 'glm'.
#' @param dat This is the dataframe used in fitting the glm object.
#' @keywords Tidy Output
#' @import parameters
#' @export
#' @examples
#' mod_glm = glm(as.factor(readmit_status) ~ `LesionSize (cm)` + Age + Sex + BMI, data = dat_single, family = "binomial")
#' glm_output = create_glm_output_table(mod_glm, dat_single)



create_glm_output_table = function(mod_glm, dat){
  require(parameters)
  z = mod_glm$coefficients/parameters::standard_error(mod_glm)$SE
  p = (1 - pnorm(abs(z), 0, 1))*2 # we are using two-tailed z test

  glm_output = data.frame(mod_glm$coefficients, parameters::standard_error(mod_glm)$SE,
                          exp(mod_glm$coefficients), z, p)
  colnames(glm_output) = c("Coefficient","Std. Errors", "Odds Ratio", "Z","p-value")

  return(glm_output)
}
