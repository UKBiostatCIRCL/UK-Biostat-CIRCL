#### Quantile Residuals ####


#' This function is used to compute the quantile residuals in a fitted beta regression model.
#'
#' When fitting beta regression models, the quantile residuals are argued to be the more reasonable and useful measure of
#' deviation from fitted and actual values in the model (Pereira, 2019). This stands in contrast to the currently
#' more typically used adjusted weighted standardized residual 1 or the weighted standardized residual 2.
#'
#' Currently, this function only works for models fitted using the `betareg` function in `R`.
#'
#' @param betareg_mod The fitted beta regression model using the `betareg` function.
#'
#' @param dat The dataframe used to fit the `betareg` model also used in this function. The dimensions of
#' the dataframe must match those of the dataframe used to fit the `betareg` object.
#'
#' @param outcome The response variable from the original dataframe used in the `betareg` object.
#' Specified as a character string.
#'
#' @references
#' Pereira, G. H. (2019). On quantile residuals in beta regression,
#' Communications in Statistics - Simulation and Computation 48(1), 302-316,
#' doi: 10.1080/03610918.2017.1381740.
#'
#' @keywords Beta regression, Quantile residuals
#'
#' @import betareg
#'
#' @examples
#'
#' require(betareg)
#' data("GasolineYield", package = "betareg")
#'
#' gy = betareg(yield ~ batch + temp | temp, data = GasolineYield)
#'
#' summary(gy)
#'
#' quantile_residuals(gy, GasolineYield, "yield")
#'
#' @export


quantile_residuals = function(betareg_mod, dat, outcome){

  require(betareg)

  predicted_mean = predict(betareg_mod, type = "response")

  predicted_precision = predict(betareg_mod, type = "precision")


  ## Calculating alpha and beta
  ## Using "method of moments" section from https://en.wikipedia.org/wiki/Beta_distribution


  # variance = mu(1-mu)/(1+phi)

  betareg_variance = (predicted_mean * (1 - predicted_mean)) /
    (1 + predicted_precision)

  alpha_parameter = (((predicted_mean * (1 - predicted_mean)) /
                        betareg_variance) - 1) * predicted_mean

  beta_parameter = (((predicted_mean * (1 - predicted_mean)) /
                       betareg_variance) - 1) * (1 - predicted_mean)

  betareg_quantile_residuals =
    qnorm(
      pbeta(as.numeric(dat[,outcome]),
            shape1 = alpha_parameter,
            shape2 = beta_parameter)
    )

  return(betareg_quantile_residuals)

}


