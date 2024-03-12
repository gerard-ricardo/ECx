#' Calculate ECx
#'
#' Calculates an ECx from a glm or glmm (lme4). This function allows users to calculate
#' ECx values using either the top of the response curve or between the top and bottom
#' of the response curve.
#'
#' @param model A model object, typically from `glm` or `glmer` from the `lme4` package.
#' @param x The percentage effect to calculate the ECx at (default is 50).
#' @param type The type of ECx calculation: 'from_top' (default), or 'from_top_bot'.
#' @return A data frame with ECx, lower, and upper confidence limits.
#' @export
#' @examples
#' # Ensure you have the 'hav4' dataset available before running this example
#' # md1 <- glm(cbind(suc,(tot - suc)) ~ raw_x, family = binomial, data = hav4)
#' # ECx(model = md1, x = 50)
ECx <- function(model, x = 50, type = c('from_top', 'from_top_bot', 'absolute')) {
  type <- match.arg(type)

  # Check if the model is a GLMM from lme4
  if("glmerMod" %in% class(model)) {
    beta <- fixef(model)  # For GLMMs, use fixef() to get fixed effects
  } else {
    beta <- coef(model)  # For GLMs, use coef()
  }

  pred_probs <- predict(model, type = "response")

  if (type == 'from_top') {
    inhib <- max(pred_probs) - ((x/100) * max(pred_probs))
  } else if (type == 'from_top_bot') {
    range_val <- max(pred_probs) - min(pred_probs)
    target_val <- min(pred_probs) + (range_val * (x/100))
    inhib <- target_val
  } else if (type == 'absolute') {
    inhib <- x/100
  }

  eta <- VGAM::logitlink(inhib)
  #beta <- coef(model)[1:2]
  ECx <- (eta - beta[1]) / beta[2]
  pd <- -cbind(1, ECx) / beta[2]
  ff <- as.matrix(vcov(model)[1:2, 1:2])
  se <- sqrt(((pd %*% ff) * pd) %*% c(1, 1))
  upper <- (ECx + se * 1.96)
  lower <- (ECx - se * 1.96)
  sum <- data.frame(ECx, lower, upper)
  return(sum)
}
