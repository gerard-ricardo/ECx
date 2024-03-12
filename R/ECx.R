#' Calculate ECx
#'
#' Calculates an ECx from a GLM or GLMM (lme4) or GLMM (glmmTMB). This function allows users to calculate
#' ECx values using either the top of the response curve or between the top and bottom
#' of the response curve.
#'
#' @param model A model object, typically from `glm` or `glmer` from the `lme4` package, or `glmmTMB`.
#' @param x The percentage effect to calculate the ECx at (default is 50).
#' @param type The type of ECx calculation: 'from_top' (default), 'from_top_bot', 'absolute', 'from_bot', or 'from_bot_top'. 'from_top' calculates the ECx from
#' the maximum of the model prediction to zero; 'from_top_bot' between the range of the maximum and minimum of the model prediction; 'absolute'
#' is used to directly calculate the ECx at an absolute value. Suitable for increasing trends, 'from_bot' calculate from the bottom; and 'from_bot_top' calculates from the bottom to the top.
#' Which is most appropriate depends on the specific research question
#' @return A data frame with ECx, lower, and upper confidence limits.
#' @importFrom lme4 fixef
#' @importFrom VGAM logitlink
#' @importFrom glmmTMB fixef
#' @importFrom stats vcov coef predict family
#' @export
#' @examples
#' set.seed(123)
#' raw_x <- sort(rep(seq(from = 0.1, to = 100, by = 10), 4))
#' suc <- 100 - rbinom(n = length(raw_x), size = 100, prob = raw_x / 100)
#' tot <- rep(100, length(raw_x))
#' data1 <- data.frame(raw_x, suc, tot)
#' md1 <- glm(cbind(suc,(tot - suc)) ~ raw_x, family = binomial, data = data1)
#' ECx(model = md1, x = 50, type = "from_top")
#'
ECx <- function(model, x = 50, type = c("from_top", "from_top_bot", "absolute", 'from_bot','from_bot_top')) {
  # Extract model family as a string



  # type of ECx
  type <- match.arg(type)
  pred_probs <- predict(model, type = "response")
  if (type == "from_top") {
    inhib <- max(pred_probs) - ((x / 100) * max(pred_probs))
  } else if (type == "from_top_bot") {
    range_val <- max(pred_probs) - min(pred_probs)
    inhib <- min(pred_probs) + (range_val * (x / 100))
  } else if (type == "absolute") {
    inhib <- x / 100
  }else if (type == "from_bot") {
    inhib <-  (1 - min(pred_probs) * (x / 100)) + min(pred_probs)
  }else if (type == "from_bot_top") {
    inhib <-  (max(pred_probs) - min(pred_probs) * (x / 100)) + min(pred_probs)
  }

  # Check model package and family
  model_family <- family(model)$family
  if ("glmerMod" %in% class(model) & model_family == "binomial") {
    beta <- fixef(model) # For lme4 GLMMs
    eta <- VGAM::logitlink(inhib)
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- as.matrix(vcov(model)[1:2, 1:2])
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  } else if ("glmerMod" %in% class(model) & model_family == "gaussian") {
    beta <- fixef(model) # For glmmTMB
    eta <- inhib
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- as.matrix(vcov(model)[1:2, 1:2])
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  } else if ("glmmTMB" %in% class(model) & model_family == "binomial") {
    beta <- fixef(model)$cond # For glmmTMB
    eta <- VGAM::logitlink(inhib)
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- vcov(model)$cond[1:2, 1:2]
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  } else if ("glmmTMB" %in% class(model) & model_family == "gaussian") {
    beta <- fixef(model)$cond # For glmmTMB
    eta <- inhib
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- vcov(model)$cond[1:2, 1:2]
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  } else if ("glm" %in% class(model) & model_family == "binomial") {
    beta <- coef(model) # For lme4 GLMMs
    eta <- VGAM::logitlink(inhib)
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- vcov(model)[1:2, 1:2]
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  } else if ("glm" %in% class(model) & model_family == "gaussian") {
    beta <- coef(model) # For lme4 GLMMs
    eta <- inhib
    ECx <- (eta - beta[1]) / beta[2]
    pd <- -cbind(1, ECx) / beta[2]
    vc <- vcov(model)[1:2, 1:2]
    se <- sqrt(((pd %*% vc) * pd) %*% c(1, 1))
    upper <- (ECx + se * 1.96)
    lower <- (ECx - se * 1.96)
    sum <- data.frame(ECx, lower, upper)
  }  else {
    sum <- "This package or family is not supported yet."
  }

   return(sum)
}
