#' Calculate ECx
#'
#' Description of what ECx calculation does, including any important details
#' users should know.
#'
#' @param model A model object, typically from `glm`, for which the ECx will
#'   be calculated.
#' @return The calculated ECx value.
#' @export
#' @examples
#' # Here, you could include a simple example of how to use the ECx function.



ECx <- function(model =  model, x = 50) {
  pred_probs <- predict(md1, type = "response")
  inhib <- max(pred_probs)-((x/100) * max(pred_probs))
  library(VGAM)
  eta <- logitlink(inhib)
  beta <- coef(model)[1:2]
  ECx <- (eta - beta[1]) / beta[2]
  pd <- -cbind(1, ECx) / beta[2]
  ff <- as.matrix(vcov(model)[1:2, 1:2])
  se <- sqrt(((pd %*% ff) * pd) %*% c(1, 1))
  upper <- (ECx + se * 1.96)
  lower <- (ECx - se * 1.96)
  sum <- data.frame(ECx, lower, upper)
  return(sum)
} # using coef
