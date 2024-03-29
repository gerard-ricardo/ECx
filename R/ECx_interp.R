

# set.seed(123)
# raw_x <- sort(rep(seq(from = 0.1, to = 100, by = 10), 4))
# suc <- 100 - rbinom(n = length(raw_x), size = 100, prob = raw_x / 100)
# tot <- rep(100, length(raw_x))
# data1 <- data.frame(raw_x, suc, tot)
# md1 <- glm(cbind(suc,(tot - suc)) ~ raw_x, family = binomial, data = data1)
# new_data <- data.frame(raw_x = seq(min(data1$raw_x), max(data1$raw_x), length.out = 100))
# predictions <- predict(md1, newdata = new_data, type = "response", se.fit = T)
# my_pred <- data.frame(x = new_data$raw_x, pred = predictions$fit, lower = predictions$fit - 1.96 * predictions$se.fit,
#                              upper = predictions$fit + 1.96 * predictions$se.fit)
#

ecx_interp <- function(ecx, x, pred, lower, upper) {
  inhibx <- max(pred) - (max(pred) * ecx)
  if(inhibx > max(pred) | inhibx < min(pred)) {
    stop("The ECx value is outside the range of predictions.")
  }
  if(inhibx > max(lower) | inhibx < min(lower)) {
    warning("The ECx lower bound value is outside the range of predictions.")
  }
  if(inhibx > max(upper) | inhibx < min(upper)) {
    warning("The ECx upper bound value is outside the range of predictions.")
  }
  nearest_idx <- which.min(abs(pred - inhibx))
  nearest_idx_lower <- which.min(abs(lower - inhibx))
  nearest_idx_upper <- which.min(abs(upper - inhibx))
  # nearest_value_p <- pred[nearest_idx]
  # nearest_value_l <- lower[nearest_idx]
  # nearest_value_u <- upper[nearest_idx]
  # percent_diff <- abs(nearest_value - inhibx) / inhibx * 100
  # # Check if the difference is more than 1%
  # if(percent_diff > 1) {
  #   warning("The selected index's value is not within 1% difference of the inhibx value.")
  # }
  df2 <- data.frame(
    ecx = x[nearest_idx],
    lower = x[nearest_idx_lower],
    upper = x[nearest_idx_upper],
    inhibx = inhibx
  )
  if(inhibx > max(lower) | inhibx < min(lower)) {
    df2$lower = NA
  }
  if(inhibx > max(upper) | inhibx < min(upper)) {
    df2$upper = NA
  }
  return(df2)
}

#ecx_interp(ecx = 0.1,  x = my_pred$x, pred = my_pred$pred, lower = my_pred$lower, upper = my_pred$upper)
