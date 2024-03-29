

set.seed(123)
raw_x <- sort(rep(seq(from = 0.1, to = 100, by = 10), 4))
suc <- 100 - rbinom(n = length(raw_x), size = 100, prob = raw_x / 100)
tot <- rep(100, length(raw_x))
data1 <- data.frame(raw_x, suc, tot)
md1 <- glm(cbind(suc,(tot - suc)) ~ raw_x, family = binomial, data = data1)
new_data <- data.frame(raw_x = seq(min(data1$raw_x), max(data1$raw_x), length.out = 100))
predictions <- predict(md1, newdata = new_data, type = "response", se.fit = T)
my_pred <- data.frame(x = new_data$raw_x, pred = predictions$fit, lower = predictions$fit - 1.96 * predictions$se.fit,
                             upper = predictions$fit + 1.96 * predictions$se.fit)

ecx.interp <- function(ecx, x, pred, lower, upper) {
  inhibx <- max(pred) - (max(pred) * ecx)
  nearest_idx <- which.min(abs(pred - inhibx))
  df2 <- data.frame(
    ecx = x[nearest_idx],
    lower = x[which.min(abs(lower - inhibx))],
    upper = x[which.min(abs(upper - inhibx))]
  )
  return(df2)
} #this function interpolates ecx from the prediction date frame
ecx.interp(ecx = 0.1,  x = my_pred$x, pred = my_pred$pred, lower = my_pred$lower, upper = my_pred$upper)
