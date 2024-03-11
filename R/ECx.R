ECx <- function(x, y) {
  library(VGAM)
  eta <- logit(y)
  beta <- coef(x)[1:2]
  ecx <- (eta - beta[1]) / beta[2]
  pd <- -cbind(1, ecx) / beta[2]
  ff <- as.matrix(vcov(x)[1:2, 1:2])
  se <- sqrt(((pd %*% ff) * pd) %*% c(1, 1))
  upper <- (ecx + se * 1.96)
  lower <- (ecx - se * 1.96)
  sum <- data.frame(ecx, lower, upper)
  return(sum)
} # using coef
