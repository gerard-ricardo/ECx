
# ECx

<!-- badges: start -->
<!-- badges: end -->

Provides tools for calculating Effective Concentration (ECx) values from both generalized linear models (GLM) and generalized linear mixed models (GLMM) using packages 'lme4' and 'glmmTMB'. It supports multiple calculation methods, including 'from_top' and 'from_top_bot', to accommodate different types of dose-response data analysis.

## Installation

You can install the development version of ECx like so:

``` r
devtools::install_github("gerard-ricardo/ECx")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(ECx)
library(lme4)
set.seed(123)
raw_x <- sort(rep(seq(from = 0.1, to = 100, by = 10), 4))
suc <- 100 - rbinom(n = length(raw_x), size = 100, prob = raw_x / 100)
tot <- rep(100, length(raw_x))
data1 <- data.frame(raw_x, suc, tot)
data1$obs <- factor(formatC(1:nrow(data1), flag = "0", width = 3))
plot(data1$suc / data1$tot ~ data1$raw_x)
md3 <- glmer(cbind(suc, (tot - suc)) ~ raw_x + (1 | obs), family = binomial, data = data1)
ECx(model = md3, x = 50, type = "from_top")
```

