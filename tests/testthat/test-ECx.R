test_that("Description of the test", {
  # Preparation code (if needed)
  set.seed(123)
  raw_x <- sort(rep(seq(from = 0.1, to = 100, by = 10), 4))
  suc <- rbinom(n = length(raw_x), size = 100, prob = raw_x/100)
  tot <- rep(100, length(raw_x))
  data1 <- data.frame(raw_x, suc, tot)
  md1 <- glm(cbind(suc,(tot - suc)) ~ raw_x, family = binomial, data = data1)
  result <- round(as.numeric((ECx(model = md1, x = 50, type = 'absolute')[1])), 4)

  # Test expectations
  expect_equal(result, 50.3502)
  # You can use other `expect_` functions depending on what you are testing
})
