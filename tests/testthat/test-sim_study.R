# Check that the true return levels calculated in sim_study() are correct

set.seed(12345)
### Exponential data
m <- c(100, 1000)
res <- sim_study(nsim = 2, return_periods = m, discard = 25)

p_block_maximum <- stats::pexp(res$true_return_levels) ^ res$block_length
p_true <- 1 - 1 / m

test_that("sim_study(): check true return levels", {
  testthat::expect_equal(p_block_maximum, p_true)
})
