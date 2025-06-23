# Check that the true return levels calculated in sim_study() are correct

set.seed(12345)
### Exponential data
m <- c(100, 1000)
res <- sim_study(nsim = 3, return_periods = m, discard = 25)

p_block_maximum <- stats::pexp(res$true_return_levels) ^ res$block_length
p_true <- 1 - 1 / m

test_that("sim_study(): check true return levels", {
  testthat::expect_equal(p_block_maximum, p_true)
})

# Check that a user-supplied statistics function gives the sample results
# as the relevant default calculations

res1 <- print(summary(res, what = "return", return_period = c(100, 1000)))
# Example of a user-defined statistics function
stat_fn <- function(x) {
 return(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
}
res2 <- print(summary(res, what = "return", statistics = stat_fn))

test_that("sim_study(): user-supplied statistics function", {
  testthat::expect_equal(attr(res1, "matrix")[1:6,  ],
                         attr(res2, "matrix"), ignore_attr = TRUE)
})
