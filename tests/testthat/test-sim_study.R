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

# Return levels

res1 <- print(summary(res, what = "return", return_period = c(100, 1000)))
# Example of a user-defined statistics function
stat_fn <- function(x) {
 return(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
}
res2 <- print(summary(res, what = "return", statistics = stat_fn))

test_that("sim_study(): user-supplied statistics function (return levels)", {
  testthat::expect_equal(attr(res1, "matrix")[1:6,  ],
                         attr(res2, "matrix"), ignore_attr = TRUE)
})

# GEV estimates

res3 <- print(summary(res, return_period = c(100, 1000)))
# Example of a user-defined statistics function
stat_fn <- function(x) {
  return(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
}
res4 <- print(summary(res, statistics = stat_fn))

test_that("sim_study(): user-supplied statistics function (GEV parameters)", {
  testthat::expect_equal(attr(res3, "matrix")[1:6,  ],
                         attr(res4, "matrix"), ignore_attr = TRUE)
})

# Check that summary() and tab() produce consistent results

# Return levels

res1m <- attr(res1, "matrix")
res1bias <- res1m[1:3, ]
res1sd <- res1m[4:6, ]
res1rmse <- res1m[7:9, ]
res1med <- res1m[10:12, ]
res1qir <- res1m[13:15, ]
res1mae <- res1m[16:18, ]
res1all <- cbind(res1bias, res1med, res1sd, res1qir, res1rmse, res1mae)
tab1 <- as.matrix(tab(res, return_period = c(100, 1000)))
# Extract only
test_that("sim_study(): summary() vs tab()", {
  testthat::expect_equal(res1all,
                         as.matrix(tab(res, return_period = c(100, 1000))),
                         ignore_attr = TRUE)
})

# GEV estimates

res3m <- attr(res3, "matrix")
res3bias <- res3m[1:3, ]
res3sd <- res3m[4:6, ]
res3rmse <- res3m[7:9, ]
res3all <- cbind(res3bias, res3sd, res3rmse)
tab3 <- as.matrix(tab(res))

test_that("sim_study(): summary() vs tab()", {
  testthat::expect_equal(res3all, tab3, ignore_attr = TRUE)
})


