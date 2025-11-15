#' Simulation study
#'
#' Performs a simulation study to compare different approaches to fitting a
#' GEV distribution to block maxima when there are missing values in the
#' underlying raw data. Plot and summary methods enables comparisons of these
#' approaches to be made. See [`sim_study_methods`].
#'
#' @inheritParams confint.evmiss
#' @param nsim A numeric scalar. The number of simulated datasets.
#' @param return_periods A numeric vector of return periods for which estimates
#'    and confidence intervals for the corresponding return levels are
#'    required.
#' @param discard A numeric scalar. This is used as described in [`gev_mle`]
#'   to discard block maxima based on more than `discard` percentage of missing
#'   data.
#' @param level A numeric scalar in (0,1). The confidence level required for
#'   confidence intervals, to be passed to [`confint.evmiss`].
#' @param profile A logical scalar. If `TRUE` then confidence intervals based
#'   on a profile log-likelihood are included in the returned object. If
#'   `profile = TRUE` then the simulations will take far longer to run.
#'   The arguments `mult`, `faster` and `epsilon` are tuning parameters of the
#'   profiling algorithm.
#' @param quiet A logical scalar. If `quiet = FALSE` then the number of the
#'   current simulated dataset is printed to the console. Otherwise, nothing is
#'   printed.
#' @param timeout Argument `timeout` passed to [`R.utils::withTimeout`].
#'   Only relevant if `profile = TRUE` and `return_periods` is supplied.
#'   If a call to [`confint.return_level`] takes longer than `timeout` seconds
#'   then the calculation is terminated and `NA` values returned for the
#'   confidence limits.
#' @param ... Further arguments to be passed to [`sim_data`].
#'
#' @details The simulation study is based on raw data simulated from a
#'   user-supplied probability distribution. Missing values are created in
#'   these full data to produce a reduced raw dataset. Samples of block maxima
#'   are created from each of these two datasets. See [`sim_data`].
#'   Three approaches are taken to estimate the 3 parameters of GEV
#'   distribution used to model these block maxima. In each case, parameters
#'   are estimated using maximum likelihood estimation, as described below.
#'   See also [`gev_mle`].
#'
#'   * **Full**. Using the full data.
#'   * **Adjust**. Using the reduced dataset, with an adjustment for the
#'     numbers of non-missing values underlying each block maximum.
#'   * **Naive**. Using the reduced dataset, with no adjustment made for the
#'     presence of missing raw values.
#'   * **Discard**. Any block maximum for which greater than `discard` percent
#'     of the underlying raw values were missing is discarded and no further
#'     adjustment is made.
#'
#' The arguments `mult = 32, faster = TRUE` and `epsilon = -1` passed to
#' [`confint.evmiss`] are set with the aim of calculating quickly confidence
#' intervals based on a profile log-likelihood (if `profile = TRUE`). However,
#' for a large simulation study setting `faster = FALSE` may be more reliable
#' because it may prevent convergence issues for some simulated datasets.
#'
#' @return A list with class `c("sim_study", "list")`, with the
#'   following components.
#'
#'   * `parameters`: a `12` by `nsim` matrix. Column `i` gives results
#'     for simulation number `i`. The rows contain 3 sets of estimates of
#'     \eqn{\mu}, \eqn{\sigma} and \eqn{\xi}, as follows.
#'
#'       - rows 1-3 (`full`): using the full data, that is, with no
#'       missing values.
#'       - rows 4-6 (`adjust`): with adjustment for the number of non-missing
#'       values.
#'       - rows 7-9 (`naive`): no adjustment for the number of non-missing
#'       values.
#'       - rows 10-12 (`discard`): some block maxima may be discarded.
#'
#'   * `ses`, `lower_sym`, `upper_sym`, `lower_prof`, `upper_prof`: objects
#'     with the same structure as `parameters`, containing the corresponding
#'     estimates standard errors and lower and upper limits of 100`level`%
#'     confidence intervals. `lower_prof` and `upper_prof` are included only
#'     if `profile = TRUE`.
#'   * `rl_ses`, `rl_lower_sym`, `rl_upper_sym`, `rl_lower_prof`,
#'     `rl_upper_prof`: analogous to the objects immediately above but
#'     relating to the return levels of interest. `rl_lower_prof` and
#'     `rl_upper_prof` are included only if `profile = TRUE`.
#'   * `nsim`: the input value of `nsim`.
#'   * `distn`: the name of the distribution from which data are simulated,
#'     that is, the argument `distn` passed to [`sim_data`].
#'   * `distn_args`: user-supplied arguments for `distn`.
#'   * `block_length`: the block length used to create simulated block maxima.
#'   * `return_periods`: the input argument `return_periods`.
#'   * `true_return_levels`: a vector containing the true level for each
#'     component of `return_periods`.
#'
#' The row and column names of the matrices `parameters` and `return_levels`
#' help to describe what is contained in the matrix.
#'
#' The simulation settings, generated by a call to [`sim_data`] with
#' `blocks = 0` are provided, as an attribute named `"sim_data_args"`.
#'
#' @seealso [`sim_study_methods`].
#' @examples
#' # In practice, increasing nsim to 1000, say, is suggested
#' set.seed(12345)
#'
#' # Set arguments for all simulation studies
#' general_args <- list(nsim = 50, return_periods = 100)
#' block_args <- list(blocks = 50, block_length = 90)
#' miss_args <- list(missing_fn = mcar,
#'                   missing_args = list(p0miss = 0, min = 0, max = 0.2))
#' all_args <- c(general_args, block_args, miss_args)
#'
#' #### Exponential data
#' exp_args <- c(all_args, distn = "exp")
#' res <- do.call(sim_study_2, exp_args)
#'
#' ### Plots
#'
#' ## Return levels
#'
#' # Marginal 100-block return level estimates by approach
#' plot(res)
#'
#' # Compare 100-block return level estimates between approaches
#' plot(res, distn = "joint")
#'
#' ## GEV parameter estimates
#'
#' # (a) Each parameter marginally, by approach
#' plot(res, what = "mu")
#' plot(res, what = "sigma")
#' plot(res, what = "xi")
#'
#' # (b) Each parameter, comparing approaches
#' plot(res, what = "mu", distn = "joint")
#' plot(res, what = "sigma", distn = "joint")
#' plot(res, what = "xi", distn = "joint")
#'
#' # All parameters marginally, for each approach
#' plot(res, what = "all", approach = "full")
#' plot(res, what = "all", approach = "naive")
#' plot(res, what = "all", approach = "adjust")
#' plot(res, what = "all", approach = "discard")
#'
#' # All parameters, comparing pairs of different approaches
#' plot(res, what = "all", distn = "joint", approach = c("full", "adjust"),
#'      vertical = FALSE)
#' plot(res, what = "all", distn = "joint", approach = c("adjust", "naive"),
#'      vertical = FALSE)
#' plot(res, what = "all", distn = "joint", approach = c("adjust", "discard"),
#'      vertical = FALSE)
#'
#' ### Summaries
#'
#' ## GEV parameter estimates
#'
#' # Comparison to what would be obtained from the full dataset
#' summary(res)
#' # Comparison with a penultimate approximation
#' summary(res, vsfull = FALSE)
#'
#' ##  Return levels
#'
#' # Comparison to what would be obtained from the full dataset
#' summary(res, what = "return", return_period = c(100, 1000))
#'
#' # Comparison with a penultimate approximation
#' summary(res, what = "return", return_period = c(100, 1000), vsfull = FALSE)
#'
#' # Example of a user-defined statistics function
#' stat_fn <- function(x) {
#'   return(c(mean = mean(x, na.rm = TRUE), sd = sd(x, na.rm = TRUE)))
#' }
#' summary(res, what = "return", statistics = stat_fn)
#'
#' #### Normal data
#'
#' res <- sim_study(nsim = 50, distn = "norm")
#' plot(res)
#' summary(res, what = "return", return_period = c(100, 1000))
#' @export
sim_study_2 <- function(nsim, return_periods, discard = 10, level = 0.95,
                        profile = FALSE, mult = 32, faster = TRUE, epsilon = -1,
                        quiet = TRUE, timeout = 10, ...) {
  if (missing(return_periods)) {
    return_periods <- NULL
  }
  # Check discard
  if (!is.numeric(discard) || any(discard < 0)) {
    stop("''discard'' must be positive number")
  }
  # Extract arguments for simulate() from ...
  dots <- list(...)
  user_blocks <- dots$blocks
  # Call sim_data() with blocks = 0 to get the values of the arguments
  # provided to sim_data(), in particular distn, distn_args and
  # block_length, which are needed to calculate true return levels
  dots$blocks <- 0
  sim_data_args <- do.call(sim_data, dots)
  # Replace blocks = 0 in sim_data_args with the value of blocks used below
  sim_data_args$blocks <- ifelse(!is.null(user_blocks), user_blocks,
                                 formals(sim_data)$blocks)
  # A function to perform one replication
  replication_fun <- function(x, return_periods) {
    if (!quiet) {
      print(x)
    }
    sdata <- sim_data(...)
    # GEV fit to the full data
    fit1 <- gev_mle(sdata$data_full, block_length = sdata$block_length)
    mle1 <- coef(fit1)
    se1 <- sqrt(diag(vcov(fit1)))
    ci_sym_1 <- confint(fit1, level = level)
    lower_sym_1 <- ci_sym_1[, 1]
    upper_sym_1 <- ci_sym_1[, 2]
    if (profile) {
      if (!is.null(fit1$optim_error) || anyNA(c(mle1, se1))) {
        lower_prof_1 <- rep(NA, 3)
        upper_prof_1 <- rep(NA, 3)
      } else {
        ci_prof_1 <- confint(fit1, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_1 <- ci_prof_1[, 1]
        upper_prof_1 <- ci_prof_1[, 2]
      }
    }
    # GEV fit with adjustment for the number of non-missing values
    fit2 <- gev_mle(sdata$data_miss, block_length = sdata$block_length)
    mle2 <- coef(fit2)
    se2 <- sqrt(diag(vcov(fit2)))
    ci_sym_2 <- confint(fit2, level = level)
    lower_sym_2 <- ci_sym_2[, 1]
    upper_sym_2 <- ci_sym_2[, 2]
    if (profile) {
      if (!is.null(fit2$optim_error) || anyNA(c(mle2, se2))) {
        lower_prof_2 <- rep(NA, 3)
        upper_prof_2 <- rep(NA, 3)
      } else {
        ci_prof_2 <- confint(fit2, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_2 <- ci_prof_2[, 1]
        upper_prof_2 <- ci_prof_2[, 2]
      }
    }
    # GEV fit without adjustment for the number of non-missing values
    fit3 <- gev_mle(sdata$data_miss, block_length = sdata$block_length,
                    adjust = FALSE)
    mle3 <- coef(fit3)
    se3 <- sqrt(diag(vcov(fit3)))
    ci_sym_3 <- confint(fit3, level = level)
    lower_sym_3 <- ci_sym_3[, 1]
    upper_sym_3 <- ci_sym_3[, 2]
    if (profile) {
      if (!is.null(fit3$optim_error) || anyNA(c(mle3, se3))) {
        lower_prof_3 <- rep(NA, 3)
        upper_prof_3 <- rep(NA, 3)
      } else {
        ci_prof_3 <- confint(fit3, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_3 <- ci_prof_3[, 1]
        upper_prof_3 <- ci_prof_3[, 2]
      }
    }
    # Discard any block maxima based with greater than discard percent missing
    # values and fit a GEV without any further adjustment (adjust = FALSE)
    fit4 <- gev_mle(sdata$data_miss, block_length = sdata$block_length,
                    adjust = FALSE, discard = discard)
    mle4 <- coef(fit4)
    se4 <- sqrt(diag(vcov(fit4)))
    ci_sym_4 <- confint(fit4, level = level)
    lower_sym_4 <- ci_sym_4[, 1]
    upper_sym_4 <- ci_sym_4[, 2]
    if (profile) {
      if (!is.null(fit4$optim_error) || anyNA(c(mle4, se4))) {
        lower_prof_4 <- rep(NA, 3)
        upper_prof_4 <- rep(NA, 3)
      } else {
        ci_prof_4 <- confint(fit4, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_4 <- ci_prof_4[, 1]
        upper_prof_4 <- ci_prof_4[, 2]
      }
    }
    # GEV fit using weighting scheme 1
    fit5 <- gev_weighted(sdata$data_miss, block_length = sdata$block_length,
                         scheme = 1)
    mle5 <- coef(fit5)
    se5 <- sqrt(diag(vcov(fit5)))
    ci_sym_5 <- confint(fit5, level = level)
    lower_sym_5 <- ci_sym_5[, 1]
    upper_sym_5 <- ci_sym_5[, 2]
    if (profile) {
      if (!is.null(fit5$optim_error) || anyNA(c(mle5, se5))) {
        lower_prof_5 <- rep(NA, 3)
        upper_prof_5 <- rep(NA, 3)
      } else {
        ci_prof_5 <- confint(fit5, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_5 <- ci_prof_5[, 1]
        upper_prof_5 <- ci_prof_5[, 2]
      }
    }
    # GEV fit using weighting scheme 2
    fit6 <- gev_weighted(sdata$data_miss, block_length = sdata$block_length,
                         scheme = 2)
    mle6 <- coef(fit6)
    se6 <- sqrt(diag(vcov(fit6)))
    ci_sym_6 <- confint(fit6, level = level)
    lower_sym_6 <- ci_sym_6[, 1]
    upper_sym_6 <- ci_sym_6[, 2]
    if (profile) {
      if (!is.null(fit6$optim_error) || anyNA(c(mle6, se6))) {
        lower_prof_6 <- rep(NA, 3)
        upper_prof_6 <- rep(NA, 3)
      } else {
        ci_prof_6 <- confint(fit6, level = level, profile = TRUE,
                             mult = mult, faster = faster, epsilon = epsilon)
        lower_prof_6 <- ci_prof_6[, 1]
        upper_prof_6 <- ci_prof_6[, 2]
      }
    }
    # Return everything
    val <- c(mle1, mle2, mle3, mle4, mle5, mle6, se1, se2, se3, se4, se5, se6,
             lower_sym_1, lower_sym_2, lower_sym_3, lower_sym_4, lower_sym_5,
             lower_sym_6,
             upper_sym_1, upper_sym_2, upper_sym_3, upper_sym_4, upper_sym_5,
             upper_sym_6)
    if (profile) {
      val <- c(val,
               lower_prof_1, lower_prof_2, lower_prof_3, lower_prof_4,
               lower_sym_5, lower_sym_6,
               upper_prof_1, upper_prof_2, upper_prof_3, upper_prof_4,
               upper_sym_5, upper_sym_6)
    }
    # If return level inferences are required then perform them
    # GEV fit to the full data
    if (!is.null(return_periods)) {
      res1 <- rl_sim_study(object = fit1, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      res2 <- rl_sim_study(object = fit2, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      res3 <- rl_sim_study(object = fit3, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      res4 <- rl_sim_study(object = fit4, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      res5 <- rl_sim_study(object = fit5, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      res6 <- rl_sim_study(object = fit6, return_periods = return_periods,
                           level = level, profile = profile, mult = mult,
                           faster = faster, epsilon = epsilon,
                           timeout = timeout)
      rl_list <- c(res1, res2, res3, res4, res5, res6)
      # Pass the list res to a function to extract the following (in order)
      # mles, ses, confidence intervals
      # First for the first return level, then for the next etc.
      rl_val <- rl_reorder_results(rl_list, profile = profile,
                                   return_periods = return_periods)
      val <- c(val, rl_val)
    }
    return(val)
  }
  # Perform nsim replications
  repeat_simulations <- function(nsim, return_periods) {
    replicate(n = nsim, replication_fun(return_periods = return_periods))
  }
  results <- sapply(1:nsim, FUN = replication_fun,
                    return_periods = return_periods)
  # Calculate the true return levels
  if (!missing(return_periods)) {
    # Set up the quantile function for the simulation distribution
    quantile_fn <- paste0("q", sim_data_args$distn)
    # The ^ (1 / sim_data_args$block_length) part transforms from the block
    # maximum scale to the scale of the raw data
    quantile_level <- (1 - 1 / return_periods) ^
      (1 / sim_data_args$block_length)
    quantile_args <- c(list(p = quantile_level, lower.tail = TRUE),
                       sim_data_args$distn_args)
    true_return_levels <- do.call(quantile_fn, quantile_args)
  } else {
    true_return_levels <- NULL
  }

  # Separate the GEV results from the return level results
  if (profile) {
    gev_rows <- 1:108
  } else {
    gev_rows <- 1:72
  }
  rl_results <- results[-gev_rows, ]
  results <- results[gev_rows, ]
  # Set informative column and row names
  colnames(results) <- paste0("sim", 1:ncol(results))
  colnames(rl_results) <- paste0("sim", 1:ncol(results))
  par_name <- c("mu", "sigma", "xi")
  method_name <- c("full", "adjust", "naive", "discard", "weight1", "weight2")
  row_names <- paste0(par_name, "_", rep(method_name, each = 3))
  #
  # GEV parameters
  #
  # Separate the estimates of GEV parameters, their estimated standard errors
  # and lower and upper confidence limits
  rows1 <- 1:18
  rows2 <- 19:36
  rows3 <- 37:54
  rows4 <- 55:72
  parameters <- results[rows1, , drop = FALSE]
  ses <- results[rows2, , drop = FALSE]
  lower_sym <- results[rows3, , drop = FALSE]
  upper_sym <- results[rows4, , drop = FALSE]
  if (profile) {
    rows5 <- 71:90
    rows6 <- 91:108
    lower_prof <- results[rows5, , drop = FALSE]
    upper_prof <- results[rows6, , drop = FALSE]
  }
  rownames(parameters) <- row_names
  rownames(ses) <- row_names
  rownames(lower_sym) <- row_names
  rownames(upper_sym) <- row_names
  if (profile) {
    rownames(lower_prof) <- row_names
    rownames(upper_prof) <- row_names
  }
  val <- list(parameters = parameters, ses = ses, lower_sym = lower_sym,
              upper_sym = upper_sym)
  if (profile) {
    val <- c(val, list(lower_prof = lower_prof, upper_prof = upper_prof))
  }
  val <- c(val, list(nsim = nsim, distn = sim_data_args$distn,
                     distn_args = sim_data_args$distn_args,
                     block_length = sim_data_args$block_length))
  #
  # Return levels
  #
  # Separate the estimates of return levels, their estimated standard errors
  # and lower and upper confidence limits
  if (!is.null(return_periods)) {
    rl_parameters <- rl_results[grepl("mle_", rownames(rl_results)), ]
    rl_ses <- rl_results[grepl("se_", rownames(rl_results)), ]
    rl_lower_sym <- rl_results[grepl("lower_sym_", rownames(rl_results)), ]
    rl_upper_sym <- rl_results[grepl("upper_sym_", rownames(rl_results)), ]

    val <- c(val, list(rl_parameters = rl_parameters, rl_ses = rl_ses,
                       rl_lower_sym = rl_lower_sym,
                       rl_upper_sym = rl_upper_sym))
    if (profile) {
      rl_lower_prof <- rl_results[grepl("lower_prof_", rownames(rl_results)), ]
      rl_upper_prof <- rl_results[grepl("upper_prof_", rownames(rl_results)), ]
      val <- c(val, list(rl_lower_prof = rl_lower_prof,
                         rl_upper_prof = rl_upper_prof))
    }
  }
  if (!is.null(return_periods)) {
    val$return_periods <- return_periods
    val$true_return_levels <- true_return_levels
  }
  attr(val, "sim_data_args") <- sim_data_args
  class(val) <- c("evmiss_sim_study_2", class(val))
  return(val)
}
