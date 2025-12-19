#' Internal evmiss simulation study functions
#'
#' Internal evmiss simulation study functions
#' @details
#' These functions are not intended to be called by the user.
#' @name evmiss-internal-sim-study
#' @keywords internal
NULL

# ============================= Data manipulation =========================== #

#' @keywords internal
#' @rdname evmiss-internal-sim-study
rl_sim_study <- function(object, return_periods, level, profile, mult, faster,
                         epsilon, timeout) {
  return_object <- gev_return(object, m = return_periods, npy = 1)
  rl_mle <- coef(return_object)
  rl_se <- sqrt(diag(vcov(return_object)))
  rl_ci_sym <- confint(return_object, level = level)
  rl_lower_sym <- rl_ci_sym[, 1, drop = FALSE]
  rl_upper_sym <- rl_ci_sym[, 2, drop = FALSE]
  names(rl_lower_sym) <- rownames(rl_lower_sym)
  names(rl_upper_sym) <- rownames(rl_upper_sym)
  val <- list(mle = rl_mle, se = rl_se, lower_sym = t(rl_lower_sym),
              upper_sym = t(rl_upper_sym))
  if (profile) {
    if (!is.null(object$optim_error) || anyNA(rl_mle) || anyNA(rl_se)) {
      NAvec <- rep(NA, length(return_periods))
      rl_lower_prof <- matrix(NAvec)
      rl_upper_prof <- matrix(NAvec)
    } else {
      rl_ci_prof <- R.utils::withTimeout({
        confint(object = return_object, level = level, profile = profile,
                mult = mult, faster = faster, epsilon = epsilon)
      }, timeout = timeout, onTimeout = "silent")
      if (is.null(rl_ci_prof)) {
        NAvec <- rep(NA, length(return_periods))
        rl_lower_prof <- matrix(NAvec)
        rl_upper_prof <- matrix(NAvec)
      } else {
        rl_lower_prof <- rl_ci_prof[, 1, drop = FALSE]
        rl_upper_prof <- rl_ci_prof[, 2, drop = FALSE]
      }
    }
    names(rl_lower_prof) <- rownames(rl_lower_sym)
    names(rl_upper_prof) <- rownames(rl_upper_sym)
    val <- c(val, list(lower_prof = t(rl_lower_prof),
                       upper_prof = t(rl_upper_prof)))
  }
  return(val)
}

#' @keywords internal
#' @rdname evmiss-internal
rl_reorder_results <- function(rl_list, profile, return_periods) {
  n_rl <- length(return_periods)
  # rl_list is a list with n_rl (number of return levels) named vector
  #   mle, se, lower_sym, upper_sym.
  # If profile = TRUE then it also has components lower_prof and upper_prof.
  # Each component may have more than one element, one for each return level.
  # We want to return a vector containing, for each return level in turn:
  #   mles for each method present in rl_list and then the
  #   ses, lower and upper symmetric confidence limits and (perhaps) the
  #   lower and upper confidence limits based on profile log-likelihood.

  # A function to extract the information in the desired order
  extract_component <- function(i) {
    temp <- lapply(rl_list, function(x) x[i])
    val <- c(temp[names(temp) == "mle"],
             temp[names(temp) == "se"],
             temp[names(temp) == "lower_sym"],
             temp[names(temp) == "upper_sym"])
    if (profile) {
      val <- c(val,
               temp[names(temp) == "lower_prof"],
               temp[names(temp) == "upper_prof"])
    }
    return(val)
  }
  # rl is a 4m (or 6m if profile = TRUE) by n_rl matrix, where m is the
  # number of methods.
  # Each column gives values (mle, se etc) for a return level.
  rl <- sapply(1:n_rl, extract_component)
  # Concatenate to a vector.
  # Name the components after the return level
  names_rl <- paste0(rownames(rl), "_rl", rep(return_periods, each = nrow(rl)),
                     "_", c("full", "adjust", "naive", "discard"))
  rl <- c(rl)
  names(rl) <- names_rl
  # Unlist to create a vector: this is what is needed in sim_study()
  return(unlist(rl))
}

#' @keywords internal
#' @rdname evmiss-internal
rl_reorder_results_2 <- function(rl_list, profile, return_periods) {
  n_rl <- length(return_periods)
  # rl_list is a list with n_rl (number of return levels) named vector
  #   mle, se, lower_sym, upper_sym.
  # If profile = TRUE then it also has components lower_prof and upper_prof.
  # Each component may have more than one element, one for each return level.
  # We want to return a vector containing, for each return level in turn:
  #   mles for each method present in rl_list and then the
  #   ses, lower and upper symmetric confidence limits and (perhaps) the
  #   lower and upper confidence limits based on profile log-likelihood.

  # A function to extract the information in the desired order
  extract_component <- function(i) {
    temp <- lapply(rl_list, function(x) x[i])
    val <- c(temp[names(temp) == "mle"],
             temp[names(temp) == "se"],
             temp[names(temp) == "lower_sym"],
             temp[names(temp) == "upper_sym"])
    if (profile) {
      val <- c(val,
               temp[names(temp) == "lower_prof"],
               temp[names(temp) == "upper_prof"])
    }
    return(val)
  }
  # rl is a 4m (or 6m if profile = TRUE) by n_rl matrix, where m is the
  # number of methods.
  # Each column gives values (mle, se etc) for a return level.
  rl <- sapply(1:n_rl, extract_component)
  # Concatenate to a vector.
  # Name the components after the return level
  names_rl <- paste0(rownames(rl), "_rl", rep(return_periods, each = nrow(rl)),
                     "_", c("full", "adjust", "naive", "discard",
                            "weight1", "weight2"))
  rl <- c(rl)
  names(rl) <- names_rl
  # Unlist to create a vector: this is what is needed in sim_study()
  return(unlist(rl))
}

# ============================ Simulation functions ========================= #

# A function to simulate a sample of size n from a unit Frechet distribution
#' @keywords internal
#' @rdname evmiss-internal-sim-study
rfrechet <- function(n) {
  return(-1 / log(stats::runif(n)))
}

# A function to simulate a realisation of length n from a maxAR process with
# extremal index theta (This is similar to pages 94-95 of Coles (2001).)
# The function frechet2gp transforms the marginal distribution to
# GP(scale, shape)
#' @keywords internal
#' @rdname evmiss-internal-sim-study
rmaxAR <- function(n, theta, scale = 1, shape = 0){
  w <- rfrechet(n)
  val <- numeric(n)
  val[1] <- w[1]
  for (i in 2:n) {
    val[i] <- max((1 - theta) * val[i - 1], theta * w[i])
  }
  # Transform from Frechet to GP scale
  val <- revdbayes::qgp(exp(-1 / val), scale = scale, shape = shape)
  return(val)
}

# =========================== For S3 method functions ======================= #

#' @keywords internal
#' @rdname evmiss-internal
select_approaches <- function(x, the_approaches, approach) {
  # Which approaches should we include?
  if (length(approach) == 1) {
    stop("If distn = \"joint\" then approach must have length > 1.")
  }
  keep <- which(is.element(the_approaches, approach))
  return(x[, keep])
}

#' @keywords internal
#' @rdname evmiss-internal
pairs_lwd_hack <- function(x, ...) {
  # Hack to avoid lwd affecting the axis line width
  dots <- list(...)
  if (!is.null(dots$lwd)) {
    pass_lwd <- dots$lwd
    dots$lwd <- NULL
  } else {
    pass_lwd <- 1
  }
  return(c(list(x = x, pass_lwd = pass_lwd), dots))
}

#' @keywords internal
#' @rdname evmiss-internal
findLeadingZeros <- function(x) {
  y <- diff(c(0, x))
  z <- which(y == 0)
  leadingZeros <- which(z - 1:length(z) == 0)
  return(leadingZeros)
}

#' @keywords internal
#' @rdname evmiss-internal
findTrailingZeros <- function(x) {
  y <- diff(c(0, rev(x)))
  z <- which(y == 0)
  trailingZeros <- length(x) + 1 - which(z - 1:length(z) == 0)
  return(trailingZeros)
}

# ========================= Monte Carlo Standard Errors ===================== #

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEbias <- function(x, truth = 0) {
  x <- x[!is.na(x)]
  x <- x - truth
  n <- length(x)
  s <- stats::sd(x)
  mcse <- s / sqrt(n)
  return(mcse)
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEsd <- function(x, truth = 0) {
  x <- x[!is.na(x)]
  n <- length(x)
  s <- stats::sd(x)
  x_centred <- x - mean(x)
  k <- n * sum(x_centred ^ 4) / (sum(x_centred ^ 2) ^ 2)
  mcse <- s * sqrt(k - 1 + 2 / (n - 1)) / (2 * sqrt(n))
  return(mcse)
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSErmse <- function(x, truth = 0) {
  x <- x[!is.na(x)]
  x <- x - truth
  n <- length(x)
  m4 <- mean(x ^ 4)
  m2 <- mean(x ^ 2)
  sdMSE <- sqrt(n - 1) * stats::sd(x ^ 2) / n
  s <- stats::sd(x)
  eMSE <- s ^ 2 + mean(x) ^ 2
  mcse <- sdMSE / (2 * sqrt(eMSE))
  return(mcse)
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEstatistics <- function(x, truth = 0) {
  return(c(bias = MCSEbias(x, truth = truth), sd = MCSEsd(x, truth = truth),
           rmse = MCSErmse(x, truth = truth)))
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEstatistics_boot <- function(x, B = 100, truth = 0) {
  x <- x[!is.na(x)]
  x <- x - truth
  bias <- Monte.Carlo.se::boot.se(x = x, B = B, theta = mean)
  sd <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::sd)
  rmse <- function(x) {
    return(sqrt(mean(x ^ 2)))
  }
  rmse <- Monte.Carlo.se::boot.se(x = x, B = B, theta = rmse)
  return(c(bias = bias, sd = sd, rmse = rmse))
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEstatistics_boot2 <- function(x, B = 100, truth = 0) {
  x <- x[!is.na(x)]
  x <- x - truth
  bias <- Monte.Carlo.se::boot.se(x = x, B = B, theta = mean)
  median_bias <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::median)
  sd <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::sd)
  iqr <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::IQR)
  rmse_fn <- function(x) {
    return(sqrt(mean(x ^ 2)))
  }
  rmse <- Monte.Carlo.se::boot.se(x = x, B = B, theta = rmse_fn)
  mae_fn <- function(x) {
    return(mean(abs(x)))
  }
  mae <- Monte.Carlo.se::boot.se(x = x, B = B, theta = mae_fn)
  return(c(bias = bias, median_bias = median_bias, sd = sd, iqr = iqr,
           rmse = rmse, mae = mae))
}

#' @keywords internal
#' @rdname evmiss-internal-sim-study
MCSEstatistics_boot3 <- function(x, B = 100, truth = 0) {
  x <- x[!is.na(x)]
  x <- x - truth
  median_bias <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::median)
  iqr <- Monte.Carlo.se::boot.se(x = x, B = B, theta = stats::IQR)
  mae_fn <- function(x) {
    return(mean(abs(x)))
  }
  mae <- Monte.Carlo.se::boot.se(x = x, B = B, theta = mae_fn)
  return(c(median_bias = median_bias, iqr = iqr, mae = mae))
}
