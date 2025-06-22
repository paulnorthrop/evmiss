#' Methods for objects of class `"evmiss_sim_study"`
#'
#' Methods for objects of class `"evmiss_sim_study"` returned from
#'   [`sim_study`].
#' @param x,object An object inheriting from class `"evmiss_sim_study"`, a
#'   result of a call to the function [`sim_study`]. For
#'   `plot.evmiss_sim_study`, further arguments to be passed to
#'   [plot.default][graphics::plot.default] and [`abline`][graphics::abline],
#'   such as graphical parameters.
#' @param ... Further arguments. Only used in the following cases.
#'
#' * `plot.evmiss_sim_study`: to pass graphical parameters to the graphical functions
#'     [`plot`][graphics::plot], [`hist`][graphics::hist].
#' * `print.summary.evmiss_sim_study`: to pass arguments, such as `digits`, to
#'   [`print`][base::print].
#' * `sim_table`: to pass arguments to `print.summary.evmiss_sim_study`.
#'
#' @details
#'
#' **Plot method**. The S3 plot method provides plots to compare these 3
#' approaches based on the point estimates that they produce for return levels
#' and GEV parameters. Optionally, if `penultimate = TRUE`, comparisons can be
#' made with a penultimate GEV approximation to the true distribution of the
#' relevant block maxima. **Update, and explain more**.
#'
#' **Summary method**. A 3-column matrix containing the values of the
#'   statistics in `statistics` used to summarise the performance of the
#'   estimators. The columns relate to the parameters `mu`, `sigma` and `xi`
#'   respectively. Each group of 3 rows (4 rows if `adjust` is supplied to
#'   `sim_study`) relates to a different combination of summary statistic and
#'   estimator, indicated by the row names.
#'
#' @seealso [`sim_study`].
#' @name sim_study_methods
#' @section Examples: See [`sim_study`].
NULL
## NULL

# =============================== Plot method =============================== #

#' @rdname sim_study_methods
#' @param what A character scalar. What should be plotted? See `distn` for
#'   further details. The default is `"return"` for the `plot` method and
#'   `"all"` for the `summary` method.
#'
#'  * `"return"`: estimates of return levels using the approaches `"full"`,
#'    `"adjust"` and `"naive"`.
#'  * `"mu"` , `"sigma"`, `"xi"` or `"all"`. The name of the parameter whose
#'    estimates are plotted. Use `"all"` to select all 3 GEV parameters.
#'
#' @param distn A character scalar. The default is `"marginal"`.
#'
#'  * `"marginal"`: 3 histograms of point estimates.
#'      - If `what = "return"` then 3 histograms of the point estimates of the
#'        required return level are produced, one plot for each of the
#'        approaches `"full"`, `"adjust"` and `"naive"`.
#'      - If `what` is `"all"` then 3 histograms of the respective point
#'        estimates of the GEV parameters \eqn{\mu}, \eqn{\sigma} and \eqn{\xi}
#'        are produced, for the approach supplied in `approach`.
#'      - if `what` is `"mu"`, `"sigma"` or `"xi"` then 3 histograms of the
#'        point estimates of the chosen GEV parameter are produced, one for
#'        each of the approaches `"full"`, `"adjust"` and `"naive"`.
#'  * `"joint"`: 3 scatter plots of point estimates.
#'      - If `what = "return"` then 3 scatter plots are produced to compare the
#'        point estimates of the required return level for a pair of the
#'        approaches `"full"`, `"adjust"` or `"naive"`.
#'      - If `what` is `"all"` then 3 scatter plots are produced, one for each
#'        of the GEV parameters \eqn{\mu}, \eqn{\sigma} and \eqn{\xi}, to
#'        compare the estimates from approaches supplied in `approach`.
#'        are produced, for the approach supplied in `approach`.
#'      - if `what` is `"mu"`, `"sigma"` or `"xi"` then 3 scatter plots of the
#'        point estimates of the chosen GEV parameter are produced, each one
#'        comparing two of the approaches `"full"`, `"adjust"` and `"naive"`.
#'
#' @param approach A character scalar or vector. A proper subset of
#'   `c("full", "adjust", "naive", "discard")`. Only relevant if `what` is
#'   `"all"`. If `distn = "marginal"` then the default is `"full"`.
#'   If `distn = "joint"` then the default is `c("full", "adjust")`.
#' @param vertical logical scalar. If `vertical = TRUE` then the 3 plots are
#'   arranged vertically. Otherwise, they are arranged horizontally.
#' @param main A character vector. Titles for the histograms.
#' @param penultimate A logical scalar. If  `distn = "marginal"` should
#'   lines be added to plots to indicate the GEV parameter values, or the
#'   implied return levels, from a penultimate approximation based on the
#'   simulation distribution and the block length. See [`mev::smith.penult`].
#' @param line_col A vector giving the colours of lines added to plots when
#'   `distn = "marginal"`. `line_col[1]`: sample median of estimates;
#'   `line_col[2]:` sample mean of estimates; `line_col[3]`: true return level
#'   (if `what = "return"`); `line_col[4]`: value from an penultimate
#'   approximation (if `penultimate = TRUE`).
#' @param line_lty A numeric vector giving the line types of these vertical
#'   lines.
#' @param legend A numeric vector of length 3. In which of the 3 plots should a
#'   legend relating to the vertical lines be placed. The default is in the
#'   final plot, the `"discard"` plot.
#' @param mar A numeric vector. The number of lines of margin on the four sides
#'   of plots. See [`par`][graphics::par].
#' @param layout A logical scalar. Set `layout = TRUE` if
#'   [`layout`][graphics::layout] is used to create a multi-plot layout
#'   external to the call to `plot.evmiss_sim_study`.
#' @param no_ylab A logical scalar. IF `no_ylab = TRUE` then no axis labels are
#'   placed on the vertical axis.
#'
#' @return **Plot method**. No return value, only the plots are produced.
#'
#' @export
plot.evmiss_sim_study <- function(x, what = c("return", "mu", "sigma", "xi",
                                              "all"),
                                   distn = c("marginal", "joint"),
                                   approach = c("full", "adjust", "naive",
                                                "discard"),
                                   return_period = 100, vertical = TRUE,
                                   main = c("full", "adjust", "naive",
                                            "discard"),
                                   penultimate = TRUE,
                                   line_col = c("orange", "purple", "red",
                                                "blue"),
                                   line_lty = c(1, 1, 2, 3), legend = 4,
                                   mar = c(5, 5, 2, 2),
                                   layout = FALSE, no_ylab = FALSE, ...) {
  # If penultimate = TRUE then check that package mev is available.
  # If it is then calculate the penultimate approximation estimates of the GEV
  # parameters mu, sigma and xi.
  # Also calculate the return level implied under the penultimate approximation
  # for the return periods in return_period.
  if (penultimate) {
    if (!requireNamespace("mev", quietly = TRUE)) {
      stop("Package 'mev' is needed. Please install it.", call.= FALSE)
    }
    penult_args <- c(list(family = x$distn, method = "bm",
                          m = x$block_length, returnList = FALSE),
                     x$distn_args)
    penult_lines <- do.call(mev::smith.penult, penult_args)
    penult_legend <- c("penultimate")
    penult_rl <- nieve::qGEV(p = 1 / return_period, loc = penult_lines[1],
                             scale = penult_lines[2], shape = penult_lines[3],
                             lower.tail = FALSE)
  } else {
    penult_lines <- NULL
  }
  # Check that what, distn and approach are appropriate
  what <- match.arg(what)
  distn <- match.arg(distn)
  # If what = "all" and distn = "marginal" then approach must be scalar
  if (what == "all" && distn == "marginal") {
    approach <- match.arg(approach)
  }
  # If what = "all" and distn = "joint" then approach must have length 2
  if (what == "all" && distn == "joint") {
    if (missing(approach)) {
      approach <- c("full", "adjust")
    }
    condition1 <- length(approach) != 2
    condition2 <- !all(is.element(approach, c("full", "adjust", "naive")))
    approach_names <- c("full", "adjust", "naive")
    if (condition1 || condition2) {
      stop("''approach'' must be a length-2 subset of ",
           "c(\"full\", \"adjust\", \"naive\")")
    }
  }
  # Fiddle margins for Figure 3
  if (length(main) == 1 && main == "") {
    graphics::par(mar = c(4.25, 1.25, 0, 0.25))
  }
  # If the user is not using graphics::layout() then use graphics::par()
  # to set the layout of the plots
  if (!layout) {
  # Reset graphical parameters on exit
    old_par <- graphics::par(no.readonly = TRUE)
    on.exit(graphics::par(old_par))

  # Need to adjust this: 4 plots (2 by 2), 6 plots (3 by 2 or 2 by 3)
  # Set the layout for the plots
    if (vertical) {
      graphics::par(mfrow = c(4, 1), mar = mar)
    } else {
      graphics::par(mfrow = c(2, 2), mar = mar)
    }
  }
  # Plotting function for histograms and, perhaps, vertical lines
  hist_fun <- function(x, vlines, line_col, line_lty, ..., freq = FALSE,
                       lwd = 3, cex.lab = 1.5, cex.axis = 1.5, cex.main = 1.5,
                       cex = 1.5, legend, main = "") {
    graphics::hist(x, ..., freq = freq, cex.lab = cex.lab, cex.axis = cex.axis,
                   cex.main = cex.main, main = main)
    if (!missing(vlines)) {
      graphics::abline(v = vlines, col = line_col, lwd = lwd, lty = line_lty)
    }
    if (!is.null(attr(x, "legend")) && attr(x, "legend")) {
      graphics::legend("topright", legend = legend, col = line_col,
                       lty = line_lty, lwd = lwd, cex = cex,
                       box.col = "transparent", bg ="transparent")
    }
  }
  # Plotting function for scatter plots and lines of equality
  scatter_fun <- function(x, y, ..., asp = 1, lwd = 2, cex.lab = 1.5,
                          cex.axis = 1.5, cex.main = 1.5, cex = 1.5,
                          col = "blue", lty = 1) {
    graphics::plot(x, y, ..., asp = asp, cex.lab = cex.lab,
                   cex.axis = cex.axis, cex.main = cex.main)
    graphics::abline(0, 1, lwd = lwd, col = col, lty = lty)
  }

  # Plots of estimated return levels
  if (what == "return") {
    # Check the value of return_period
    if (return_period <= 1) {
      stop("''return_period'' must be greater than 1.")
    }
    # Extract the respective MLEs of the parameters
    mle1 <- x$parameters[1:3, ]
    mle2 <- x$parameters[4:6, ]
    mle3 <- x$parameters[7:9, ]
    mle4 <- x$parameters[10:12, ]
    # Calculate the return levels estimates for return period return_period
    rl_full <- nieve::qGEV(1 / return_period, loc = mle1[1, ],
                           scale = mle1[2, ], shape = mle1[3, ],
                           lower.tail = FALSE)
    rl_adjust <- nieve::qGEV(1 / return_period, loc = mle2[1, ],
                             scale = mle2[2, ], shape = mle2[3, ],
                             lower.tail = FALSE)
    rl_naive <- nieve::qGEV(1 / return_period, loc = mle3[1, ],
                            scale = mle3[2, ], shape = mle3[3, ],
                            lower.tail = FALSE)
    rl_discard <- nieve::qGEV(1 / return_period, loc = mle4[1, ],
                              scale = mle4[2, ], shape = mle4[3, ],
                              lower.tail = FALSE)
    # Add attributes to control whether a legend is placed in the plot
    attr(rl_full, "legend") <- ifelse(is.element(1, legend), TRUE, FALSE)
    attr(rl_adjust, "legend") <- ifelse(is.element(2, legend), TRUE, FALSE)
    attr(rl_naive, "legend") <- ifelse(is.element(3, legend), TRUE, FALSE)
    attr(rl_discard, "legend") <- ifelse(is.element(4, legend), TRUE, FALSE)
    # Calculate the true return level
    # Set up the quantile function for the simulation distribution
    quantile_fn <- paste0("q", x$distn)
    quantile_level <- (1 - 1 / return_period) ^ (1 / x$block_length)
    quantile_args <- c(list(p = quantile_level, lower.tail = TRUE),
                       x$distn_args)
    true_return_level <- do.call(quantile_fn, quantile_args)
    # distn = "marginal": plots of marginal distributions of return levels
    # distn = "joint": scatter plots of comparing the 3 approaches
    if (distn == "marginal") {
      # Call hist() to enable equality of the vertical axis scales
      y1 <- suppressWarnings(graphics::hist(rl_full, plot = FALSE, ...))
      y2 <- suppressWarnings(graphics::hist(rl_adjust, plot = FALSE, ...))
      y3 <- suppressWarnings(graphics::hist(rl_naive, plot = FALSE, ...))
      y4 <- suppressWarnings(graphics::hist(rl_discard, plot = FALSE, ...))
      my_ylim <- c(0, max(y1$density, y2$density, y3$density, y4$density))
      my_xlim <- range(y1$breaks, y2$breaks, y3$breaks, y4$breaks)
      my_xlab <- paste(return_period, "block return level")
      my_legend <- c("median", "mean", "truth")
      if (length(main) == 1 && main == "" && !no_ylab) {
        names_on_y_axis <- TRUE
      } else {
        names_on_y_axis <- FALSE
      }
      main <- rep_len(main, 4)
      rl_call_hist_fn <- function(estimates, which, ..., xlab = my_xlab,
                                  xlim = my_xlim, ylim = my_ylim,
                                  legend = my_legend) {
        vlines <- c(stats::median(estimates, na.rm = TRUE),
                    mean(estimates, na.rm = TRUE), true_return_level)
        if (penultimate) {
          legend <- c(legend, penult_legend)
          vlines <- c(vlines, penult_rl)
        }
        if (names_on_y_axis) {
          hist_fun(estimates, vlines = vlines, line_col = line_col,
                   line_lty = line_lty, ..., ylab = approach[which],
                   xlab = xlab, xlim = xlim, ylim = ylim,
                   legend = legend)
        } else {
          hist_fun(estimates, vlines = vlines, line_col = line_col,
                   line_lty = line_lty, ..., main = main[which],
                   xlab = xlab, xlim = xlim, ylim = ylim,
                   legend = legend)
        }
        return(invisible())
      }
      rl_call_hist_fn(rl_full, which = 1, ...)
      rl_call_hist_fn(rl_adjust, which = 2, ...)
      rl_call_hist_fn(rl_naive, which = 3, ...)
      rl_call_hist_fn(rl_discard, which = 4, ...)
    } else {
      my_ylim <- range(rl_full, rl_adjust, rl_naive, rl_discard)
      my_xlim <- my_ylim
      # To ensure that the plots are square
      graphics::par(pty = "s")
      scatter_fun(rl_full, rl_adjust, ..., xlab = "full data",
                  ylab = "adjust for missings", xlim = my_xlim, ylim = my_ylim)
      scatter_fun(rl_full, rl_naive, ..., xlab = "full data",
                  ylab = "no adjustment", xlim = my_xlim, ylim = my_ylim)
      scatter_fun(rl_adjust, rl_naive, ..., xlab = "adjust for missings",
                  ylab = "no adjustment", xlim = my_xlim, ylim = my_ylim)
    }
  } else {
    # distn = "marginal": plots of marginal distributions of parameters
    #   if what = "all": 1 approach for mu, sigma and xi
    #
    # distn = "joint": scatter plots of comparing the 3 approaches
    if (distn == "marginal") {
      parameter_call_hist_fn <- function(estimates, ...) {
        hist_fun(estimates, line_col = line_col[-3], line_lty = line_lty[-3], ...)
        return(invisible())
      }
      if (what == "all") {
        # Extract all GEV parameter estimates for the selected approach
        gev_names <- c("mu", "sigma", "xi")
        row_names <- paste(gev_names, approach, sep = "_")
        pars <- x$parameters[row_names, ]
        mu_pars <- pars[1, ]
        mu_lines <- c(stats::median(mu_pars, na.rm = TRUE),
                      mean(mu_pars, na.rm = TRUE))
        sigma_pars <- pars[2, ]
        sigma_lines <- c(stats::median(sigma_pars, na.rm = TRUE),
                        mean(sigma_pars, na.rm = TRUE))
        xi_pars <- pars[3, ]
        xi_lines <- c(stats::median(xi_pars, na.rm = TRUE),
                      mean(xi_pars, na.rm = TRUE))
        if (penultimate) {
          mu_lines <- c(mu_lines, penult_lines[1])
          sigma_lines <- c(sigma_lines, penult_lines[2])
          xi_lines <- c(xi_lines, penult_lines[3])
        }
        attr(mu_pars, "legend") <- ifelse(is.element(1, legend), TRUE, FALSE)
        attr(sigma_pars, "legend") <- ifelse(is.element(2, legend), TRUE, FALSE)
        attr(xi_pars, "legend") <- ifelse(is.element(3, legend), TRUE, FALSE)
        all_legend <- c("median", "mean")
        if (penultimate) {
          all_legend <- c(all_legend, penult_legend)
        }
        parameter_call_hist_fn(mu_pars, vlines = mu_lines, xlab = gev_names[1],
                               xlim = range(mu_pars, mu_lines),
                               legend = all_legend, ...)
        parameter_call_hist_fn(sigma_pars,sigma_lines, xlab = gev_names[2],
                               xlim = range(sigma_pars, sigma_lines),
                               legend = all_legend, ...)
        parameter_call_hist_fn(xi_pars, xi_lines, xlab = gev_names[3],
                               xlim = range(xi_pars, xi_lines),
                               legend = all_legend, ...)
      } else {
        # Extract estimates for the selected parameter
        row_names <- paste(what, c("full", "adjust", "naive"), sep = "_")
        pars <- x$parameters[row_names, ]
        # Call hist() to enable equality of the vertical axis scales
        full_pars <- pars[1, ]
        full_lines <- c(stats::median(full_pars, na.rm = TRUE),
                        mean(full_pars, na.rm = TRUE))
        adj_pars <- pars[2, ]
        adj_lines <- c(stats::median(adj_pars, na.rm = TRUE),
                       mean(adj_pars, na.rm = TRUE))
        naive_pars <- pars[3, ]
        naive_lines <- c(stats::median(naive_pars, na.rm = TRUE),
                         mean(naive_pars, na.rm = TRUE))
        pen_val <- which(is.element(c("mu", "sigma", "xi"), what))
        if (penultimate) {
          full_lines <- c(full_lines, penult_lines[pen_val])
          adj_lines <- c(adj_lines, penult_lines[pen_val])
          naive_lines <- c(naive_lines, penult_lines[pen_val])
        }
        y1 <- graphics::hist(full_pars, plot = FALSE)
        y2 <- graphics::hist(adj_pars, plot = FALSE)
        y3 <- graphics::hist(naive_pars, plot = FALSE)
        my_ylim <- c(0, max(y1$density, y2$density, y3$density))
        my_xlim <- range(y1$breaks, y2$breaks, y3$breaks)
        attr(full_pars, "legend") <- ifelse(is.element(1, legend), TRUE, FALSE)
        attr(adj_pars, "legend") <- ifelse(is.element(2, legend), TRUE, FALSE)
        attr(naive_pars, "legend") <- ifelse(is.element(3, legend), TRUE, FALSE)
        par_legend <- c("median", "mean")
        if (penultimate) {
          par_legend <- c(par_legend, penult_legend)
        }
        parameter_call_hist_fn(full_pars, vlines = full_lines, xlab = what,
                               ..., main = main[1], xlim = my_xlim,
                               ylim = my_ylim, legend = par_legend)
        parameter_call_hist_fn(adj_pars, vlines = adj_lines, xlab = what,
                               ..., main = main[2], xlim = my_xlim,
                               ylim = my_ylim, legend = par_legend)
        parameter_call_hist_fn(naive_pars, naive_lines, xlab = what,
                               ..., main = main[3], xlim = my_xlim,
                               ylim = my_ylim, legend = par_legend)
      }
    } else {
      if (what == "all") {
        # Extract all GEV parameter estimates for the selected approaches
        gev_names <- c("mu", "sigma", "xi")
        row_names_1 <- paste(gev_names, approach[1], sep = "_")
        row_names_2 <- paste(gev_names, approach[2], sep = "_")
        row_names <- c(row_names_1, row_names_2)
        pars <- x$parameters[row_names, ]
        my_ylim <- range(pars)
        my_xlim <- my_ylim
        approaches <- c("full", "adjust", "naive")
        my_xlab <- main[which(is.element(approaches, approach[1]))]
        my_ylab <- main[which(is.element(approaches, approach[2]))]
        # To ensure that the plots are square
        graphics::par(pty = "s")
        scatter_fun(pars[1, ], pars[4, ], ..., xlab = my_xlab, ylab = my_ylab,
                    main = "mu")
        scatter_fun(pars[2, ], pars[5, ], ..., xlab = my_xlab, ylab = my_ylab,
                    main = "sigma")
        scatter_fun(pars[3, ], pars[6, ], ..., xlab = my_xlab, ylab = my_ylab,
                    main = "xi")
      } else {
        # Extract estimates for the selected parameter
        row_names <- paste(what, c("full", "adjust", "naive"), sep = "_")
        pars <- x$parameters[row_names, ]
        my_ylim <- range(pars)
        my_xlim <- my_ylim
        # To ensure that the plots are square
        graphics::par(pty = "s")
        scatter_fun(pars[1, ], pars[2, ], ..., xlab = "full data",
                    ylab = "adjust for missings", xlim = my_xlim, ylim = my_ylim)
        scatter_fun(pars[1, ], pars[3, ], ..., xlab = "full data",
                    ylab = "no adjustment", xlim = my_xlim, ylim = my_ylim)
        scatter_fun(pars[2, ], pars[3, ], ..., xlab = "adjust for missings",
                    ylab = "no adjustment", xlim = my_xlim, ylim = my_ylim)
      }
    }
  }
  return(invisible())
}

# ============================= Summary method ============================== #

#' summary method for objects of class `"sim_study"`

#' @rdname sim_study_methods
#' @param statistics A function used to calculate statistics to summarise the
#'   differences between estimates and the quantities described in `vsfull`
#'   below. The object returned from this function should be a named vector.
#'   See **Examples**. If `statistics` is missing then the default is to
#'   calculate the following sample statistics: mean, standard deviation, root
#'   mean squared error, median and inter-quartile range.
#' @param meanSE A logical scalar. If `meanSE = TRUE` and `vsfull = FALSE` then
#'   the sample mean of the estimated standard errors over all simulated
#'   datasets is added to the output summary statistics. Otherwise, this is not
#'   added because a comparison of the sample standard deviation of an
#'   estimator to `meanSE` is only valid for `vsfull = FALSE`.
#' @param log A logical scalar. If `log = TRUE` then make comparisons for the
#'   GEV scale parameter \eqn{\sigma} based on estimate of \eqn{\log \sigma.}
#'   Otherwise, based them on estimates of \eqn{\sigma}.
#' @param log_return A logical scalar. If `log_return = TRUE` then make
#'   comparisons for return levels on the log scale. Otherwise, use the
#'   original return level scale.
#' @param vsfull A logical scalar. If `vsfull = TRUE` then comparisons are made
#'   relative to the estimates from the `"full"` case, that is, fitting to data
#'   with no missing values. If `vsfull = FALSE` then comparisons are made
#'   relative to the true values (`what = "return"`) or penultimate
#'   approximations to the GEV parameter values (`what = "all"`).
#' @return **Summary method**. A list object, with class
#'   `c("summary.evmiss_sim_study", "list")`, containing the original function
#'   call to `summary.evmiss_sim_study` and the following components, depending
#'   on `what` and `vsfull`.
#'
#'   If `what = "all"`, matrices `full_gev`, `adjust_gev` and `naive_gev`.
#'   Columns 1 to 3 contain the values of the summary statistics relating to
#'   the estimates of \eqn{\mu}, \eqn{\sigma}, \eqn{\xi} respectively.
#'
#'   If `what = "return"`, matrices `full_rl`, `adjust_rl`, `naive_rl`.
#'   The columns contain the values of the summary statistics for return levels
#'   corresponding to the return periods in `return_period`.
#'
#'   The object is printed by [`print.summary.evmiss_sim_study`]. The rows
#'   in the matrices described above are interleaved so that values for the
#'   same summary statistic are printed in adjacent rows. If `vsfull = TRUE`
#'   then all the values in `full_gev` and `full_rl` are zero and therefore are
#'   not included when printing. If the elements of the vector returned from
#'   the function `statistics` are named, then these names are included in the
#'   row names. Otherwise, the names used are `stat1`, `stat2` etc.
#'
#' @export
summary.evmiss_sim_study <- function(object, ..., what = c("all", "return"),
                                     return_period, statistics,
                                     meanSE = FALSE, log = FALSE,
                                     log_return = FALSE, vsfull = TRUE) {

  # If return_period is not supplied then any available in object
  if (missing(return_period)) {
    return_period <- object$return_periods
  }
  res <- list()
  res$call <- match.call(expand.dots = TRUE)
  what <- match.arg(what)
  if (!vsfull && !requireNamespace("mev", quietly = TRUE)) {
    stop("Package 'mev' is needed. Please install it.", call.= FALSE)
  }
  # Calculate penultimate approximations to the GEV parameters
  penult_args <- c(list(family = object$distn, method = "bm",
                        m = object$block_length, returnList = FALSE),
                   object$distn_args)
  penult_parameters <- do.call(mev::smith.penult, penult_args)[-4]
  # Infer the penultimate approximation to the return levels of interest
  penult_rl <- nieve::qGEV(p = 1 / return_period, loc = penult_parameters[1],
                           scale = penult_parameters[2],
                           shape = penult_parameters[3], lower.tail = FALSE)
  # Calculate the true return level
  # Set up the quantile function for the simulation distribution
  quantile_fn <- paste0("q", object$distn)
  quantile_level <- (1 - 1 / return_period) ^ (1 / object$block_length)
  quantile_args <- c(list(p = quantile_level, lower.tail = TRUE),
                     object$distn_args)
  true_return_level <- do.call(quantile_fn, quantile_args)
  # Extract the respective MLEs of the parameters
  mle1 <- object$parameters[1:3, ]
  mle2 <- object$parameters[4:6, ]
  mle3 <- object$parameters[7:9, ]
  mle4 <- object$parameters[10:12, ]
  # Each of mle1, mle2, mle3 and mle4 is a 3 by nsim matrix with rows
  # containing the MLEs of mu, sigma and xi respectively
  # Summary statistic function
  if (missing(statistics)) {
    # Check that nsim > 1
    if (object$nsim == 1) {
      stop("''nsim'' must be greater than 1.")
    }
    # Set the default statistics
    statistics <- function(x) {
      c(bias = mean(x, na.rm = TRUE),
        sd = stats::sd(x, na.rm = TRUE),
        rmse = sqrt(mean(x ^ 2, na.rm = TRUE)),
        median_bias = stats::median(x, na.rm = TRUE),
        iqr = stats::IQR(x, na.rm = TRUE),
        mae = mean(abs(x), na.rm = TRUE),
        isNA = sum(is.na(x)))
    }
  }
  # If vsfull = TRUE compare adjust, naive and discard to full
  # If vsfull = FALSE compare full, adjust, naive and discard to penultimate
  if (vsfull) {
    compare1 <- mle1
    compare2 <- mle1
    compare3 <- mle1
    compare4 <- mle1
  } else {
    compare1 <- penult_parameters
    compare2 <- penult_parameters
    compare3 <- penult_parameters
    compare4 <- penult_parameters
  }
  # If log = TRUE then make comparisons for log(sigma) not sigma
  if (log) {
    log_compare1[2] <- log(compare1[2])
    log_compare2[2] <- log(compare2[2])
    log_compare3[2] <- log(compare3[2])
    log_compare4[2] <- log(compare4[2])
    log_mle1 <- mle1
    log_mle2 <- mle2
    log_mle3 <- mle3
    log_mle4 <- mle4
    log_mle1[2, ] <- log(log_mle1[2, ])
    log_mle2[2, ] <- log(log_mle2[2, ])
    log_mle3[2, ] <- log(log_mle3[2, ])
    log_mle4[2, ] <- log(log_mle4[2, ])
    full_gev <- apply(log_mle1 - log_compare1, 1, FUN = statistics)
    adjust_gev <- apply(log_mle2 - log_compare2, 1, FUN = statistics)
    naive_gev <- apply(log_mle3 - log_compare3, 1, FUN = statistics)
    discard_gev <- apply(log_mle4 - log_compare4, 1, FUN = statistics)
  } else {
    full_gev <- apply(mle1 - compare1, 1, FUN = statistics)
    adjust_gev <- apply(mle2 - compare2, 1, FUN = statistics)
    naive_gev <- apply(mle3 - compare3, 1, FUN = statistics)
    discard_gev <- apply(mle4 - compare4, 1, FUN = statistics)
    # If we are not comparing to the full MLE then, if required, extract the
    # respective SEs of the parameters
    if (meanSE && !vsfull) {
      se1 <- object$ses[1:3, ]
      se2 <- object$ses[4:6, ]
      se3 <- object$ses[7:9, ]
      se4 <- object$ses[10:12, ]
      full_gev <- rbind(full_gev, meanSE = apply(se1, 1, FUN = mean,
                                                 na.rm = TRUE))
      adjust_gev <- rbind(adjust_gev, meanSE = apply(se2, 1, FUN = mean,
                                                     na.rm = TRUE))
      naive_gev <- rbind(naive_gev, meanSE = apply(se3, 1, FUN = mean,
                                                   na.rm = TRUE))
      discard_gev <- rbind(discard_gev, meanSE = apply(se4, 1, FUN = mean,
                                                       na.rm = TRUE))
    }
  }
  # Calculate the return levels estimates for return period(s) return_period
  rl_function <- function(i, mles, log_return) {
    rl_values <- nieve::qGEV(1 / return_period[i], loc = mles[1, ],
                             scale = mles[2, ], shape = mles[3, ],
                             lower.tail = FALSE)
    if (vsfull) {
      compare_return_level <- nieve::qGEV(1 / return_period[i], loc = mle1[1, ],
                                          scale = mle1[2, ], shape = mle1[3, ],
                                          lower.tail = FALSE)
    } else {
      compare_return_level <- true_return_level[i]
    }
    if (log_return) {
      rl_values <- log(rl_values)
      compare_return_level <- log(compare_return_level)
    }
    rl_stats <- statistics(rl_values - compare_return_level)
    return(rl_stats)
  }
  full_rl <- as.matrix(sapply(1:length(return_period), FUN = rl_function,
                              mles = mle1, log_return = log_return))
  adjust_rl <- as.matrix(sapply(1:length(return_period), FUN = rl_function,
                                mles = mle2, log_return = log_return))
  naive_rl <- as.matrix(sapply(1:length(return_period), FUN = rl_function,
                               mles = mle3, log_return = log_return))
  discard_rl <- as.matrix(sapply(1:length(return_period), FUN = rl_function,
                                 mles = mle4, log_return = log_return))
  colnames(full_rl) <- paste0(return_period, "-year return level")
  colnames(adjust_rl) <- paste0(return_period, "-year return level")
  colnames(naive_rl) <- paste0(return_period, "-year return level")
  colnames(discard_rl) <- paste0(return_period, "-year return level")
  # If we are not comparing to the full MLE then, if required, extract the
  # respective SEs of the parameters
  if (meanSE && !vsfull) {
    rows_full <- grepl("full", rownames(object$rl_ses))
    rows_adjust <- grepl("adjust", rownames(object$rl_ses))
    rows_naive <- grepl("naive", rownames(object$rl_ses))
    rows_discard <- grepl("discard", rownames(object$rl_ses))
    rl_se1 <- object$rl_ses[rows_full, ]
    rl_se2 <- object$rl_ses[rows_adjust, ]
    rl_se3 <- object$rl_ses[rows_naive, ]
    rl_se4 <- object$rl_ses[rows_discard, ]
    # Find the rows in which the results for the requested return periods live
    find_rl <- function(x, y) {
      which(grepl(x, rownames(y)))
    }
    rl_names <- paste0(return_period, "-year")
    which_rows <- sapply(rl_names, FUN = find_rl, y = rl_se1)
    if (length(which_rows[[1]]) == 0) {
      stop("At least one requested return period is not available in object")
    }
    full_rl <- rbind(full_rl,
                     meanSE = apply(rl_se1[which_rows, , drop = FALSE], 1,
                                             FUN = mean, na.rm = TRUE))
    adjust_rl <- rbind(adjust_rl,
                       meanSE = apply(rl_se2[which_rows, , drop = FALSE], 1,
                                                 FUN = mean, na.rm = TRUE))
    naive_rl <- rbind(naive_rl,
                      meanSE = apply(rl_se3[which_rows, , drop = FALSE], 1,
                                               FUN = mean, na.rm = TRUE))
    discard_rl <- rbind(discard_rl,
                        meanSE = apply(rl_se4[which_rows, , drop = FALSE], 1,
                                                   FUN = mean, na.rm = TRUE))
  }
  # Return only the requested things
  if (what == "all") {
    res$full_gev <- full_gev
    res$adjust_gev <- adjust_gev
    res$naive_gev <- naive_gev
    res$discard_gev <- discard_gev
  } else {
    res$full_rl <- full_rl
    res$adjust_rl <- adjust_rl
    res$naive_rl <- naive_rl
    res$discard_rl <- discard_rl
  }
  res$vsfull <- vsfull
  class(res) <- c("summary.evmiss_sim_study", class(res))
  return(res)
}

# ============================ print.summary method ========================= #

#' Print method for objects of class `"summary.evmiss_sim_study"`
#'
#' @rdname sim_study_methods
#' @param return_period Either a numeric scalar (for `plot.evmiss_sim_study`)
#'   or a numeric vector (for `summary.evmiss_sim_study`). A return period or
#'   periods, in numbers of blocks, for which to compare the three estimation
#'   approaches. All values must be greater than 1.
#' @param quiet A logical scalar. If `quiet = TRUE` then do not print the
#'   summary to the console.
#' @export
print.summary.evmiss_sim_study <- function(x, quiet = FALSE, ...) {
  if (!quiet) {
    cat("\nCall:\n", paste(deparse(x$call), sep = "\n", collapse = "\n"),
        "\n\n", sep = "")
  }

  # If vsfull = TRUE then only print results for the adjust and naive cases
  # Otherwise, print results for full, adjust and naive

  # GEV parameter case
  if (any(grepl("gev", names(x)))) {
    # How many summary statistics are involved?
    nstats <- nrow(x$adjust_gev)
    if (x$vsfull) {
      mat <- rbind(x$adjust_gev, x$naive_gev, x$discard_gev)
      mat <- mat[order(c(seq_len(nrow(x$adjust_gev)),
                         seq_len(nrow(x$naive_gev)),
                         seq_len(nrow(x$discard_gev)))), ]
      if (is.null(rownames(mat))) {
        rownames(mat) <- rep(paste0("stat", 1:nstats), each = 3)
      }
      rownames(mat) <- paste(rownames(mat), c("adjust", "naive", "discard"),
                             sep = ",")
    } else {
      mat <- rbind(x$full_gev, x$adjust_gev, x$naive_gev, x$discard_gev)
      mat <- mat[order(c(seq_len(nrow(x$full_gev)),
                         seq_len(nrow(x$adjust_gev)),
                         seq_len(nrow(x$naive_gev)),
                         seq_len(nrow(x$discard_gev)))), ]
      if (is.null(rownames(mat))) {
        rownames(mat) <- rep(paste0("stat", 1:nstats), each = 4)
      }
      rownames(mat) <- paste(rownames(mat),
                             c("full", "adjust", "naive", "discard"),
                             sep = ",")
    }
    colnames(mat) <- c("mu", "sigma", "xi")
  } else {
    # How many summary statistics are involved?
    nstats <- nrow(x$adjust_rl)
    if (x$vsfull) {
      mat <- rbind(x$adjust_rl, x$naive_rl, x$discard_rl)
      mat <- mat[order(c(seq_len(nrow(x$adjust_rl)),
                         seq_len(nrow(x$naive_rl)),
                         seq_len(nrow(x$discard_rl)))), , drop = FALSE]
      if (is.null(rownames(mat))) {
        rownames(mat) <- rep(paste0("stat", 1:nstats), each = 3)
      }
      rownames(mat) <- paste(rownames(mat), c("adjust", "naive", "discard"),
                             sep = ",")
    } else {
      mat <- rbind(x$full_rl, x$adjust_rl, x$naive_rl, x$discard_rl)
      mat <- mat[order(c(seq_len(nrow(x$full_rl)),
                         seq_len(nrow(x$adjust_rl)),
                         seq_len(nrow(x$naive_rl)),
                         seq_len(nrow(x$discard_rl)))), , drop = FALSE]
      if (is.null(rownames(mat))) {
        rownames(mat) <- rep(paste0("stat", 1:nstats), each = 4)
      }
      rownames(mat) <- paste(rownames(mat),
                             c("full", "adjust", "naive", "discard"),
                             sep = ",")
    }
  }
  if (!quiet) {
    print(mat, ...)
  }
  attr(x, "matrix") <- mat
  return(invisible(x))
}

# ========================== Simulation results table ======================= #

#' Simulation results table
#'
#' @rdname sim_study_methods
#' @export
sim_table <- function(object, return_period, ...) {
  # If return_period is supplied then compare return levels
  # Otherwise compare GEV parameters
  if (missing(return_period)) {
    res <- print(summary(object, what = "all", ...), quiet = TRUE)
    res <- attr(res, "matrix")
    # Extract bias, sd and rmse
    bias_rows <- grepl("bias",  rownames(res)) &
      !grepl("median",  rownames(res))
    sd_rows <- grepl("sd",  rownames(res))
    rmse_rows <- grepl("rmse",  rownames(res))
    bias_res <- res[bias_rows, , drop = FALSE]
    sd_res <- res[sd_rows, , drop = FALSE]
    rmse_res <- res[rmse_rows, , drop = FALSE]
    gev_names <- c("mu", "sigma", "xi")
    colnames(bias_res) <- paste("bias", gev_names, sep = "_")
    colnames(sd_res) <- paste("sd", gev_names, sep = "_")
    colnames(rmse_res) <- paste("rmse", gev_names, sep = "_")
    val <- cbind(bias_res, sd_res, rmse_res)
  } else {
    res <- print(summary(object, what = "return",
                         return_period = return_period, ...), quiet = TRUE)
    res <- attr(res, "matrix")
    # Extract bias, sd and rmse
    bias_rows <- grepl("bias",  rownames(res)) &
      !grepl("median",  rownames(res))
    sd_rows <- grepl("sd",  rownames(res))
    rmse_rows <- grepl("rmse",  rownames(res))
    median_bias_rows <- grepl("median",  rownames(res))
    iqr_rows <- grepl("iqr",  rownames(res))
    mae_rows <- grepl("mae",  rownames(res))
    bias_res <- res[bias_rows, , drop = FALSE]
    sd_res <- res[sd_rows, , drop = FALSE]
    rmse_res <- res[rmse_rows, , drop = FALSE]
    median_bias_res <- res[median_bias_rows, , drop = FALSE]
    iqr_res <- res[iqr_rows, , drop = FALSE]
    mae_res <- res[mae_rows, , drop = FALSE]
    rl_names <- paste("z", return_period, sep = "_")
    colnames(bias_res) <- paste("bias", rl_names, sep = "_")
    colnames(sd_res) <- paste("sd", rl_names, sep = "_")
    colnames(rmse_res) <- paste("rmse", rl_names, sep = "_")
    colnames(median_bias_res) <- paste("median bias", rl_names, sep = "_")
    colnames(iqr_res) <- paste("iqr ", rl_names, sep = "_")
    colnames(mae_res) <- paste("mae", rl_names, sep = "_")
    val <- cbind(bias_res, median_bias_res, sd_res, iqr_res, rmse_res, mae_res)
  }
  rownames(val) <- substring(rownames(val), first = 6)
  val <- as.data.frame(val)
  return(val)
}
