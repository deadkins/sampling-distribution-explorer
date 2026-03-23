# Helper functions for the Sampling Distribution Explorer.
# These functions are organized so the simulation logic can be tested
# independently from the Shiny UI.

N_POP <- 200000

POPULATION_SPECS <- list(
  sleep = list(
    key = "sleep",
    label = "Hours of sleep per night",
    units = "hours",
    shape = "Approximately normal",
    rationale = "Many adult behavioral measures are roughly bell-shaped in a population.",
    type = "continuous",
    seed = 1001L,
    generator = function(n) {
      pmin(pmax(rnorm(n, mean = 7.0, sd = 1.2), 2), 12)
    }
  ),
  net_worth = list(
    key = "net_worth",
    label = "Household net worth",
    units = "$1,000s",
    shape = "Strongly right-skewed",
    rationale = "Most households have modest wealth, while a small number have very large wealth.",
    type = "continuous",
    seed = 2027L,
    generator = function(n) {
      rlnorm(n, meanlog = log(180), sdlog = 0.9)
    }
  ),
  voted = list(
    key = "voted",
    label = "Voted in the last national election",
    units = "0/1",
    shape = "Binary",
    rationale = "Many social outcomes are yes/no indicators.",
    type = "binary",
    seed = 3049L,
    generator = function(n) {
      rbinom(n, size = 1, prob = 0.62)
    }
  ),
  children = list(
    key = "children",
    label = "Number of children in household",
    units = "count",
    shape = "Discrete count with many zeros and right skew",
    rationale = "Count outcomes are common in social data and often pile up at zero.",
    type = "count",
    seed = 4093L,
    generator = function(n) {
      is_structural_zero <- rbinom(n, 1, 0.35)
      ifelse(is_structural_zero == 1, 0, rpois(n, lambda = 2.4))
    }
  ),
  work_hours = list(
    key = "work_hours",
    label = "Weekly paid work hours",
    units = "hours/week",
    shape = "Bimodal",
    rationale = "A population can be bimodal when it mixes part-time and full-time workers; this synthetic population excludes people outside the labor force.",
    type = "continuous",
    seed = 5129L,
    generator = function(n) {
      full_time <- rbinom(n, 1, 0.65)
      ifelse(
        full_time == 1,
        pmin(pmax(rnorm(n, 42, 4), 0), 80),
        pmin(pmax(rnorm(n, 20, 5), 0), 80)
      )
    }
  )
)

population_choices <- function() {
  stats::setNames(
    vapply(POPULATION_SPECS, function(spec) spec$key, character(1)),
    vapply(POPULATION_SPECS, function(spec) spec$label, character(1))
  )
}

get_population_metadata <- function(key) {
  spec <- POPULATION_SPECS[[key]]
  if (is.null(spec)) {
    stop(sprintf("Unknown population key: %s", key))
  }
  spec
}

generate_population <- function(key, n = N_POP) {
  spec <- get_population_metadata(key)
  set.seed(spec$seed)
  x <- spec$generator(n)
  if (spec$type %in% c("binary", "count")) {
    x <- as.integer(round(x))
  }
  x
}

compute_estimator <- function(x, estimator = c("Mean", "Median")) {
  estimator <- match.arg(estimator)
  if (estimator == "Mean") {
    mean(x)
  } else {
    stats::median(x)
  }
}

compute_true_parameter <- function(population, estimator = c("Mean", "Median")) {
  compute_estimator(population, estimator)
}

compute_true_se <- function(population, n) {
  stats::sd(population) / sqrt(as.integer(n))
}

simulate_repeated_samples <- function(population,
                                      n,
                                      reps,
                                      estimator = c("Mean", "Median"),
                                      seed = NULL,
                                      keep_last = 50L) {
  estimator <- match.arg(estimator)
  if (!is.null(seed)) {
    set.seed(seed)
  }

  reps <- as.integer(reps)
  n <- as.integer(n)
  keep_last <- max(1L, min(as.integer(keep_last), reps))

  estimates <- numeric(reps)
  last_samples <- vector("list", keep_last)
  keep_from <- reps - keep_last + 1L

  for (i in seq_len(reps)) {
    sample_x <- sample(population, size = n, replace = TRUE)
    estimates[i] <- compute_estimator(sample_x, estimator)
    if (i >= keep_from) {
      last_samples[[i - keep_from + 1L]] <- sample_x
    }
  }

  list(
    estimates = estimates,
    last_samples = last_samples,
    example_sample = last_samples[[keep_last]],
    n = n,
    reps = reps,
    estimator = estimator,
    keep_last = keep_last
  )
}

compute_mean_ci <- function(x) {
  n <- length(x)
  estimate <- mean(x)
  se <- stats::sd(x) / sqrt(n)
  crit <- stats::qt(0.975, df = n - 1)
  lower <- estimate - crit * se
  upper <- estimate + crit * se
  c(lower = lower, estimate = estimate, upper = upper)
}

compute_median_boot_ci <- function(x, B = 300L) {
  B <- as.integer(B)
  boot_stats <- replicate(B, stats::median(sample(x, length(x), replace = TRUE)))
  estimate <- stats::median(x)
  qs <- stats::quantile(boot_stats, probs = c(0.025, 0.975), names = FALSE, type = 7)
  c(lower = qs[1], estimate = estimate, upper = qs[2])
}

compute_last20_cis <- function(last_samples,
                               estimator = c("Mean", "Median"),
                               bootstrap_B = 300L) {
  estimator <- match.arg(estimator)
  draws <- seq_along(last_samples)
  out <- data.frame(
    draw = draws,
    lower = numeric(length(last_samples)),
    estimate = numeric(length(last_samples)),
    upper = numeric(length(last_samples)),
    stringsAsFactors = FALSE
  )

  for (i in draws) {
    ci <- if (estimator == "Mean") {
      compute_mean_ci(last_samples[[i]])
    } else {
      compute_median_boot_ci(last_samples[[i]], B = bootstrap_B)
    }
    out$lower[i] <- ci[["lower"]]
    out$estimate[i] <- ci[["estimate"]]
    out$upper[i] <- ci[["upper"]]
  }

  out$contains_true <- NA
  out
}

format_value <- function(x, key, digits = 2) {
  spec <- get_population_metadata(key)
  if (spec$key == "voted") {
    sprintf("%.3f", x)
  } else if (spec$key == "net_worth") {
    format(round(x, digits), nsmall = digits, big.mark = ",", trim = TRUE)
  } else {
    format(round(x, digits), nsmall = digits, trim = TRUE)
  }
}

sampling_distribution_caption <- function(show_normal_curve) {
  if (isTRUE(show_normal_curve)) {
    "Normal curve matched to the simulated estimator distribution."
  } else {
    ""
  }
}

get_sampling_x_limits <- function(population, key, estimator = c("Mean", "Median")) {
  estimator <- match.arg(estimator)
  true_value <- compute_true_parameter(population, estimator)
  pop_sd <- stats::sd(population)
  pop_iqr <- stats::IQR(population)
  spread <- max(pop_sd, pop_iqr / 1.349, 1e-6)

  if (key == "sleep") {
    return(if (estimator == "Mean") c(5.8, 8.2) else c(5.5, 8.5))
  }
  if (key == "net_worth") {
    return(if (estimator == "Mean") c(120, 330) else c(100, 260))
  }
  if (key == "voted") {
    return(c(0, 1))
  }
  if (key == "children") {
    return(if (estimator == "Mean") c(0.8, 2.5) else c(0, 4))
  }
  if (key == "work_hours") {
    return(if (estimator == "Mean") c(22, 38) else c(15, 45))
  }

  width <- 4 * spread
  c(true_value - width, true_value + width)
}

binary_median_note <- function(key, estimator) {
  if (identical(key, "voted") && identical(estimator, "Median")) {
    "For binary data, the median is the majority category and is usually less informative than the mean/proportion."
  } else {
    ""
  }
}

plot_population_distribution <- function(population, key) {
  spec <- get_population_metadata(key)

  if (spec$type == "continuous") {
    x <- population
    main <- "Population distribution"
    xlab <- sprintf("%s (%s)", spec$label, spec$units)
    caption <- ""
    if (key == "net_worth") {
      cutoff <- stats::quantile(x, probs = 0.99, names = FALSE)
      x <- x[x <= cutoff]
      caption <- "Display is zoomed to the 99th percentile for readability; the upper tail is omitted visually."
    }
    hist(
      x,
      breaks = "FD",
      col = "#c7d7ea",
      border = "white",
      main = main,
      cex.main = 0.98,
      xlab = xlab,
      ylab = "Count",
      cex.lab = 0.9,
      cex.axis = 0.86
    )
    if (nzchar(caption)) {
      mtext(caption, side = 1, line = 3.8, cex = 0.7, col = "#5b6470")
    }
  } else {
    tab <- if (spec$type == "binary") {
      table(factor(population, levels = c(0, 1)))
    } else {
      table(factor(population, levels = 0:max(population)))
    }
    bar_cols <- if (spec$type == "binary") c("#c7d7ea", "#7aa6d1") else "#9ec2a5"
    names_vec <- names(tab)
    if (spec$type == "binary") {
      names_vec <- c("No", "Yes")
    }
    barplot(
      as.numeric(tab),
      names.arg = names_vec,
      col = bar_cols,
      border = NA,
      main = "Population distribution",
      cex.main = 0.98,
      xlab = spec$label,
      ylab = "Count",
      cex.lab = 0.9,
      cex.axis = 0.86,
      cex.names = 0.86
    )
  }
}

plot_sample_distribution <- function(sample_x, key) {
  spec <- get_population_metadata(key)
  if (spec$type == "continuous") {
    hist(
      sample_x,
      breaks = "FD",
      col = "#e6cfb3",
      border = "white",
      main = "One sample from the most recent run",
      cex.main = 0.98,
      xlab = sprintf("%s (%s)", spec$label, spec$units),
      ylab = "Count",
      cex.lab = 0.9,
      cex.axis = 0.86
    )
  } else {
    tab <- if (spec$type == "binary") {
      table(factor(sample_x, levels = c(0, 1)))
    } else {
      table(factor(sample_x, levels = 0:max(sample_x)))
    }
    names_vec <- names(tab)
    if (spec$type == "binary") {
      names_vec <- c("No", "Yes")
    }
    barplot(
      as.numeric(tab),
      names.arg = names_vec,
      col = "#e6cfb3",
      border = NA,
      main = "One sample from the most recent run",
      cex.main = 0.98,
      xlab = spec$label,
      ylab = "Count",
      cex.lab = 0.9,
      cex.axis = 0.86,
      cex.names = 0.86
    )
  }
}

plot_sampling_distribution <- function(estimates,
                                       population,
                                       true_value,
                                       estimator,
                                       key,
                                       show_normal_curve = TRUE) {
  x_limits <- get_sampling_x_limits(
    population = population,
    key = key,
    estimator = estimator
  )
  hist_obj <- hist(
    estimates,
    breaks = "FD",
    col = "#d5e8d4",
    border = "white",
    main = sprintf("Sampling distribution of the %s", tolower(estimator)),
    cex.main = 0.94,
    xlim = x_limits,
    xlab = sprintf("Repeated-sample %s", tolower(estimator)),
    ylab = "Count",
    cex.lab = 0.9,
    cex.axis = 0.86
  )
  abline(v = true_value, lwd = 2, col = "#b53a3a")

  if (isTRUE(show_normal_curve)) {
    mu <- mean(estimates)
    sigma <- stats::sd(estimates)
    if (is.finite(sigma) && sigma > 0) {
      x_seq <- seq(min(hist_obj$breaks), max(hist_obj$breaks), length.out = 300)
      binwidth <- median(diff(hist_obj$breaks))
      y_curve <- stats::dnorm(x_seq, mean = mu, sd = sigma) * length(estimates) * binwidth
      lines(x_seq, y_curve, lwd = 2, col = "#1d6f8a")
    }
  }
  mtext("True population parameter", side = 3, adj = 0, line = 0.2, cex = 0.72, col = "#b53a3a")
  if (isTRUE(show_normal_curve)) {
    mtext("Normal overlay", side = 3, adj = 1, line = 0.2, cex = 0.72, col = "#1d6f8a")
    mtext(
      sampling_distribution_caption(TRUE),
      side = 1,
      line = 4.2,
      cex = 0.68,
      col = "#5b6470"
    )
  }
}

plot_last20_cis <- function(ci_df, true_value, estimator) {
  n_rows <- nrow(ci_df)
  y_pos <- seq_len(n_rows)
  colors <- ifelse(ci_df$contains_true, "#2a7f62", "#b53a3a")
  plot(
    ci_df$estimate,
    y_pos,
    type = "n",
    xlim = range(c(ci_df$lower, ci_df$upper, true_value), finite = TRUE),
    ylim = c(0.5, n_rows + 0.5),
    yaxt = "n",
    xlab = sprintf("%s estimate", estimator),
    ylab = "Draw order",
    main = sprintf("Last %d sample estimates with 95%% CIs", n_rows),
    cex.main = 0.9,
    cex.lab = 0.88,
    cex.axis = 0.72
  )
  abline(v = true_value, lwd = 2, col = "#b53a3a")
  segments(ci_df$lower, y_pos, ci_df$upper, y_pos, col = colors, lwd = 1.2)
  points(ci_df$estimate, y_pos, pch = 19, col = colors, cex = 0.5)
  axis(2, at = y_pos, labels = ci_df$draw, las = 1, cex.axis = 0.62)
  box()
}
