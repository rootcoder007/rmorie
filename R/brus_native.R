# Brus (2022) Spatial Sampling with R surface (The R Series, CRC Press;
# open-access edition dickbrus.github.io/SpatialSamplingwithR).
# Mirrors morie.fn._brus; every function cites its equation numbers.

#' Horvitz-Thompson estimators of the total and mean
#'
#' Equations 2.2 to 2.4 of Brus (2022): t_hat = sum z_k/pi_k with design
#' weights 1/pi_k, and zbar_hat = t_hat/N. Mirrors the ch2 modules.
#'
#' @param z Sample values.
#' @param pi Inclusion probabilities in (0, 1].
#' @param n_population Population size N.
#' @return List with total, mean, weights.
#' @export
#' @examples
#' morie_ht_estimators(z = c(10, 20, 15), pi = c(0.1, 0.2, 0.15), n_population = 100)
morie_ht_estimators <- function(z, pi, n_population) {
  z <- as.numeric(z)
  pi <- as.numeric(pi)
  stopifnot(
    length(z) == length(pi), all(pi > 0), all(pi <= 1),
    n_population > 0
  )
  total <- sum(z / pi)
  list(total = total, mean = total / n_population, weights = 1 / pi)
}

#' Simple random sampling estimators for proportions and infinite totals
#'
#' Equations 3.6, 3.14, 3.15, 3.18, 3.21 of Brus (2022).
#'
#' @param y 0/1 indicator sample (proportion pieces).
#' @param n,n_population Sample and population sizes.
#' @param estimate,variance,u_crit Interval pieces.
#' @param zbar_hat,area,sample_area,s2_hat Infinite-population pieces.
#' @return List with p_hat, var_p, ci, total_inf, var_total_inf (pieces
#'   whose inputs were supplied).
#' @export
#' @examples
#' morie_si_estimators()
morie_si_estimators <- function(y = NULL, n = NA, n_population = NA,
                                estimate = NA, variance = NA, u_crit = NA,
                                zbar_hat = NA, area = NA, sample_area = NA,
                                s2_hat = NA) {
  out <- list()
  if (!is.null(y)) {
    y <- as.numeric(y)
    stopifnot(all(y %in% c(0, 1)))
    out$p_hat <- mean(y)
    if (!is.na(n) && !is.na(n_population) && n >= 2) {
      out$var_p <- (1 - n / n_population) * out$p_hat * (1 - out$p_hat) /
        (n - 1)
    }
  }
  if (!is.na(estimate) && !is.na(variance) && !is.na(u_crit)) {
    out$ci <- c(
      estimate - u_crit * sqrt(variance),
      estimate + u_crit * sqrt(variance)
    )
  }
  if (!is.na(zbar_hat) && !is.na(area) && !is.na(sample_area)) {
    out$total_inf <- area / sample_area * zbar_hat
    if (!is.na(s2_hat) && !is.na(n)) {
      out$var_total_inf <- (area / sample_area)^2 * s2_hat / n
    }
  }
  out
}

#' Stratified simple random sampling estimators
#'
#' Equations 4.1, 4.2, 4.4, 4.18 of Brus (2022).
#'
#' @param stratum_means,stratum_weights,stratum_variances Stratum pieces.
#' @param c0,stratum_costs,stratum_sizes Cost model pieces.
#' @return List with mean, variance, cost (as supplied).
#' @export
#' @examples
#' morie_stsi_estimators()
morie_stsi_estimators <- function(stratum_means = NULL,
                                  stratum_weights = NULL,
                                  stratum_variances = NULL, c0 = NA,
                                  stratum_costs = NULL,
                                  stratum_sizes = NULL) {
  out <- list()
  if (!is.null(stratum_means)) {
    w <- as.numeric(stratum_weights)
    stopifnot(abs(sum(w) - 1) < 1e-8, all(w >= 0))
    out$mean <- sum(w * as.numeric(stratum_means))
  }
  if (!is.null(stratum_variances)) {
    w <- as.numeric(stratum_weights)
    out$variance <- sum(w^2 * as.numeric(stratum_variances))
  }
  if (!is.na(c0)) {
    out$cost <- c0 + sum(as.numeric(stratum_sizes) *
      as.numeric(stratum_costs))
  }
  out
}

#' Cluster and two-stage sampling estimators
#'
#' Equations 6.4, 6.9, 6.10, 7.2, 7.3, 7.7, 7.8, 7.13 of Brus (2022).
#'
#' @param cluster_totals,cluster_sizes,m_population,n_clusters_population
#'   Cluster pieces.
#' @param primary_unit_means PSU means (two-stage pieces).
#' @param s2_between,s2_within,n,m True-variance pieces.
#' @return List with total_pps, total_si, mean_from_total, ts_mean,
#'   ts_variance, s2_psu, true_variance (as supplied).
#' @export
#' @examples
#' morie_cluster_twostage()
morie_cluster_twostage <- function(cluster_totals = NULL,
                                   cluster_sizes = NULL,
                                   m_population = NA,
                                   n_clusters_population = NA,
                                   primary_unit_means = NULL,
                                   s2_between = NA, s2_within = NA,
                                   n = NA, m = NA) {
  out <- list()
  if (!is.null(cluster_totals)) {
    t <- as.numeric(cluster_totals)
    nn <- length(t)
    if (!is.null(cluster_sizes) && !is.na(m_population)) {
      mm <- as.numeric(cluster_sizes)
      stopifnot(all(mm > 0))
      out$total_pps <- m_population / nn * sum(t / mm)
      out$mean_from_total <- out$total_pps / m_population
    }
    if (!is.na(n_clusters_population)) {
      out$total_si <- n_clusters_population / nn * sum(t)
    }
  }
  if (!is.null(primary_unit_means)) {
    pm <- as.numeric(primary_unit_means)
    stopifnot(length(pm) >= 2)
    out$ts_mean <- mean(pm)
    out$s2_psu <- stats::var(pm)
    out$ts_variance <- out$s2_psu / length(pm)
  }
  if (!is.na(s2_between) && !is.na(s2_within) && !is.na(n) && !is.na(m)) {
    out$true_variance <- s2_between / n + s2_within / (n * m)
  }
  out
}

#' Optimal two-stage design sizes
#'
#' Equations 7.9 to 7.11 of Brus (2022).
#'
#' @param s_w,s_b Within and between PSU standard deviations.
#' @param c1,c2 PSU and SSU unit costs.
#' @param v_max Target variance.
#' @param c_max Budget.
#' @return List with m_opt, n_for_variance, n_for_budget.
#' @export
#' @examples
#' morie_twostage_design(s_w = c(1, 2, 3, 4, 5, 6, 7, 8), s_b = c(1, 2, 3, 4, 5, 6, 7, 8), c1 = c(1, 2, 3, 4, 5, 6, 7, 8), c2 = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_twostage_design <- function(s_w, s_b, c1, c2, v_max = NA,
                                  c_max = NA) {
  stopifnot(s_w > 0, s_b > 0, c1 > 0, c2 > 0)
  out <- list(m_opt = s_w / s_b * sqrt(c1 / c2))
  if (!is.na(v_max)) {
    out$n_for_variance <- (s_w * s_b * sqrt(c2 / c1) + s_b^2) / v_max
  }
  if (!is.na(c_max)) {
    out$n_for_budget <- c_max * s_b / (s_w * sqrt(c1 * c2) + s_b * c1)
  }
  out
}

#' pps with-replacement variance estimator of a total
#'
#' Equation 8.2 of Brus (2022): sum((z_k/p_k - t_hat)^2)/(n(n-1)).
#'
#' @param z Sample values. @param p Draw probabilities. @param t_hat
#'   Estimated total.
#' @return Numeric variance estimate.
#' @export
#' @examples
#' morie_pps_variance(z = c(1, 2, 3, 4, 5, 6, 7, 8), p = c(1, 2, 3, 4, 5, 6, 7, 8), t_hat = c(1, 2, 3, 4, 5, 6, 7, 8))
morie_pps_variance <- function(z, p, t_hat) {
  z <- as.numeric(z)
  p <- as.numeric(p)
  n <- length(z)
  stopifnot(length(p) == n, all(p > 0), n >= 2)
  sum((z / p - t_hat)^2) / (n * (n - 1))
}

#' Model-assisted (GREG) estimators
#'
#' Equations 10.2, 10.4, 10.6, 10.8, 10.9, 10.23, 10.27 of Brus (2022):
#' difference estimator, GLS coefficients (population and sample forms),
#' the general and slope-form regression estimators, and the ratio
#' estimator with its constant g-weight.
#'
#' @param m_all Model predictions over the population.
#' @param z_sample,m_sample,pi_sample Sample values, predictions, pi.
#' @param n_population Population size.
#' @param x,z,sigma2,pi GLS design pieces (x may be a matrix).
#' @param x_all,b_hat,x_sample General-regression pieces.
#' @param zbar_pi,b_hats,xbar_true,xbar_pi Slope-form pieces.
#' @param t_pi_z,t_pi_x,t_x_true Ratio pieces.
#' @return List with difference, gls_b, regr_general, regr_slopes, ratio,
#'   ratio_g (as supplied).
#' @export
#' @examples
#' morie_model_assisted()
morie_model_assisted <- function(m_all = NULL, z_sample = NULL,
                                 m_sample = NULL, pi_sample = NULL,
                                 n_population = NA, x = NULL, z = NULL,
                                 sigma2 = NULL, pi = NULL, x_all = NULL,
                                 b_hat = NULL, x_sample = NULL,
                                 zbar_pi = NA, b_hats = NULL,
                                 xbar_true = NULL, xbar_pi = NULL,
                                 t_pi_z = NA, t_pi_x = NA, t_x_true = NA) {
  out <- list()
  if (!is.null(m_all) && !is.null(z_sample)) {
    ma <- as.numeric(m_all)
    zs <- as.numeric(z_sample)
    ms <- as.numeric(m_sample)
    ps <- as.numeric(pi_sample)
    stopifnot(length(ma) == n_population, all(ps > 0))
    out$difference <- mean(ma) + sum((zs - ms) / ps) / n_population
  }
  if (!is.null(x) && !is.null(z)) {
    xm <- as.matrix(x)
    zv <- as.numeric(z)
    s2 <- if (is.null(sigma2)) rep(1, length(zv)) else as.numeric(sigma2)
    pv <- if (is.null(pi)) rep(1, length(zv)) else as.numeric(pi)
    w <- 1 / (s2 * pv)
    xtx <- t(xm * w) %*% xm
    out$gls_b <- as.numeric(solve(xtx, t(xm * w) %*% zv))
  }
  if (!is.null(x_all) && !is.null(b_hat)) {
    xa <- as.matrix(x_all)
    xs <- as.matrix(x_sample)
    b <- as.numeric(b_hat)
    zs <- as.numeric(z_sample)
    ps <- as.numeric(pi_sample)
    resid <- zs - as.numeric(xs %*% b)
    out$regr_general <- mean(as.numeric(xa %*% b)) +
      sum(resid / ps) / n_population
  }
  if (!is.na(zbar_pi) && !is.null(b_hats)) {
    out$regr_slopes <- zbar_pi + sum(as.numeric(b_hats) *
      (as.numeric(xbar_true) -
        as.numeric(xbar_pi)))
  }
  if (!is.na(t_pi_z) && !is.na(t_pi_x) && !is.na(t_x_true)) {
    stopifnot(t_pi_x != 0)
    out$ratio <- t_pi_z / t_pi_x * t_x_true
    out$ratio_g <- t_x_true / t_pi_x
  }
  out
}

#' GREG variance estimators and g-weights
#'
#' Equations 10.13, 10.14, 10.17, 10.18, 10.25, 10.42 of Brus (2022).
#'
#' @param e Residuals. @param n,n_population Sizes.
#' @param x_k,xbar_true,xbar_sample,s2_x Simple g-weight pieces.
#' @param g g-weights for the weighted variance.
#' @param pi Inclusion probabilities (calibration-residual route).
#' @param ratio If TRUE return the N^2 S2(e)/n ratio-total variance.
#' @return List with s2_e, variance, g_weight, g_variance, ratio_variance,
#'   mc_variance (as supplied).
#' @export
#' @examples
#' morie_greg_variance()
morie_greg_variance <- function(e = NULL, n = NA, n_population = NA,
                                x_k = NA, xbar_true = NA, xbar_sample = NA,
                                s2_x = NA, g = NULL, pi = NULL,
                                ratio = FALSE) {
  out <- list()
  if (!is.null(e)) {
    ev <- as.numeric(e)
    stopifnot(length(ev) == n, n >= 2)
    out$s2_e <- sum(ev^2) / (n - 1)
    if (!is.na(n_population)) {
      out$variance <- (1 - n / n_population) * out$s2_e / n
      if (ratio) out$ratio_variance <- n_population^2 * out$s2_e / n
    }
    if (!is.null(g)) {
      out$g_variance <- (1 - n / n_population) *
        sum(as.numeric(g)^2 * ev^2) / (n * (n - 1))
    }
    if (!is.null(pi)) {
      pv <- as.numeric(pi)
      stopifnot(all(pv > 0))
      t_hat <- sum(ev / pv)
      out$mc_variance <- sum((n * ev / pv - t_hat)^2) / (n * (n - 1)) /
        n_population^2
    }
  }
  if (!is.na(x_k)) {
    out$g_weight <- 1 + (xbar_true - xbar_sample) * (x_k - xbar_sample) /
      s2_x
  }
  out
}

#' Mixed-model calibration estimators
#'
#' Equations 10.32, 10.36, 10.38, 10.40 of Brus (2022): poststratified
#' mean, the mixed calibration estimator, its intercept, and the SI
#' shortcut.
#'
#' @param group_means_sample,group_weights Poststratification pieces.
#' @param zbar_pi,a_hat,pi_sample,m_all_mean,m_ht_mean,b_hat,n_population
#'   Calibration pieces.
#' @param z_sample,b_si,m_sample_mean SI shortcut pieces.
#' @return List with poststratified, calibrated, intercept, si_shortcut.
#' @export
#' @examples
#' morie_calibration()
morie_calibration <- function(group_means_sample = NULL,
                              group_weights = NULL, zbar_pi = NA,
                              a_hat = NA, pi_sample = NULL,
                              m_all_mean = NA, m_ht_mean = NA, b_hat = NA,
                              n_population = NA, z_sample = NULL,
                              b_si = NA, m_sample_mean = NA) {
  out <- list()
  if (!is.null(group_means_sample)) {
    w <- as.numeric(group_weights)
    stopifnot(abs(sum(w) - 1) < 1e-8)
    out$poststratified <- sum(w * as.numeric(group_means_sample))
  }
  if (!is.null(pi_sample) && !is.na(b_hat)) {
    pv <- as.numeric(pi_sample)
    stopifnot(all(pv > 0))
    if (!is.null(z_sample) && !is.na(n_population) && is.na(a_hat)) {
      out$intercept <- (1 - b_hat) * sum(as.numeric(z_sample) / pv) /
        n_population
    }
    if (!is.na(zbar_pi) && !is.na(a_hat)) {
      out$calibrated <- zbar_pi + a_hat * (1 - sum(1 / pv) /
        n_population) +
        b_hat * (m_all_mean - m_ht_mean)
    }
  }
  if (!is.null(z_sample) && !is.na(b_si)) {
    out$si_shortcut <- mean(as.numeric(z_sample)) +
      b_si * (m_all_mean - m_sample_mean)
  }
  out
}

#' Balanced/spread-sample and two-phase variance estimators
#'
#' Equations 9.2, 9.3, 9.10, 11.5, 11.7, 11.8 of Brus (2022).
#'
#' @param t_pi_z,t_x_true,t_pi_x,b_hat Regression-total pieces.
#' @param e,pi,c_k,n_population,p Balanced pieces.
#' @param e_local_mean,n Local-mean pieces.
#' @param n1h,n1,s2_2h,n2h,zbar_2h,zbar_hat Two-phase stratification.
#' @param s2_z,s2_e,n2 Two-phase regression pieces.
#' @return List with regression_total, balanced_variance,
#'   local_mean_variance, twophase_strat, twophase_regr, s2_resid.
#' @export
#' @examples
#' morie_balanced_twophase()
morie_balanced_twophase <- function(t_pi_z = NA, t_x_true = NA,
                                    t_pi_x = NA, b_hat = NA, e = NULL,
                                    pi = NULL, c_k = NULL,
                                    n_population = NA, p = NA,
                                    e_local_mean = NULL, n = NA,
                                    n1h = NULL, n1 = NA, s2_2h = NULL,
                                    n2h = NULL, zbar_2h = NULL,
                                    zbar_hat = NA, s2_z = NA, s2_e = NA,
                                    n2 = NA) {
  out <- list()
  if (!is.na(t_pi_z) && !is.na(b_hat)) {
    out$regression_total <- t_pi_z + b_hat * (t_x_true - t_pi_x)
  }
  if (!is.null(e)) {
    ev <- as.numeric(e)
    if (!is.null(pi)) {
      pv <- as.numeric(pi)
      nn <- length(ev)
      if (!is.null(c_k) && !is.na(n_population)) {
        out$balanced_variance <- sum(as.numeric(c_k) * (ev / pv)^2) *
          nn / (nn - p) / n_population^2
      }
      if (!is.null(e_local_mean)) {
        out$local_mean_variance <- n / (n - p) * p / (p + 1) *
          sum((1 - pv) * (ev / pv - as.numeric(e_local_mean))^2)
      }
    }
    if (!is.na(n) && is.null(pi)) {
      out$s2_resid <- sum(ev^2) / (n - 1)
    }
  }
  if (!is.null(n1h)) {
    a <- as.numeric(n1h)
    out$twophase_strat <- sum((a / n1)^2 * as.numeric(s2_2h) /
      as.numeric(n2h)) +
      sum((a / n1) * (as.numeric(zbar_2h) - zbar_hat)^2) / n1
  }
  if (!is.na(s2_z) && !is.na(s2_e)) {
    out$twophase_regr <- (1 - n1 / n_population) * s2_z / n1 +
      (1 - n2 / n1) * s2_e / n2
  }
  out
}

#' Required sample sizes and Bayesian criteria
#'
#' Equations 12.3, 12.7, 12.10, 12.11, 12.14, 12.17 to 12.19, 12.24,
#' 12.25, 12.27 of Brus (2022).
#'
#' @param p_star,se_max,u_crit,s_star,l_max,cv_star,r_max Frequentist
#'   pieces. @param design_effect,n_si Design-effect pieces.
#' @param p,z,n,c,d Beta posterior pieces. @param v,l Interval pieces.
#' @param lengths,probs,coverages,alpha ALC/ACC pieces.
#' @return List with the requested sample sizes and criteria.
#' @export
#' @examples
#' morie_sample_size()
morie_sample_size <- function(p_star = NA, se_max = NA, u_crit = NA,
                              s_star = NA, l_max = NA, cv_star = NA,
                              r_max = NA, design_effect = NA, n_si = NA,
                              p = NA, z = NA, n = NA, c = NA, d = NA,
                              v = NA, l = NA, lengths = NULL,
                              probs = NULL, coverages = NULL,
                              alpha = NA) {
  out <- list()
  if (!is.na(p_star) && !is.na(se_max)) {
    out$n_prop_se <- (sqrt(p_star * (1 - p_star)) / se_max)^2 + 1
  }
  if (!is.na(u_crit) && !is.na(s_star) && !is.na(l_max)) {
    out$n_mean_length <- (u_crit * s_star / (l_max / 2))^2
  }
  if (!is.na(u_crit) && !is.na(cv_star) && !is.na(r_max)) {
    out$n_cv <- (u_crit * cv_star / r_max)^2
  }
  if (!is.na(u_crit) && !is.na(p_star) && !is.na(l_max)) {
    out$n_prop_length <- (u_crit * sqrt(p_star * (1 - p_star)) /
      (l_max / 2))^2 + 1
  }
  if (!is.na(design_effect) && !is.na(n_si)) {
    out$n_design_effect <- sqrt(design_effect) * n_si
  }
  if (!is.na(p) && !is.na(z) && !is.na(n)) {
    out$beta_pdf <- stats::dbeta(p, z + c, n - z + d)
  }
  if (!is.na(v) && !is.na(l) && !is.na(z) && !is.na(n)) {
    out$interval_prob <- stats::pbeta(min(v + l, 1), z + c, n - z + d) -
      stats::pbeta(v, z + c, n - z + d)
  }
  if (!is.null(lengths)) {
    el <- sum(as.numeric(lengths) * as.numeric(probs))
    out$expected_length <- el
    if (!is.na(l_max)) out$alc_satisfied <- el <= l_max
  }
  if (!is.null(coverages)) {
    ec <- sum(as.numeric(coverages) * as.numeric(probs))
    out$expected_coverage <- ec
    if (!is.na(alpha)) out$acc_satisfied <- ec >= 1 - alpha
  }
  out
}

#' Model-based prediction of design variances (Ospats family)
#'
#' Equations 13.5, 13.7, 13.10, 13.12, and print equations 13.15 to 13.17
#' of Brus (2022). The print edition tags E_xi of the stratum variance as
#' 13.16 (untagged on the web) and the Ospats objective as 13.17 (web
#' 13.16).
#'
#' @param gamma_bar_h Mean semivariances. @param weights,n_h Strata.
#' @param n Total sample size. @param s_h,c_h Allocation pieces.
#' @param zhat_i,zhat_j,r2,s2_i,s2_j,s2_ij Pairwise pieces.
#' @param d2_upper_sum,n_h_units Stratum-variance pieces.
#' @param per_stratum_sums,n_population Objective pieces.
#' @return List with stsi_variance, equal_area_variance, alloc_variance,
#'   objective_o, d2, stratum_variance, ospats_objective (as supplied).
#' @export
#' @examples
#' morie_ospats()
morie_ospats <- function(gamma_bar_h = NULL, weights = NULL, n_h = NULL,
                         n = NA, s_h = NULL, c_h = NULL, zhat_i = NA,
                         zhat_j = NA, r2 = NA, s2_i = NA, s2_j = NA,
                         s2_ij = NA, d2_upper_sum = NA, n_h_units = NA,
                         per_stratum_sums = NULL, n_population = NA) {
  out <- list()
  if (!is.null(gamma_bar_h)) {
    g <- as.numeric(gamma_bar_h)
    if (!is.null(weights) && !is.null(n_h)) {
      out$stsi_variance <- sum(as.numeric(weights)^2 * g /
        as.numeric(n_h))
    }
    if (!is.na(n)) out$equal_area_variance <- sum(g) / n^2
  }
  if (!is.null(s_h) && !is.null(weights)) {
    w <- as.numeric(weights)
    s <- as.numeric(s_h)
    if (!is.null(c_h) && !is.na(n)) {
      cc <- as.numeric(c_h)
      out$alloc_variance <- sum(w * s * sqrt(cc)) *
        sum(w * s / sqrt(cc)) / n
    }
    out$objective_o <- sum(w * s)^2
  }
  if (!is.na(zhat_i) && !is.na(r2)) {
    out$d2 <- (zhat_i - zhat_j)^2 / r2 + s2_i + s2_j - 2 * s2_ij
  }
  if (!is.na(d2_upper_sum) && !is.na(n_h_units)) {
    out$stratum_variance <- d2_upper_sum / n_h_units^2
  }
  if (!is.null(per_stratum_sums) && !is.na(n_population)) {
    out$ospats_objective <- sum(sqrt(as.numeric(per_stratum_sums))) /
      n_population
  }
  out
}

#' Ordinary kriging system, variances, and the exponential semivariogram
#'
#' Equations 21.4, 21.8, 21.11, 21.13, 21.23 of Brus (2022). The
#' exponential semivariogram implements c0 + c1 (1 - exp(-h/phi)) with
#' gamma(0) = 0: the book's own prose (95 percent of the sill at three
#' distance parameters) pins this form; the printed exp term is a display
#' typo.
#'
#' @param cov_ss,cov_s0 Kriging system pieces. @param sigma2,lam,nu
#'   Variance pieces. @param gamma_s0 Semivariance form pieces.
#' @param h,c0,c1,phi Semivariogram pieces. @param z,mu,cov Likelihood.
#' @return List with lam, nu, v_ok_cov, v_ok_gamma, gamma_h, loglik.
#' @export
#' @examples
#' morie_kriging()
morie_kriging <- function(cov_ss = NULL, cov_s0 = NULL, sigma2 = NA,
                          lam = NULL, nu = NA, gamma_s0 = NULL, h = NULL,
                          c0 = NA, c1 = NA, phi = NA, z = NULL,
                          mu = NULL, cov = NULL) {
  out <- list()
  if (!is.null(cov_ss) && !is.null(cov_s0)) {
    cm <- as.matrix(cov_ss)
    c0v <- as.numeric(cov_s0)
    nn <- length(c0v)
    a <- rbind(cbind(cm, 1), c(rep(1, nn), 0))
    sol <- solve(a, c(c0v, 1))
    out$lam <- sol[seq_len(nn)]
    out$nu <- sol[nn + 1]
  }
  if (!is.null(lam) && !is.null(cov_s0) && !is.na(sigma2) && !is.na(nu)) {
    out$v_ok_cov <- sigma2 - sum(as.numeric(lam) * as.numeric(cov_s0)) -
      nu
  }
  if (!is.null(lam) && !is.null(gamma_s0) && !is.na(nu)) {
    out$v_ok_gamma <- sum(as.numeric(lam) * as.numeric(gamma_s0)) + nu
  }
  if (!is.null(h) && !is.na(c1)) {
    hv <- as.numeric(h)
    stopifnot(all(hv >= 0), c0 >= 0, c1 > 0, phi > 0)
    out$gamma_h <- ifelse(hv == 0, 0, c0 + c1 * (1 - exp(-hv / phi)))
  }
  if (!is.null(z) && !is.null(cov)) {
    zv <- as.numeric(z)
    mv <- as.numeric(mu)
    cm <- as.matrix(cov)
    diff <- zv - mv
    ld <- determinant(cm, logarithm = TRUE)
    stopifnot(ld$sign > 0)
    out$loglik <- -0.5 * (length(zv) * log(2 * pi) +
      as.numeric(ld$modulus) +
      as.numeric(t(diff) %*% solve(cm, diff)))
  }
  out
}

#' Variogram-sampling design criteria
#'
#' Equations 24.1 to 24.6 of Brus (2022): nested ANOVA composition, REML
#' Fisher information, variance of the kriging variance, the augmented
#' kriging variance, expected squared prediction shift, and the
#' estimation-adjusted criterion.
#'
#' @param mu,a_i,b_ij,c_ijk,eps Nested pieces. @param a,da_list REML
#'   pieces. @param cov_theta,dv_dtheta VKV pieces. @param v_ok,e_tau2 AKV
#'   pieces. @param dlam_dtheta,a_mat Tau2 pieces. @param akv,vkv EAC.
#' @return List with nested, fisher_info, vkv, akv, e_tau2, eac.
#' @export
#' @examples
#' morie_variogram_design()
morie_variogram_design <- function(mu = NA, a_i = NA, b_ij = NA,
                                   c_ijk = NA, eps = NA, a = NULL,
                                   da_list = NULL, cov_theta = NULL,
                                   dv_dtheta = NULL, v_ok = NA,
                                   e_tau2 = NA, dlam_dtheta = NULL,
                                   a_mat = NULL, akv = NA, vkv = NA) {
  out <- list()
  if (!is.na(mu)) out$nested <- mu + a_i + b_ij + c_ijk + eps
  if (!is.null(a) && !is.null(da_list)) {
    am <- as.matrix(a)
    ai <- solve(am)
    pp <- length(da_list)
    info <- matrix(0, pp, pp)
    for (i in seq_len(pp)) {
      for (j in seq_len(pp)) {
        info[i, j] <- 0.5 * sum(diag(ai %*% as.matrix(da_list[[i]]) %*%
          ai %*% as.matrix(da_list[[j]])))
      }
    }
    out$fisher_info <- info
  }
  if (!is.null(cov_theta) && !is.null(dv_dtheta)) {
    dv <- as.numeric(dv_dtheta)
    out$vkv <- as.numeric(t(dv) %*% as.matrix(cov_theta) %*% dv)
  }
  if (!is.na(v_ok) && !is.na(e_tau2)) out$akv <- v_ok + e_tau2
  if (!is.null(dlam_dtheta) && !is.null(a_mat)) {
    ct <- as.matrix(cov_theta)
    am <- as.matrix(a_mat)
    pp <- length(dlam_dtheta)
    acc <- 0
    for (i in seq_len(pp)) {
      for (j in seq_len(pp)) {
        acc <- acc + ct[i, j] *
          as.numeric(t(as.numeric(dlam_dtheta[[i]])) %*% am %*%
            as.numeric(dlam_dtheta[[j]]))
      }
    }
    out$e_tau2 <- acc
  }
  if (!is.na(akv) && !is.na(vkv) && !is.na(v_ok)) {
    out$eac <- akv + vkv / (2 * v_ok)
  }
  out
}

#' Design-based vs model-based variance identities (chapter 26)
#'
#' Equations 26.2 to 26.5 of Brus (2022), plus the small-area, repeated
#' survey, response surface and validation pieces of chapters 14 to 25:
#' small-area mean (14.15), trend weights (15.4), GLS estimator (15.10),
#' linear working model (16.1), OLS beta and prediction variance (20.2,
#' 20.3), classification indicator (25.8).
#'
#' @param sigma2,n,rho_bar,s2,n_population Chapter 26 pieces.
#' @param xbar_d,beta_hat,v_d Small-area pieces. @param times Trend.
#' @param x,c_mat,zhat GLS pieces. @param beta0,beta1,x_val Model 16.1.
#' @param x_design,z_obs OLS pieces. @param sigma2_eps,x0 Prediction var.
#' @param c_hat,c_true,u Classification pieces.
#' @return List of the requested quantities.
#' @export
#' @examples
#' morie_survey_variances()
morie_survey_variances <- function(sigma2 = NA, n = NA, rho_bar = NA,
                                   s2 = NA, n_population = NA,
                                   xbar_d = NULL, beta_hat = NULL,
                                   v_d = NA, times = NULL, x = NULL,
                                   c_mat = NULL, zhat = NULL, beta0 = NA,
                                   beta1 = NA, x_val = NA,
                                   x_design = NULL, z_obs = NULL,
                                   sigma2_eps = NA, x0 = NULL,
                                   c_hat = NULL, c_true = NULL,
                                   u = NULL) {
  out <- list()
  if (!is.na(sigma2) && !is.na(n)) {
    out$v_iid <- sigma2 / n
    if (!is.na(rho_bar)) {
      out$v_autocorrelated <- sigma2 / n * (1 + (n - 1) * rho_bar)
      out$n_effective <- n / (1 + (n - 1) * rho_bar)
    }
  }
  if (!is.na(s2) && !is.na(n) && !is.na(n_population)) {
    out$v_fpc <- (1 - n / n_population) * s2 / n
  }
  if (!is.null(xbar_d)) {
    out$small_area <- sum(as.numeric(xbar_d) * as.numeric(beta_hat)) +
      v_d
  }
  if (!is.null(times)) {
    t <- as.numeric(times)
    d <- t - mean(t)
    out$trend_weights <- d / sum(d^2)
  }
  if (!is.null(x) && !is.null(c_mat)) {
    xm <- as.matrix(x)
    ci_x <- solve(as.matrix(c_mat), xm)
    ci_z <- solve(as.matrix(c_mat), as.numeric(zhat))
    out$gls <- as.numeric(solve(t(xm) %*% ci_x, t(xm) %*% ci_z))
  }
  if (!is.na(beta0)) out$linear_model <- beta0 + beta1 * x_val
  if (!is.null(x_design) && !is.null(z_obs)) {
    xm <- as.matrix(x_design)
    out$ols_beta <- as.numeric(solve(
      t(xm) %*% xm,
      t(xm) %*% as.numeric(z_obs)
    ))
    if (!is.na(sigma2_eps) && !is.null(x0)) {
      x0v <- as.numeric(x0)
      out$ols_pred_var <- sigma2_eps *
        (1 + as.numeric(t(x0v) %*% solve(t(xm) %*% xm, x0v)))
    }
  }
  if (!is.null(c_hat)) {
    out$class_indicator <- as.numeric(identical(c_hat, c_true) &&
      identical(c_hat, u))
  }
  out
}
