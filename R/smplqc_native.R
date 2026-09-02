# SPDX-License-Identifier: AGPL-3.0-or-later
#
# Per-sample genotype QC (Smplqc): call rate, heterozygosity rate,
# method-of-moments F. Bit-identical mirror of src/morie/fn/smplqc.py.

#' Per-sample genotype quality control
#'
#' Follows the sample-QC stages of Marees et al. (2018): individuals
#' are flagged when their genotype call rate is below
#' \code{callrate_min} (Marees step 1, missingness above 0.02) and
#' when their heterozygosity rate deviates more than \code{het_sd}
#' standard deviations from the sample mean (Marees step 5).
#'
#' Per individual i over its non-missing variants: call rate
#' \eqn{n_i / m}; observed homozygote count \eqn{O_i}; expected
#' homozygote count under Hardy-Weinberg
#' \eqn{E_i = \sum_j (1 - 2 p_j (1 - p_j) c_j)} with \eqn{c_j =
#' N_j/(N_j - 1)} when \code{small_sample} is TRUE (Nei's unbiased
#' form; PLINK restores it via its small-sample modifier); the
#' method-of-moments inbreeding coefficient
#' \eqn{F_i = (O_i - E_i)/(n_i - E_i)} (PLINK 1.9 het report); and the
#' heterozygosity rate \eqn{(n_i - O_i)/n_i} used by Marees et al.
#'
#' @param G Genotype matrix, individuals by variants, coded 0/1/2; any
#'   other value is treated as missing.
#' @param callrate_min Call-rate flag threshold (default 0.98).
#' @param het_sd Heterozygosity flag width in standard deviations.
#' @param small_sample Apply the N/(N-1) multiplier to expected
#'   homozygosity.
#' @return List with \code{estimate} (number of samples passing),
#'   \code{callrate}, \code{het_rate}, \code{F}, \code{obs_hom},
#'   \code{exp_hom}, \code{n_obs}, \code{flag_callrate},
#'   \code{flag_het}, \code{pass_qc}, \code{het_mean}, \code{het_sd},
#'   \code{freq}, \code{n}, \code{m}, \code{method}.
#' @references Marees, A. T., de Kluiver, H., Stringer, S., et al.
#'   (2018). A tutorial on conducting genome-wide association studies.
#'   International Journal of Methods in Psychiatric Research 27(2),
#'   e1608, Table 1 steps 1 and 5 (fetched-wave3 PDF). PLINK 1.9
#'   basic statistics documentation, het and missing reports
#'   (cog-genomics.org/plink/1.9/basic_stats, fetched 2026-08-09).
#'   Nei, M. (1978). Genetics 89(3), 583-590.
#' @export
#' @examples
#' V <- c(1, 2, 3, 4, 5, 6, 7, 8)
#' Smplqc(V)
Smplqc <- function(G, callrate_min = 0.98, het_sd = 3.0,
                   small_sample = FALSE) {
  Gm <- as.matrix(G)
  storage.mode(Gm) <- "double"
  n <- nrow(Gm); m <- ncol(Gm)
  if (n == 0L) stop("empty genotype matrix", call. = FALSE)
  is_valid <- function(v) v %in% c(0, 1, 2)
  freq <- numeric(m); nobs_col <- integer(m)
  for (j in seq_len(m)) {
    obs <- Gm[is_valid(Gm[, j]), j]
    nobs_col[j] <- length(obs)
    freq[j] <- if (length(obs) > 0) sum(obs) / (2 * length(obs)) else NaN
  }
  callrate <- numeric(n); het_rate <- numeric(n); Fv <- numeric(n)
  obs_hom <- integer(n); exp_hom <- numeric(n); n_obs_v <- integer(n)
  for (i in seq_len(n)) {
    n_obs <- 0L; o_hom <- 0L; e_hom <- 0
    for (j in seq_len(m)) {
      g <- Gm[i, j]
      if (!is_valid(g)) next
      n_obs <- n_obs + 1L
      if (g != 1) o_hom <- o_hom + 1L
      p <- freq[j]
      cj <- if (small_sample && nobs_col[j] > 1L) {
        nobs_col[j] / (nobs_col[j] - 1)
      } else 1
      e_hom <- e_hom + 1 - 2 * p * (1 - p) * cj
    }
    callrate[i] <- n_obs / m
    n_obs_v[i] <- n_obs
    obs_hom[i] <- o_hom
    exp_hom[i] <- e_hom
    den <- n_obs - e_hom
    Fv[i] <- if (den != 0) (o_hom - e_hom) / den else NaN
    het_rate[i] <- if (n_obs > 0L) (n_obs - o_hom) / n_obs else NaN
  }
  het_ok <- het_rate[!is.nan(het_rate)]
  hmean <- if (length(het_ok) > 0) mean(het_ok) else NaN
  hsd <- if (length(het_ok) > 1) {
    sqrt(sum((het_ok - hmean)^2) / (length(het_ok) - 1))
  } else NaN
  flag_cr <- callrate < as.numeric(callrate_min)
  flag_het <- !is.nan(het_rate) & !is.nan(hsd) &
    abs(het_rate - hmean) > as.numeric(het_sd) * hsd
  pass_qc <- !(flag_cr | flag_het)
  list(
    estimate = as.numeric(sum(pass_qc)),
    callrate = callrate, het_rate = het_rate, F = Fv,
    obs_hom = obs_hom, exp_hom = exp_hom, n_obs = n_obs_v,
    flag_callrate = flag_cr, flag_het = flag_het, pass_qc = pass_qc,
    het_mean = hmean, het_sd = hsd, freq = freq,
    n = as.integer(n), m = as.integer(m),
    method = "Sample QC (Marees 2018 steps 1+5; PLINK --het F)")
}
