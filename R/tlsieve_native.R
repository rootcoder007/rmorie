# morie.fn -- function file (rootcoder007/morie)
# r"""Cumulative vaccine sieve effects.
#
# An HIV vaccine is built from a few antigens, so it may protect well
# against strains resembling them and poorly against antigenically
# distant ones. **Sieve analysis** asks how efficacy varies with the
# virus's genetic characteristics -- the vaccine as a sieve, with "holes"
# that particular strains pass through. A sieve effect at a genetic locus
# is the difference in vaccine efficacy comparing viruses *matched* to
# the vaccine at that locus with viruses *mismatched* there, and it
# guides which antigens a future multivalent vaccine should carry.
#
# **Statistically this is competing risks.** Each viral genotype is a
# distinct endpoint; a participant infected by one type is no longer at
# risk of a first infection by another. The cumulative parameter is
# **cumulative incidence** -- the probability of being infected by time
# :math:`t` *with a virus of that type*:
#
# .. math:: F_j(t) = \int_0^t S(u^-)\, d\Lambda_j(u),
#
# the type-:math:`j` hazard integrated against overall survival. Vaccine
# efficacy at :math:`t` for type :math:`j` is
# :math:`VE_j(t) = 1 - F_j^{vac}(t)/F_j^{pla}(t)`, and the sieve effect
# contrasts :math:`VE` between matched and mismatched types. The
# cumulative parameter, rather than the instantaneous hazard, is the one
# with public health meaning when vaccine effects **wane**.
#
# **Why not the standard estimator.** Aalen-Johansen is consistent under
# uninformative censoring and, with no covariates, nonparametric
# efficient. But informative censoring is routine in a longitudinal
# trial and prognostic covariates -- sexual risk behaviour, say -- are
# collected as a matter of course. Using them weakens the censoring
# assumption *and* improves efficiency. The semiparametric alternatives
# that do use covariates require a correctly specified finite-dimensional
# regression, which is the assumption TMLE removes.
#
# The anchor exploits the structure rather than a reference
# implementation: with no censoring and no covariates the TMLE must
# reproduce Aalen-Johansen exactly, the cumulative incidences of all
# types plus survival must sum to one at every time, and under
# informative censoring the covariate-using estimator must beat the
# naive one.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) *Targeted Learning in Data
# Science*, Springer, doi:10.1007/978-3-319-65304-4. Chap. 11
# (Benkeser, Carone & Gilbert): sieve analysis as the study of how
# vaccine efficacy varies with viral genetics; the sieve effect at a
# locus as the difference in efficacy between matched and mismatched
# viruses, and its use in selecting antigens for multivalent vaccines;
# the competing risks framework with each genotype a separate endpoint;
# cumulative incidence as the cumulative parameter, of greater public
# health relevance when vaccine effects wane; the Aalen-Johansen
# estimator's consistency under uninformative censoring and
# nonparametric efficiency absent covariates; the concern of informative
# censoring and the routine availability of prognostic covariates; and
# the drawback that semiparametric hazard-based alternatives require a
# correctly specified finite-dimensional regression model.
#
# Aalen, O. O. & Johansen, S. (1978) "An Empirical Transition Matrix for
# Non-Homogeneous Markov Chains Based on Censored Observations",
# *Scandinavian Journal of Statistics* 5(3), 141-150. The estimator
# being improved on.
#
# Gilbert, P. B., Self, S. G. & Ashby, M. A. (1998) "Statistical methods
# for assessing differential vaccine protection against human
# immunodeficiency virus types", *Biometrics* 54(3), 799-814,
# doi:10.2307/2533838. Sieve analysis.
# """

.tlsieve_eps <- 1e-12


.tlsieve_cause_specific_hazard <- function(time, event_type, times, weights = NULL) {
  t <- as.numeric(time)
  e <- as.integer(event_type)
  n <- length(t)
  if (length(e) != n) {
    stop("tlsieve: ", n, " times but ", length(e), " event types")
  }
  if (is.null(weights)) {
    w <- rep(1.0, n)
  } else {
    w <- as.numeric(weights)
  }
  types <- sort(unique(e[e > 0]))
  out <- list()
  for (j in types) {
    h <- numeric(length(times))
    for (u_idx in seq_along(times)) {
      u <- times[u_idx]
      at_risk <- sum(w[t >= u])
      ev <- sum(w[abs(t - u) < .tlsieve_eps & e == j])
      h[u_idx] <- if (at_risk > .tlsieve_eps) ev / at_risk else 0.0
    }
    out[[as.character(j)]] <- h
  }
  list(hazards = out, types = types, times = as.list(times))
}


.tlsieve_cumulative_incidence <- function(hazards, times) {
  type_names <- names(hazards)
  if (is.null(type_names) || length(type_names) == 0) {
    stop("tlsieve: no event types given")
  }
  types <- sort(as.integer(type_names))
  m <- length(times)
  S <- 1.0
  F <- list()
  for (j in types) {
    F[[as.character(j)]] <- numeric(m)
  }
  surv <- numeric(m)
  closure <- numeric(m)

  for (u_idx in seq_len(m)) {
    tot <- 0
    for (j in types) {
      tot <- tot + hazards[[as.character(j)]][u_idx]
    }
    for (j in types) {
      prev <- if (u_idx > 1) F[[as.character(j)]][u_idx - 1] else 0.0
      F[[as.character(j)]][u_idx] <- prev + S * hazards[[as.character(j)]][u_idx]
    }
    S <- S * (1.0 - tot)
    surv[u_idx] <- S
    closure[u_idx] <- surv[u_idx] +
      sum(sapply(types, function(j) F[[as.character(j)]][u_idx]))
  }

  list(F = F, survival = surv, times = as.list(times), types = types,
       closure = closure,
       note = "cumulative incidences plus survival sum to 1 at every time")
}


.tlsieve_aalen_johansen <- function(time, event_type, times, weights = NULL) {
  h <- .tlsieve_cause_specific_hazard(time, event_type, times, weights)
  ci <- .tlsieve_cumulative_incidence(h$hazards, times)
  list(estimate = ci$F, F = ci$F,
       survival = ci$survival, types = ci$types,
       times = as.list(times), closure = ci$closure,
       method = "Aalen-Johansen cumulative incidence; van der Laan & Rose (2018) Chap. 11",
       caveat = "consistent under UNINFORMATIVE censoring and efficient only absent covariates")
}


.tlsieve_vaccine_efficacy <- function(F_vaccine, F_placebo) {
  a <- as.numeric(F_vaccine)
  b <- as.numeric(F_placebo)
  if (length(a) != length(b)) {
    stop("tlsieve: the two arms differ in length")
  }
  result <- numeric(length(a))
  for (i in seq_along(a)) {
    result[i] <- if (b[i] > .tlsieve_eps) 1.0 - a[i] / b[i] else NaN
  }
  result
}


#' morie_tlsieve
#'
#' Part of the tlsieve_native implementation; see the file header for
#' the source it follows.
#'
#' @param F_vac_matched See Usage.
#' @param F_pla_matched See Usage.
#' @param F_vac_mismatched See Usage.
#' @param F_pla_mismatched See Usage.
#' @return A list with \code{estimate}, \code{sieve_effect}, \code{ve_matched}, \code{ve_mismatched}, \code{method}, \code{note}.
#' @export
morie_tlsieve <- function(F_vac_matched, F_pla_matched, F_vac_mismatched,
                          F_pla_mismatched) {
  ve_m <- .tlsieve_vaccine_efficacy(F_vac_matched, F_pla_matched)
  ve_x <- .tlsieve_vaccine_efficacy(F_vac_mismatched, F_pla_mismatched)
  if (length(ve_m) != length(ve_x)) {
    stop("tlsieve: matched and mismatched series differ in length")
  }
  d <- ve_m - ve_x
  list(estimate = d, sieve_effect = d,
       ve_matched = ve_m, ve_mismatched = ve_x,
       method = "cumulative sieve effect; van der Laan & Rose (2018) Chap. 11",
       note = "zero at every time means the vaccine does not sieve at this locus")
}


.tlsieve_cheatsheet <- function() {
  paste("tlsieve: an HIV vaccine built from a few antigens protects",
        "unevenly across strains, so SIEVE ANALYSIS asks how efficacy",
        "varies with viral genetics -- the sieve effect at a locus is",
        "efficacy against MATCHED minus MISMATCHED viruses. Each",
        "genotype is a competing risk, and the parameter is CUMULATIVE",
        "INCIDENCE, F_j = integral of S(u-) dLambda_j -- the cumulative",
        "form matters when vaccine effects WANE. Aalen-Johansen is",
        "consistent under uninformative censoring and efficient without",
        "covariates; using covariates weakens the censoring assumption",
        "and gains efficiency, which is what TMLE does without a",
        "parametric hazard model.")
}


# compact alias per ledger/NAMING.md
vaccinesieve <- morie_tlsieve
