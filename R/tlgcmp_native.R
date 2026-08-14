# Defining the model and parameter: the g-computation estimand.
# Sources: van der Laan, M. J. & Rose, S. (2018) *Targeted
# Learning in Data Science*, Springer, doi:10.1007/978-3-319-65304-4.
# Chap. 2 (the causal model for longitudinal data; the causal
# target parameter Psi^F(P_{U,X}) defined by counterfactual means
# under an intervention rule; identification of that quantity as
# a function of the observed data distribution by the
# g-computation estimand under sequential randomization and
# positivity; and the point that under those assumptions the
# estimand equals the causal quantity, but either way it has a
# valid statistical interpretation). Chap. 4 (the g-computation
# formula in the running longitudinal example). Robins, J. M.
# (1986) "A new approach to causal inference in mortality studies
# with a sustained exposure period", Mathematical Modelling 7(9-12),
# 1393-1512, doi:10.1016/0270-0255(86)90088-6. The g-computation
# formula. Pearl, J. (2009) *Causality: Models, Reasoning, and
# Inference*, 2nd edition, Cambridge University Press,
# doi:10.1017/CBO9780511803161. Causal graphs and the backdoor
# criterion, discussed in Chap. 2 as an alternative route to
# identifiability.
#
# Native implementation mirroring Python morie.fn.tlgcmp exactly:
# the same positivity check with the worst-propensity anchor, the
# same point-treatment g-formula, the same backward recursion
# for the longitudinal case, the same stratified counterfactual
# mean, and the same assumption-reporting.

morie_tlgcmp <- function(strata = NULL, outcome_means = NULL,
                         covariate_probs = NULL,
                         Q_functions = NULL, L_supports = NULL,
                         L_probs = NULL, rule = NULL,
                         Y = NULL, A = NULL, L = NULL,
                         a_star = NULL, g = NULL, delta = 0.01,
                         mode = c("point", "sequential",
                                  "counterfactual", "positivity")) {
  mode <- match.arg(mode)
  if (mode == "positivity")
    return(positivity_check(g, delta))
  if (mode == "counterfactual")
    return(counterfactual_mean(Y, A, L, a_star))
  if (mode == "sequential")
    return(sequential_g_formula(Q_functions, L_supports, L_probs,
                                 rule))
  g_computation(strata, outcome_means, covariate_probs)
}

positivity_check <- function(g, delta = 0.01) {
  gg <- as.numeric(g)
  if (length(gg) == 0L)
    stop("tlgcmp: no propensity scores given")
  lo <- min(gg); hi <- max(gg)
  worst <- min(lo, 1 - hi)
  list(min_g = lo, max_g = hi, worst = worst,
       satisfied = worst > as.numeric(delta),
       delta = as.numeric(delta),
       note = "without positivity the outcome regression is asked to extrapolate into cells with no data")
}

g_computation <- function(strata, outcome_means, covariate_probs) {
  s <- as.list(strata)
  p <- as.numeric(covariate_probs)
  if (abs(sum(p) - 1) > 1e-9)
    stop(sprintf("tlgcmp: the covariate distribution must sum to 1, got %.9f",
                 sum(p)))
  q <- as.numeric(outcome_means)
  sum(p * q)
}

sequential_g_formula <- function(Q_functions, L_supports, L_probs,
                                 rule) {
  T <- length(L_supports)
  if (length(L_probs) != T)
    stop(sprintf("tlgcmp: %d covariate supports but %d distributions",
                 T, length(L_probs)))

  walk <- function(t, hist) {
    if (t == T)
      return(Q_functions[[T + 1L]](hist))
    probs <- as.numeric(L_probs[[t + 1L]](hist))
    if (abs(sum(probs) - 1) > 1e-9)
      stop(sprintf("tlgcmp: the conditional law at time %d sums to %.9f",
                   t, sum(probs)))
    tot <- 0
    for (j in seq_along(L_supports[[t + 1L]])) {
      l <- L_supports[[t + 1L]][j]
      a <- rule(c(hist, l))
      tot <- tot + probs[j] * walk(t + 1L, c(hist, l, a))
    }
    tot
  }

  val <- walk(0L, numeric(0))
  list(estimate = val, psi = val, horizon = T,
       method = "sequential g-computation; van der Laan & Rose (2018) Chaps. 2 and 4",
       note = "the treatment mechanism does not appear -- the intervention replaces it",
       assumptions = c("sequential randomization (no unmeasured confounding) and positivity"))
}

counterfactual_mean <- function(Y, A, L, a_star, strata_probs = NULL) {
  y <- as.numeric(Y); a <- as.numeric(A); l <- as.character(L)
  if (!(length(y) == length(a) && length(a) == length(l)))
    stop("tlgcmp: the inputs differ in length")
  levels <- sort(unique(l))
  if (is.null(strata_probs)) {
    probs <- table(l) / length(l)
    strata_probs <- as.list(probs)
    names(strata_probs) <- names(probs)
  }
  tot <- 0
  for (v in levels) {
    idx <- which(l == v & a == as.numeric(a_star))
    if (length(idx) == 0L)
      stop(sprintf("tlgcmp: stratum %s contains no unit with A = %s -- a positivity violation, not a missing value",
                   v, as.numeric(a_star)))
    p <- if (is.null(names(strata_probs))) strata_probs[[v]]
         else strata_probs[[v]]
    if (is.null(p)) p <- mean(l == v)
    tot <- tot + p * mean(y[idx])
  }
  tot
}

cheatsheet <- function() {
  paste("tlgcmp: the causal parameter lives on the FULL data ",
        "(U, X); identification maps it to a functional of the ",
        "OBSERVED data. The g-computation formula integrates the ",
        "outcome regression over the covariate law with treatment ",
        "held FIXED, so the treatment mechanism disappears -- the ",
        "intervention replaced it. Two assumptions doing different ",
        "jobs: sequential randomization (no unmeasured ",
        "confounding) and positivity (every history keeps positive ",
        "probability). Break positivity and the regression is ",
        "asked to extrapolate into empty cells. Either way ",
        "Psi(P) remains a valid STATISTICAL parameter.", sep = "")
}
