# morie.fn -- function file (rootcoder007/morie)
# Transporting a treatment effect from one cohort to another.
#
# An effect estimated in one population is a statement about THAT
# population. Moving it to another is licensed only when the two
# differ in ways you have measured, and the correction is a
# reweighting.
#
# The identity: if the source cohort S=1 and target cohort S=0 share
# the conditional effect tau(x) = E[Y(1)-Y(0)|X=x] but differ in the
# distribution of X, then
#   tau_target = E_P0[tau(X)] = E_P1[ (p0(X)/p1(X)) tau(X) ],
# and with pi(x) = P(S=1|X=x),
#   p0(x)/p1(x) = [P(S=0)/P(S=1)] (1-pi(x))/pi(x),
# a logistic regression away.
#
# Overlap is the binding assumption and it is checkable: outside the
# source's support the weight is infinite and the effect is not
# identified, so transport_weights refuses membership probabilities at
# the boundary and reports the largest weight and effective sample
# size (sum w)^2 / sum w^2.
#
# Four routes: "ipw" reweights; "outcome" fits tau_hat(x) in the
# source and averages over the target; "dr" combines them (consistent
# if either piece is right, the default); "balance" solves for
# non-negative w minimising sum w_i^2 subject to sum w_i X_i =
# Xbar_target, balancing the named moments exactly even under
# misspecification.
#
# transfer_msm multiplies the transport weights by the MSM's IPW
# weights and fits the MSM by weighted least squares.
#
# References
# ----------
# Wager, S. (2025) Causal Inference: A Statistical Learning Approach,
# Stanford University, draft of 26 November 2025. Chapters 2 (Sec.
# 2.2), 3, and 7 (Secs. 7.1-7.2).
#
# Notes: the ledger recorded this as "Athey-Wager (2019),
# transfer-learning MSM across cohorts"; no paper of that title was
# located and the implementation follows Wager (2025).

.trnsfr_EPS <- 1e-12
.trnsfr_METHODS <- c("dr", "ipw", "outcome", "balance")

#' .trnsfr_cohort
#'
#' Part of the trnsfr_native implementation; see the file header for the
#' source it follows.
#'
#' @param S See Usage.
#' @return The value of \code{s}, as built in the body.
#' @export
.trnsfr_cohort <- function(S) {
  s <- .s03vec(S)
  if (any(!(s %in% c(0.0, 1.0)))) {
    stop("trnsfr: the cohort indicator must be 0/1 (1 = source)")
  }
  if (sum(s) < 2 || length(s) - sum(s) < 2) {
    stop(sprintf(paste0("trnsfr: both cohorts need at least 2 units ",
                        "(source %d, target %d)"),
                 as.integer(sum(s)), as.integer(length(s) - sum(s))))
  }
  s
}

#' Weighted least squares with an intercept prepended. rows is a
#'
#' matrix (or vector) of predictors; returns list(coef=...) with the
#' intercept first.
#'
#' @param rows See Usage.
#' @param y See Usage.
#' @param w See Usage.
#' @return A list with \code{coef}.
#' @export
.trnsfr_wls <- function(rows, y, w) {
  # Weighted least squares with an intercept prepended. rows is a
  # matrix (or vector) of predictors; returns list(coef=...) with the
  # intercept first.
  X <- cbind(1, as.matrix(rows))
  storage.mode(X) <- "double"
  p <- ncol(X)
  XtWX <- matrix(0.0, p, p)
  XtWy <- numeric(p)
  for (i in seq_len(nrow(X))) {
    xi <- X[i, ]
    XtWX <- XtWX + w[i] * outer(xi, xi)
    XtWy <- XtWy + w[i] * y[i] * xi
  }
  list(coef=.s03ridgesolve(XtWX, XtWy, 1e-10))
}

#' Odds-of-membership weights for the source cohort. Fits pi(x) by
#'
#' logistic regression on the pooled data and returns, for each source
#' unit, (1-pi)/pi, normalised to mean 1. Target units get 0.
#'
#' @param X See Usage.
#' @param S See Usage.
#' @param trim Defaults to \code{0.001}.
#' @param ridge Defaults to \code{1e-06}.
#' @return A list with \code{weights}, \code{pi}, \code{max_weight}, \code{ess}, \code{ess_fraction}, \code{n_source}, \code{coef}, \code{method}.
#' @export
morie_trnsfr_transport_weights <- function(X, S, trim=1e-3, ridge=1e-6) {
  # Odds-of-membership weights for the source cohort. Fits pi(x) by
  # logistic regression on the pooled data and returns, for each
  # source unit, (1-pi)/pi, normalised to mean 1. Target units get 0.
  Xm <- .s03mat(X)
  s <- .trnsfr_cohort(S)
  n <- nrow(Xm)
  if (length(s) != n) {
    stop(sprintf("trnsfr: %d cohort labels for %d rows", length(s), n))
  }
  D <- .s03design(Xm, n)
  beta <- .s03logit(D, s, ridge=ridge)
  pi <- vapply(seq_len(n),
               function(i) .s03sigmoid(sum(D[i, ] * beta)), numeric(1))
  lo <- as.numeric(trim)
  hi <- 1.0 - as.numeric(trim)
  bad <- which(!(pi >= lo & pi <= hi))
  if (length(bad) > 0L) {
    stop(sprintf(paste0("trnsfr: %d unit(s) have a cohort-membership ",
                        "probability outside [%g, %g] -- there is no ",
                        "overlap there and the transported effect is ",
                        "not identified for them"), length(bad), lo, hi))
  }
  raw <- ifelse(s == 1.0, (1.0 - pi) / pi, 0.0)
  tot <- sum(raw)
  if (tot <= .trnsfr_EPS) {
    stop("trnsfr: the transport weights are all zero")
  }
  ns <- as.integer(sum(s))
  w <- raw * ns / tot
  ess <- (sum(w) ^ 2) / sum(w * w)
  list(weights=w, pi=pi, max_weight=max(w), ess=ess,
       ess_fraction=ess / ns, n_source=ns, coef=beta,
       method=paste0("odds of cohort membership; Wager (2025) Sec. ",
                     "2.2 applied to S rather than W"))
}

#' Minimum-variance weights that match the target\'s X means: solve
#'
#' min sum w_i^2 over source units subject to sum w_i Xtilde_i =
#' Xbar_target with Xtilde = (1, X). Weights may go negative, which is
#' reported.
#'
#' @param X See Usage.
#' @param S See Usage.
#' @param ridge Defaults to \code{1e-08}.
#' @return A list with \code{weights}, \code{target_moments}, \code{achieved}, \code{max_imbalance}, \code{n_negative}, \code{positive_mass}, \code{method}.
#' @export
morie_trnsfr_balancing_weights <- function(X, S, ridge=1e-8) {
  # Minimum-variance weights that match the target's X means: solve
  # min sum w_i^2 over source units subject to sum w_i Xtilde_i =
  # Xbar_target with Xtilde = (1, X). Weights may go negative, which
  # is reported.
  Xm <- .s03mat(X)
  s <- .trnsfr_cohort(S)
  n <- nrow(Xm)
  if (length(s) != n) {
    stop(sprintf("trnsfr: %d cohort labels for %d rows", length(s), n))
  }
  D <- .s03design(Xm, n)
  p <- ncol(D)
  src <- which(s == 1.0)
  tgt <- which(s == 0.0)
  if (length(src) < p) {
    stop(sprintf("trnsfr: %d source units cannot balance %d moments",
                 length(src), p))
  }
  b <- colSums(D[tgt, , drop=FALSE]) / length(tgt)
  Ds <- D[src, , drop=FALSE]
  G <- crossprod(Ds) + diag(ridge, p)
  lam <- .s03cholsolve(G, b)
  w <- numeric(n)
  w[src] <- as.numeric(Ds %*% lam)
  achieved <- as.numeric(crossprod(Ds, w[src]))
  err <- max(abs(achieved - b))
  pos <- sum(w[src][w[src] > 0.0])
  list(weights=w, target_moments=b, achieved=achieved,
       max_imbalance=err, n_negative=sum(w[src] < 0.0),
       positive_mass=pos,
       method=paste0("minimum-variance covariate balancing weights; ",
                     "Wager (2025) Sec. 7.1"))
}

#' morie_trnsfr_transport_ate
#'
#' Part of the trnsfr_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param W See Usage.
#' @param X See Usage.
#' @param S See Usage.
#' @param method Defaults to \code{"dr"}.
#' @param e Defaults to \code{NULL}.
#' @param trim Defaults to \code{0.001}.
#' @param ridge Defaults to \code{1e-06}.
#' @return A list with \code{estimate}, \code{source_ate}, \code{outcome_route}, \code{n_source}, \code{n_target}, \code{method}, \code{diagnostics}, \code{assumption}.
#' @export
morie_trnsfr_transport_ate <- function(Y, W, X, S, method="dr", e=NULL,
                                       trim=1e-3, ridge=1e-6) {
  # The source-cohort effect, transported to the target cohort.
  if (!(method %in% .trnsfr_METHODS)) {
    stop(sprintf("trnsfr: method must be one of %s, got %s",
                 paste(.trnsfr_METHODS, collapse=", "), method))
  }
  y <- .s03vec(Y)
  w <- .s03vec(W)
  Xm <- .s03mat(X)
  s <- .trnsfr_cohort(S)
  n <- length(y)
  if (length(w) != n || nrow(Xm) != n || length(s) != n) {
    stop("trnsfr: W, X and S must have one row per outcome")
  }
  if (any(!(w %in% c(0.0, 1.0)))) {
    stop("trnsfr: W must be 0/1")
  }
  src <- which(s == 1.0)
  tgt <- which(s == 0.0)
  if (!any(w[src] == 1.0) || !any(w[src] == 0.0)) {
    stop(paste0("trnsfr: the source cohort must contain both treated ",
                "and control units"))
  }
  ps <- if (is.null(e)) {
    rep(0.5, n)
  } else if (length(e) == 1L) {
    rep(as.numeric(e), n)
  } else {
    .s03vec(e)
  }
  if (any(!(ps > 0.0 & ps < 1.0))) {
    stop("trnsfr: the treatment propensity must lie strictly in (0, 1)")
  }
  # tau(x) fitted in the source by an interacted linear model
  Dx <- .s03design(Xm, n)
  p <- ncol(Dx)
  rows <- cbind(Dx[src, , drop=FALSE], w[src] * Dx[src, , drop=FALSE])
  beta <- .s03lstsq(rows, y[src], 1e-8)
  tau_hat <- function(i) sum(Dx[i, ] * beta[p + seq_len(p)])
  mu <- function(i, wv) {
    sum(Dx[i, ] * beta[seq_len(p)]) + wv * sum(Dx[i, ] * beta[p + seq_len(p)])
  }
  out_part <- sum(vapply(tgt, tau_hat, numeric(1))) / length(tgt)
  if (method == "outcome") {
    est <- out_part
    diag_ <- list()
  } else {
    if (method == "balance") {
      wd <- morie_trnsfr_balancing_weights(Xm, s, ridge=1e-8)
    } else {
      wd <- morie_trnsfr_transport_weights(Xm, s, trim=trim, ridge=ridge)
    }
    tw <- wd$weights
    norm_ <- sum(tw[src])
    if (abs(norm_) <= .trnsfr_EPS) {
      stop("trnsfr: the transport weights sum to 0")
    }
    if (method == "ipw" || method == "balance") {
      # Hajek (self-normalised) form: each arm is divided by the
      # weight it actually received, not by a common total.
      n1 <- sum(tw[src] * w[src] / ps[src])
      n0 <- sum(tw[src] * (1.0 - w[src]) / (1.0 - ps[src]))
      if (abs(n1) <= .trnsfr_EPS || abs(n0) <= .trnsfr_EPS) {
        stop(sprintf(paste0("trnsfr: one treatment arm carries no ",
                            "transport weight (treated %.3g, control ",
                            "%.3g)"), n1, n0))
      }
      est <- (sum(tw[src] * w[src] * y[src] / ps[src]) / n1 -
              sum(tw[src] * (1.0 - w[src]) * y[src] / (1.0 - ps[src])) / n0)
    } else {  # dr
      num <- sum(vapply(src, function(i) {
        tw[i] * (mu(i, 1.0) - mu(i, 0.0) +
                 w[i] * (y[i] - mu(i, 1.0)) / ps[i] -
                 (1.0 - w[i]) * (y[i] - mu(i, 0.0)) / (1.0 - ps[i]))
      }, numeric(1)))
      est <- num / norm_
    }
    diag_ <- wd[setdiff(names(wd), "weights")]
  }
  naive <- (sum(y[src] * w[src]) / max(sum(w[src]), .trnsfr_EPS) -
            sum(y[src] * (1.0 - w[src])) / max(sum(1.0 - w[src]),
                                               .trnsfr_EPS))
  list(estimate=est, source_ate=naive, outcome_route=out_part,
       n_source=length(src), n_target=length(tgt), method=method,
       diagnostics=diag_,
       assumption=paste0("the conditional effect function is shared ",
                         "across cohorts and the target's covariate ",
                         "support lies inside the source's"))
}

#' morie_trnsfr_transfer_msm
#'
#' Part of the trnsfr_native implementation; see the file header for the
#' source it follows.
#'
#' @param Y See Usage.
#' @param A See Usage.
#' @param H See Usage.
#' @param cohort See Usage.
#' @param target Defaults to \code{0}.
#' @param e Defaults to \code{NULL}.
#' @param trim Defaults to \code{0.001}.
#' @param ridge Defaults to \code{1e-06}.
#' @return A list with \code{estimate}, \code{intercept}, \code{coef}, \code{weights}, \code{transport_weights}, \code{msm_weights}, \code{target}, \code{cohorts}, \code{n}, \code{method}.
#' @export
morie_trnsfr_transfer_msm <- function(Y, A, H, cohort, target=0, e=NULL,
                                      trim=1e-3, ridge=1e-6) {
  # A marginal structural model fitted with transported weights. Every
  # unit contributes w_transport_i * w_MSM_i, so the MSM coefficient
  # is the one that would have been obtained had the source cohort had
  # the target's covariate distribution.
  y <- .s03vec(Y)
  a <- .s03vec(A)
  Hm <- .s03mat(H)
  lab <- as.character(cohort)
  n <- length(y)
  if (!(length(a) == n && nrow(Hm) == n && length(lab) == n)) {
    stop("trnsfr: Y, A, H and cohort must agree in length")
  }
  tgt <- as.character(target)
  if (!(tgt %in% lab)) {
    stop(sprintf("trnsfr: target cohort %s is not present; cohorts are %s",
                 tgt, paste(sort(unique(lab)), collapse=", ")))
  }
  S <- ifelse(lab == tgt, 0.0, 1.0)
  tw <- morie_trnsfr_transport_weights(Hm, S, trim=trim, ridge=ridge)$weights
  if (is.null(e)) {
    Dh <- .s03design(Hm, n)
    bh <- .s03logit(Dh, as.numeric(a > 0.0), ridge=ridge)
    ps <- vapply(seq_len(n),
                 function(i) .s03sigmoid(sum(Dh[i, ] * bh)), numeric(1))
  } else if (length(e) == 1L) {
    ps <- rep(as.numeric(e), n)
  } else {
    ps <- .s03vec(e)
  }
  if (any(!(ps > 0.0 & ps < 1.0))) {
    stop("trnsfr: the exposure propensity must lie strictly in (0, 1)")
  }
  msm_w <- ifelse(a > 0.0, 1.0 / ps, 1.0 / (1.0 - ps))
  tot <- tw * msm_w
  # Fit on the SOURCE cohort only: the target supplies covariates, not
  # outcomes.
  src <- which(S == 1.0)
  rows <- matrix(a[src], ncol=1L)
  ys <- y[src]
  ws <- tot[src]
  if (length(unique(rows[, 1L])) < 2L) {
    stop(paste0("trnsfr: the source cohort has no exposure variation, ",
                "so no MSM coefficient is identified"))
  }
  fit <- .trnsfr_wls(rows, ys, ws)
  list(estimate=fit$coef[2], intercept=fit$coef[1], coef=fit$coef,
       weights=tot, transport_weights=tw, msm_weights=msm_w,
       target=tgt, cohorts=sort(unique(lab)), n=n,
       method=paste0("MSM fitted under IPW weights multiplied by ",
                     "cohort-transport weights; Wager (2025) Secs. 2.2 ",
                     "and 7.1"))
}

#' morie_trnsfr_cheatsheet
#'
#' Part of the trnsfr_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_trnsfr_cheatsheet <- function() {
  paste0(
    "trnsfr: move an effect between cohorts by reweighting. ",
    "p0(x)/p1(x) = [P(S=0)/P(S=1)] (1-pi(x))/pi(x), so a ",
    "logistic model for COHORT membership gives the weights -- ",
    "no density ratio is modelled. Overlap binds: outside the ",
    "source's support nothing is identified, so extreme pi is ",
    "refused, not trimmed silently. Routes: ipw / outcome / dr ",
    "(default) / balance. Balancing weights match the named ",
    "moments EXACTLY even under misspecification; IPW does ",
    "not."
  )
}

# compact alias per ledger/NAMING.md
morie_trnsfr_transferlearningmsm <- morie_trnsfr_transfer_msm
morie_trnsfr_transfer_learning_msm <- morie_trnsfr_transfer_msm

#' @export
morie_trnsfr <- morie_trnsfr_transport_ate
