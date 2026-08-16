# morie.fn -- function file (rootcoder007/morie)
# Online super learning.
#
# The data arrive sequentially: O(t) is drawn from a conditional
# distribution given a fixed-dimensional summary measure Z(t) of the
# past, and that conditional law is described by one common parameter
# theta across time. With an empty summary measure it is the ordinary
# i.i.d. case; with a parametric conditional density it is a classical
# time series model; and it also covers group sequential adaptive
# designs where the randomisation depends on data from earlier groups.
#
# Cross-validation has to respect time. V-fold splitting would train
# on the future to predict the past. The online analogue is
# sequential validation: at each t, train on O(1),...,O(t-1) and score
# the one-step-ahead prediction of O(t). Summed over t, that is an
# honest risk estimate for a sequentially-generated sample, and it is
# the quantity the weights are chosen to minimise.
#
# The oracle property survives the dependence: because the loss is
# evaluated on a genuinely held-out future observation at every step,
# the online super learner performs asymptotically as well as the best
# candidate in the library. The weights are refitted as data arrive;
# the cumulative losses are sufficient, so nothing needs re-scoring
# and memory does not grow with t.
#
# References
# ----------
# van der Laan, M. J. & Rose, S. (2018) Targeted Learning in Data
# Science, Springer, doi:10.1007/978-3-319-65304-4. Chap. 18 (van der
# Laan & Benkeser).
#
# Benkeser, D., Ju, C., Lendle, S. & van der Laan, M. J. (2018)
# "Online cross-validation-based ensemble learning", Statistics in
# Medicine 37(2), 249-260, doi:10.1002/sim.7320.
#
# van der Laan, M. J., Polley, E. C. & Hubbard, A. E. (2007) "Super
# Learner", Statistical Applications in Genetics and Molecular
# Biology 6(1), Article 25, doi:10.2202/1544-6115.1309.

.tlonsl_EPS <- 1e-12
.tlonsl_LOSSES <- c("squared", "log")

#' .tlonsl_loss
#'
#' Part of the tlonsl_native implementation; see the file header for the
#' source it follows.
#'
#' @param kind See Usage.
#' @param y See Usage.
#' @param p See Usage.
#' @return A numeric value.
#' @export
.tlonsl_loss <- function(kind, y, p) {
  if (kind == "squared") {
    return((y - p) ^ 2)
  }
  q <- min(max(p, .tlonsl_EPS), 1.0 - .tlonsl_EPS)
  -(y * log(q) + (1.0 - y) * log(1.0 - q))
}

#' A fixed-dimensional summary Z of the past. lags=0 gives the empty
#'
#' summary, which is exactly the i.i.d. case.
#'
#' @param history See Usage.
#' @param lags Defaults to \code{1}.
#' @return The value of \code{z}, as built in the body.
#' @export
morie_tlonsl_summary_measure <- function(history, lags=1) {
  # A fixed-dimensional summary Z of the past. lags=0 gives the empty
  # summary, which is exactly the i.i.d. case.
  L <- as.integer(lags)
  if (L < 0) {
    stop("tlonsl: lags must be non-negative")
  }
  if (L == 0L) {
    return(numeric(0))
  }
  h <- as.numeric(history)
  if (length(h) >= L) {
    z <- h[seq.int(length(h) - L + 1L, length(h))]
  } else {
    z <- c(rep(0.0, L - length(h)), h)
  }
  z
}

#' morie_tlonsl_sequential_risk
#'
#' Part of the tlonsl_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param algorithm See Usage.
#' @param loss Defaults to \code{"squared"}.
#' @param burn_in Defaults to \code{5}.
#' @param lags Defaults to \code{1}.
#' @return A list with \code{risk}, \code{predictions}, \code{losses}, \code{n_scored}, \code{note}.
#' @export
morie_tlonsl_sequential_risk <- function(y, algorithm, loss="squared",
                                         burn_in=5, lags=1) {
  # Train on the past, score the one-step-ahead prediction. V-fold
  # cross-validation would train on the future; this is the honest
  # analogue for a sequentially generated sample.
  if (!(loss %in% .tlonsl_LOSSES)) {
    stop(sprintf("tlonsl: loss must be one of %s, got %s",
                 paste(.tlonsl_LOSSES, collapse=", "), loss))
  }
  v <- as.numeric(y)
  b <- as.integer(burn_in)
  if (b < 1L || b >= length(v)) {
    stop(sprintf("tlonsl: burn_in must lie in 1..%d, got %d",
                 length(v) - 1L, b))
  }
  tot <- 0.0
  preds <- numeric(0)
  losses <- numeric(0)
  for (t in seq.int(b + 1L, length(v))) {
    fit <- algorithm(v[seq_len(t - 1L)])
    z <- morie_tlonsl_summary_measure(v[seq_len(t - 1L)], lags)
    p <- as.numeric(fit(z))
    preds <- c(preds, p)
    l <- .tlonsl_loss(loss, v[t], p)
    losses <- c(losses, l)
    tot <- tot + l
  }
  list(risk=tot / length(losses), predictions=preds,
       losses=losses, n_scored=length(losses),
       note=paste0("each prediction is scored on a genuinely ",
                   "held-out FUTURE observation"))
}

#' Exponentially weighted update from cumulative losses. The
#'
#' cumulative losses are sufficient, so the update is O(1) in memory.
#'
#' @param cum_losses See Usage.
#' @param eta Defaults to \code{1}.
#' @return A numeric value.
#' @export
morie_tlonsl_update_weights <- function(cum_losses, eta=1.0) {
  # Exponentially weighted update from cumulative losses. The
  # cumulative losses are sufficient, so the update is O(1) in memory.
  cl <- as.numeric(cum_losses)
  if (length(cl) == 0L) {
    stop("tlonsl: no cumulative losses given")
  }
  m <- min(cl)
  e <- exp(-as.numeric(eta) * (cl - m))
  e / sum(e)
}

#' morie_tlonsl_online_super_learner
#'
#' Part of the tlonsl_native implementation; see the file header for the
#' source it follows.
#'
#' @param y See Usage.
#' @param library See Usage.
#' @param loss Defaults to \code{"squared"}.
#' @param burn_in Defaults to \code{5}.
#' @param lags Defaults to \code{1}.
#' @param eta Defaults to \code{1}.
#' @return A list with \code{estimate}, \code{weights}, \code{risk}, \code{member_risks}, \code{best_single}, \code{best_member}, \code{weight_path}, \code{n_scored}, \code{method}, \code{note}.
#' @export
morie_tlonsl_online_super_learner <- function(y, library, loss="squared",
                                              burn_in=5, lags=1, eta=1.0) {
  # Sequentially-validated ensemble over a library. Weights are
  # updated as data arrive; the reported risk is the honest
  # one-step-ahead risk of the ensemble.
  if (length(library) == 0L) {
    stop("tlonsl: the library is empty")
  }
  v <- as.numeric(y)
  nms <- sort(names(library))
  per <- list()
  for (n in nms) {
    per[[n]] <- morie_tlonsl_sequential_risk(v, library[[n]], loss,
                                             burn_in, lags)
  }
  T_ <- per[[nms[1L]]][["n_scored"]]
  cum <- stats::setNames(rep(0.0, length(nms)), nms)
  ens_loss <- 0.0
  weight_path <- list()
  b <- as.integer(burn_in)
  for (s in seq_len(T_)) {
    w <- morie_tlonsl_update_weights(unname(cum[nms]), eta)
    weight_path[[s]] <- as.list(stats::setNames(w, nms))
    p <- 0.0
    for (j in seq_along(nms)) {
      p <- p + w[j] * per[[nms[j]]][["predictions"]][s]
    }
    ens_loss <- ens_loss + .tlonsl_loss(loss, v[b + s], p)
    for (n in nms) {
      cum[[n]] <- cum[[n]] + per[[n]][["losses"]][s]
    }
  }
  member_risks <- lapply(stats::setNames(nms, nms),
                         function(n) per[[n]][["risk"]])
  best <- nms[which.min(unlist(member_risks))]
  list(
    estimate=weight_path[[T_]], weights=weight_path[[T_]],
    risk=ens_loss / T_, member_risks=member_risks,
    best_single=per[[best]][["risk"]], best_member=best,
    weight_path=weight_path, n_scored=T_,
    method=paste0("online super learner with sequential validation; ",
                  "van der Laan & Rose (2018) Chap. 18"),
    note=paste0("an EMPTY summary measure recovers the i.i.d. case; ",
                "a parametric conditional density recovers a ",
                "classical time series model")
  )
}

#' morie_tlonsl_cheatsheet
#'
#' Part of the tlonsl_native implementation; see the file header for the
#' source it follows.
#'
#' @return A character value.
#' @export
morie_tlonsl_cheatsheet <- function() {
  paste0(
    "tlonsl: data arrive sequentially, O(t) given a ",
    "FIXED-DIMENSIONAL summary of the past, one common ",
    "parameter across time -- empty summary = i.i.d., ",
    "parametric conditional density = classical time series, ",
    "and group sequential adaptive designs are covered too. ",
    "V-fold CV would train on the FUTURE; use SEQUENTIAL ",
    "validation instead: train on 1..t-1, score the ",
    "one-step-ahead prediction of t. The oracle property ",
    "survives because every loss is evaluated on a held-out ",
    "future point. Cumulative losses are sufficient, so the ",
    "weight update is O(1) in memory."
  )
}

# compact alias per ledger/NAMING.md
morie_tlonsl_onlinesuperlearner <- morie_tlonsl_online_super_learner

#' @export
morie_tlonsl <- morie_tlonsl_online_super_learner
