# SPDX-License-Identifier: AGPL-3.0-or-later
#' Joint loss for multi-output DNN with mixed outcome types
#'
#' Montesinos Lopez, Montesinos Lopez and Crossa (2022), Multivariate
#' Statistical Machine Learning Methods for Genomic Prediction, Springer, read
#' as rendered pages.  Volume \[Pages 477-532\], Chapter 12, Section 12.4,
#' pp. 490-493: the multi-trait network is compiled with a per-trait loss and
#' per-trait loss_weights, "we need to specify a loss function and metrics for
#' each trait (outcome), that need to be in agreement with the type of
#' response of each trait", and the joint objective is the weighted sum of
#' them.  Section 12.4.5, p. 504, is the mixed-outcome case.  The chapter also
#' prescribes how to set the weights when the traits are on different scales:
#' "(1) first we calculated the median of each trait, (2) then we calculated
#' the 0.25 and 0.75 quantiles for each trait, (3) then we calculated the
#' maximum distance in terms of absolute value between the median and both
#' quantiles, (4) then we used as the weight for the first trait its
#' calculated distance, and (5) then we used as weight for the second trait
#' the value obtained by dividing the distance of the first trait by the
#' distance of the second trait", which is what weights = NULL reproduces.
#'
#' Volume \[Pages 427-476\], Chapter 11, Section 11.1.3, p. 428, fixes which
#' loss goes with which outcome: sum of squares for continuous, logistic
#' (cross-entropy) for binary, categorical cross-entropy for categorical and
#' ordinal, Poisson (or negative binomial) for counts.  Volume \[Pages
#' 379-425], Section 10.7.2, p. 401, writes the Poisson loss as
#' L = sum_ij (yhat_ij - y_ij log yhat_ij).
#'
#' @param y_dict named list, outcome name -> list(kind, observed vector), kind
#'   one of "cont", "binary", "count", "ordinal".
#' @param y_hat_dict named list, outcome name -> predicted vector.
#' @param weights optional named list or numeric vector of loss_weights.  When
#'   absent the Section 12.4 recipe is used.
#' @return list: estimate, loss, parts, weights, n, method.
#' @keywords internal
#' @examples
#' Jntlo(list(a = list("cont", c(1, 2, 3))), list(a = c(1.1, 1.9, 3.2)), c(1))$loss
#' @export
Jntlo <- function(y_dict, y_hat_dict, weights = NULL) {
  if (length(y_dict) == 0L) stop("joint_loss_mixed_outcomes: no outcomes supplied")
  nms <- names(y_dict)
  types <- c("cont", "binary", "count", "ordinal")
  kinds <- character(length(nms))
  ys <- vector("list", length(nms))
  for (j in seq_along(nms)) {
    nm <- nms[j]
    kd <- y_dict[[nm]][[1L]]
    if (!(kd %in% types)) {
      stop(sprintf("joint_loss_mixed_outcomes: unknown outcome type %s", kd))
    }
    kinds[j] <- kd
    ys[[j]] <- .s03vec(y_dict[[nm]][[2L]])
    if (!(nm %in% names(y_hat_dict))) {
      stop(sprintf("joint_loss_mixed_outcomes: no prediction for outcome %s", nm))
    }
    if (length(.s03vec(y_hat_dict[[nm]])) != length(ys[[j]])) {
      stop(sprintf("joint_loss_mixed_outcomes: outcome %s has mismatched lengths", nm))
    }
    if (length(ys[[j]]) == 0L) {
      stop(sprintf("joint_loss_mixed_outcomes: outcome %s is empty", nm))
    }
  }
  if (is.null(weights)) {
    d <- numeric(length(nms))
    for (j in seq_along(nms)) {
      med <- .s03median(ys[[j]])
      q1 <- .s03quantile7(ys[[j]], 0.25)
      q3 <- .s03quantile7(ys[[j]], 0.75)
      d[j] <- max(abs(med - q1), abs(med - q3))
    }
    w <- numeric(length(nms))
    w[1L] <- d[1L]
    if (length(nms) > 1L) {
      for (j in seq(2L, length(nms))) w[j] <- if (d[j] > 0) d[1L] / d[j] else 0
    }
  } else if (!is.null(names(weights))) {
    w <- vapply(nms, function(nm) as.numeric(weights[[nm]]), 0)
  } else {
    wv <- .s03vec(weights)
    if (length(wv) != length(nms)) {
      stop("joint_loss_mixed_outcomes: one weight per outcome is required")
    }
    w <- wv
  }
  parts <- numeric(length(nms))
  tot <- 0
  for (j in seq_along(nms)) {
    L <- .jntlo_loss(kinds[j], ys[[j]], .s03vec(y_hat_dict[[nms[j]]]))
    parts[j] <- L
    tot <- tot + w[j] * L
  }
  names(parts) <- nms
  names(w) <- nms
  list(estimate = tot, loss = tot, parts = parts, weights = w, n = length(nms),
       method = paste0("L = sum_t w_t L_t with the Chapter 11 Sect. 11.1.3 ",
                       "loss per outcome type, weights per Chapter 12 Sect. 12.4"))
}

.jntlo_loss <- function(kind, y, yh) {
  n <- length(y)
  s <- 0
  if (kind == "cont") {
    for (i in seq_len(n)) s <- s + (y[i] - yh[i])^2
    return(s / n)
  }
  if (kind == "binary") {
    for (i in seq_len(n)) {
      if (y[i] != 0 && y[i] != 1) {
        stop("joint_loss_mixed_outcomes: binary targets must be 0 or 1")
      }
      p <- min(max(yh[i], 1e-15), 1 - 1e-15)
      s <- s - (y[i] * log(p) + (1 - y[i]) * log(1 - p))
    }
    return(s / n)
  }
  if (kind == "count") {
    for (i in seq_len(n)) {
      if (yh[i] <= 0) stop("joint_loss_mixed_outcomes: count predictions must be positive")
      s <- s + yh[i] - y[i] * log(yh[i])
    }
    return(s / n)
  }
  if (kind == "ordinal") {
    for (i in seq_len(n)) {
      p <- min(max(yh[i], 1e-15), 1)
      s <- s - y[i] * log(p)
    }
    return(s / n)
  }
  stop(sprintf("joint_loss_mixed_outcomes: unknown outcome type %s", kind))
}
