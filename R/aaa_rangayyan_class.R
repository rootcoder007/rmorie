# Rangayyan pattern classification and computer-aided diagnosis: the
# accuracy measures, the separability measures, and the classifiers of
# Chapter 10.  Mirror of the Python bsaclass chunks 1 and 2.
#
# Chapter map for the 2024 third edition, from the PDF table of contents:
# chapter 9 is signal DECOMPOSITION and chapter 10 is Computer-aided
# Diagnosis and Healthcare, which is where these live.
#
# Equations verified in the PDF: 10.29, 10.68-10.73, 10.100-10.106,
# 10.112-10.117.
#
# BHATTACHARYYA appears nowhere in this book.  Rangayyan measures class
# separability with the normalized distance of eq (10.112) and the
# divergence of eqs (10.115)-(10.117).  GaussOverlap and ErrBound are
# because they are standard and correct, and are documented as not being
# from this text.

#' .morie_rg_gcd
#'
#' A step of the rangayyan_class implementation. Called by \code{.morie_rg_frac}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a Numeric; passed to \code{abs}.
#' @param b Numeric; passed to \code{abs}.
#' @return The value of \code{a}, as built in the body.
#' @export
.morie_rg_gcd <- function(a, b) {
  a <- abs(a)
  b <- abs(b)
  while (b > 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  a
}

#' An exact rational as a list, so counts-based ratios are not rounded
#'
#' A step of the rangayyan_class implementation. Called by \code{.morie_rg_asfrac}, \code{.morie_rg_fadd}, \code{.morie_rg_fmul} and 2 others in the module.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param n Numeric; combined arithmetically in the body.
#' @param d Numeric; combined arithmetically in the body.
#' @return The value of \code{structure}.
#' @export
.morie_rg_frac <- function(n, d) {
  # an exact rational as a list, so counts-based ratios are not rounded
  if (d == 0) stop("a rational cannot have a zero denominator")
  if (d < 0) {
    n <- -n
    d <- -d
  }
  g <- .morie_rg_gcd(n, d)
  if (g == 0) g <- 1
  structure(list(n = n / g, d = d / g), class = "morie_frac")
}

#' .morie_rg_asfrac
#'
#' A step of the rangayyan_class implementation. Called by \code{Accuracy}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param v See Usage.
#' @return Nothing; this branch always raises.
#' @export
.morie_rg_asfrac <- function(v) {
  if (inherits(v, "morie_frac")) {
    return(v)
  }
  s <- as.character(v)
  if (grepl("^-?[0-9]+$", s)) {
    return(.morie_rg_frac(as.numeric(s), 1))
  }
  if (grepl("^-?[0-9]*\\.[0-9]+$", s)) {
    dec <- sub("^-?[0-9]*\\.", "", s)
    p <- nchar(dec)
    return(.morie_rg_frac(as.numeric(s) * 10^p, 10^p))
  }
  stop(
    "cannot represent ", s, " exactly; give an integer, a decimal ",
    "string, or a morie_frac"
  )
}

#' .morie_rg_fadd
#'
#' A step of the rangayyan_class implementation. Called by \code{Accuracy}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a A list; the body reads \code{$d}, \code{$n} from it.
#' @param b A list; the body reads \code{$d}, \code{$n} from it.
#' @return The value of \code{.morie_rg_frac}.
#' @export
.morie_rg_fadd <- function(a, b) {
  .morie_rg_frac(a$n * b$d + b$n * a$d, a$d * b$d)
}
#' .morie_rg_fsub
#'
#' A step of the rangayyan_class implementation. Called by \code{Accuracy}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a A list; the body reads \code{$d}, \code{$n} from it.
#' @param b A list; the body reads \code{$d}, \code{$n} from it.
#' @return The value of \code{.morie_rg_frac}.
#' @export
.morie_rg_fsub <- function(a, b) {
  .morie_rg_frac(a$n * b$d - b$n * a$d, a$d * b$d)
}
#' .morie_rg_fmul
#'
#' A step of the rangayyan_class implementation. Called by \code{Accuracy}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param a A list; the body reads \code{$d}, \code{$n} from it.
#' @param b A list; the body reads \code{$d}, \code{$n} from it.
#' @return The value of \code{.morie_rg_frac}.
#' @export
.morie_rg_fmul <- function(a, b) .morie_rg_frac(a$n * b$n, a$d * b$d)

# as.numeric() dispatches to as.double methods, not as.numeric ones
#' As.numeric() dispatches to as.double methods, not as.numeric ones
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A list; the body reads \code{$d}, \code{$n} from it.
#' @param ... Passed through.
#' @return A numeric value.
#' @export
as.double.morie_frac <- function(x, ...) x$n / x$d
#' as.numeric.morie_frac
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A list; the body reads \code{$d}, \code{$n} from it.
#' @param ... Passed through.
#' @return A numeric value.
#' @export
as.numeric.morie_frac <- function(x, ...) x$n / x$d
#' format.morie_frac
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x A list; the body reads \code{$d}, \code{$n} from it.
#' @param ... Passed through.
#' @return A character value.
#' @export
format.morie_frac <- function(x, ...) paste0(x$n, "/", x$d)
#' print.morie_frac
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param x See Usage.
#' @param ... Passed through.
#' @return The value of \code{cat}.
#' @export
print.morie_frac <- function(x, ...) cat(format(x), "\n")
"==.morie_frac" <- function(e1, e2) {
  a <- .morie_rg_asfrac(e1)
  b <- .morie_rg_asfrac(e2)
  a$n == b$n && a$d == b$d
}

#' Sum of (x - mu)(x - mu)^T: the SCATTER, not divided by n
#'
#' A step of the rangayyan_class implementation. Called by \code{FishLda}, \code{SepIndex}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param mu See Usage.
#' @return The value of \code{%*%}.
#' @export
.morie_rg_scatter <- function(X, mu) {
  # sum of (x - mu)(x - mu)^T: the SCATTER, not divided by n
  d <- sweep(as.matrix(X), 2, mu, "-")
  t(d) %*% d
}

#' Split rows by label, preserving first-seen order
#'
#' A step of the rangayyan_class implementation. Called by \code{FishLda}, \code{Qda}, \code{SepIndex}.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; indexed by row and column.
#' @param y See Usage.
#' @return A list with \code{order}, \code{groups}.
#' @export
.morie_rg_groups <- function(X, y) {
  # split rows by label, preserving first-seen order
  order <- unique(y)
  list(
    order = order,
    groups = lapply(order, function(l) X[y == l, , drop = FALSE])
  )
}

#' Eq (10.100): S+ = TP / (subjects with the disease).  Measures the
#'
#' capability to DETECT and says nothing about false alarms -- a test
#' calling everyone positive scores 1, which is why the book always
#' reports it beside the specificity.
#'
#' @param tp A matrix; passed to \code{as.matrix}.
#' @param fn Defaults to \code{NULL}.
#' @return A list with \code{sensitivity}, \code{tpf}, \code{fnf}, \code{n_diseased}, \code{tp}, \code{fn}, \code{says_nothing_about_false_alarms}, \code{method}.
#' @export
Sens <- function(tp, fn = NULL) {
  # eq (10.100): S+ = TP / (subjects with the disease).  Measures the
  # capability to DETECT and says nothing about false alarms -- a test
  # calling everyone positive scores 1, which is why the book always
  # reports it beside the specificity.
  if (is.null(fn)) {
    t <- as.matrix(tp)
    if (nrow(t) != 2L || ncol(t) != 2L) {
      stop("give TP and FN, or a 2x2 table [[TP, FN], [FP, TN]]")
    }
    TP <- t[1, 1]
    FN <- t[1, 2]
  } else {
    TP <- as.numeric(tp)
    FN <- as.numeric(fn)
  }
  if (TP < 0 || FN < 0) stop("counts cannot be negative")
  n <- TP + FN
  if (n <= 0) {
    stop("no subjects with the disease; the sensitivity is undefined")
  }
  list(
    sensitivity = TP / n, tpf = TP / n, fnf = FN / n,
    n_diseased = n, tp = TP, fn = FN,
    says_nothing_about_false_alarms = TRUE,
    method = "Rangayyan (2024) eq. (10.100)"
  )
}

#' Eq (10.101): S- = TN / (subjects without the disease)
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param tn A matrix; passed to \code{as.matrix}.
#' @param fp Defaults to \code{NULL}.
#' @return A list with \code{specificity}, \code{tnf}, \code{fpf}, \code{n_healthy}, \code{tn}, \code{fp}, \code{method}.
#' @export
Spec <- function(tn, fp = NULL) {
  # eq (10.101): S- = TN / (subjects without the disease).
  if (is.null(fp)) {
    t <- as.matrix(tn)
    if (nrow(t) != 2L || ncol(t) != 2L) {
      stop("give TN and FP, or a 2x2 table [[TP, FN], [FP, TN]]")
    }
    TN <- t[2, 2]
    FP <- t[2, 1]
  } else {
    TN <- as.numeric(tn)
    FP <- as.numeric(fp)
  }
  if (TN < 0 || FP < 0) stop("counts cannot be negative")
  n <- TN + FP
  if (n <= 0) {
    stop("no subjects without the disease; the specificity is undefined")
  }
  list(
    specificity = TN / n, tnf = TN / n, fpf = FP / n,
    n_healthy = n, tn = TN, fp = FP,
    method = "Rangayyan (2024) eq. (10.101)"
  )
}

#' Ppv
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param tp A matrix; passed to \code{as.matrix}.
#' @param fp Defaults to \code{NULL}.
#' @param prevalence Defaults to \code{NULL}.
#' @param sensitivity Defaults to \code{NULL}.
#' @param specificity Defaults to \code{NULL}.
#' @return The value of \code{out}, as built in the body.
#' @export
Ppv <- function(tp, fp = NULL, prevalence = NULL, sensitivity = NULL,
                specificity = NULL) {
  # eq (10.106): PPV = TP / (TP + FP).  Unlike sensitivity and
  # specificity it depends on the PREVALENCE, so a PPV measured where
  # half the subjects are ill does not transfer to screening a
  # population where one in a thousand is.
  if (is.null(fp) && !is.numeric(tp)) {
    t <- as.matrix(tp)
    if (nrow(t) != 2L || ncol(t) != 2L) {
      stop("give TP and FP, or a 2x2 table [[TP, FN], [FP, TN]]")
    }
    TP <- t[1, 1]
    FP <- t[2, 1]
  } else {
    if (is.null(fp)) stop("give TP and FP, or a 2x2 table")
    TP <- as.numeric(tp)
    FP <- as.numeric(fp)
  }
  if (TP < 0 || FP < 0) stop("counts cannot be negative")
  n <- TP + FP
  if (n <= 0) stop("no positive decisions; the PPV is undefined")
  out <- list(
    ppv = TP / n, precision = TP / n, tp = TP, fp = FP,
    n_positive_calls = n, depends_on_prevalence = TRUE,
    method = "Rangayyan (2024) eq. (10.106)"
  )
  if (!is.null(prevalence)) {
    if (is.null(sensitivity) || is.null(specificity)) {
      stop(
        "a prevalence correction needs both the sensitivity and the ",
        "specificity"
      )
    }
    p <- as.numeric(prevalence)
    if (p < 0 || p > 1) stop("the prevalence must lie in [0, 1]")
    se <- as.numeric(sensitivity)
    sp <- as.numeric(specificity)
    den <- se * p + (1 - sp) * (1 - p)
    out$prevalence <- p
    out$ppv_at_prevalence <- if (den > 0) se * p / den else NULL
  }
  out
}

#' Accuracy
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param table Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @param tp Defaults to \code{NULL}.
#' @param tn Defaults to \code{NULL}.
#' @param fp Defaults to \code{NULL}.
#' @param fn Defaults to \code{NULL}.
#' @param prevalence Optional; may be \code{NULL}. Passed to \code{.morie_rg_asfrac}.
#' @param kind Defaults to \code{NULL}.
#' @param exact A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{accuracy}, \code{kind}, \code{raw_accuracy}, \code{weighted_accuracy}, \code{balanced_accuracy}, \code{sensitivity}, \code{specificity}, \code{prevalence}, \code{test_set_prevalence}, \code{counts}, \code{n}, \code{exact}, \code{prior_weighted}, \code{balanced_is_eq_10_102_at_one_half}, \code{eq_10_103_is_eq_10_102_at_the_test_set_prevalence}, \code{method}.
#' @export
Accuracy <- function(table = NULL, tp = NULL, tn = NULL, fp = NULL,
                     fn = NULL, prevalence = NULL, kind = NULL,
                     exact = FALSE) {
  # Every definition, not just one.  Eq (10.102), stated first, is
  # prevalence weighted:  accuracy = S+ P(A) + S- P(N).  Eq (10.103) is
  # the fallback used only "if the prior probabilities are not
  # available", and IS eq (10.102) at the prevalence of the TEST SET, so
  # on a set balanced 50/50 it describes a population that does not
  # exist.  The balanced form -- the unweighted mean of sensitivity and
  # specificity -- is eq (10.102) at a prevalence of one half.
  #
  # `exact` returns exact rationals: the inputs are integer counts, so
  # the raw accuracy is a RATIO OF INTEGERS and rounding it to a double
  # is a choice, not a necessity.
  if (!is.null(table)) {
    t <- as.matrix(table)
    if (nrow(t) != 2L || ncol(t) != 2L) {
      stop("the table must be 2x2, [[TP, FN], [FP, TN]]")
    }
    TP <- t[1, 1]
    FN <- t[1, 2]
    FP <- t[2, 1]
    TN <- t[2, 2]
  } else {
    if (is.null(tp) || is.null(tn) || is.null(fp) || is.null(fn)) {
      stop("give a 2x2 table or all four of tp, tn, fp, fn")
    }
    TP <- tp
    TN <- tn
    FP <- fp
    FN <- fn
  }
  cts <- c(TP, TN, FP, FN)
  if (any(cts < 0)) stop("counts cannot be negative")
  if (any(cts != round(cts))) stop("counts must be whole numbers")
  TP <- as.integer(TP)
  TN <- as.integer(TN)
  FP <- as.integer(FP)
  FN <- as.integer(FN)
  total <- TP + TN + FP + FN
  if (total <= 0L) stop("the table is empty")
  if (TP + FN <= 0L || TN + FP <= 0L) {
    stop("a class is empty; the sensitivity or specificity is undefined")
  }
  kinds <- c("raw", "weighted", "balanced")
  if (!is.null(kind) && !kind %in% kinds) {
    stop("kind must be one of ", paste(kinds, collapse = ", "))
  }

  if (exact) {
    se <- .morie_rg_frac(TP, TP + FN)
    sp <- .morie_rg_frac(TN, TN + FP)
    raw <- .morie_rg_frac(TP + TN, total)
    tprev <- .morie_rg_frac(TP + FN, total)
    balanced <- .morie_rg_fmul(
      .morie_rg_frac(1, 2),
      .morie_rg_fadd(se, sp)
    )
  } else {
    se <- TP / (TP + FN)
    sp <- TN / (TN + FP)
    raw <- (TP + TN) / total
    tprev <- (TP + FN) / total
    balanced <- 0.5 * (se + sp)
  }

  weighted <- NULL
  prev <- NULL
  if (!is.null(prevalence)) {
    if (exact) {
      prev <- .morie_rg_asfrac(prevalence)
      if (prev$n < 0 || prev$n > prev$d) {
        stop("the prevalence must lie in [0, 1]")
      }
      one <- .morie_rg_frac(1, 1)
      weighted <- .morie_rg_fadd(
        .morie_rg_fmul(se, prev),
        .morie_rg_fmul(sp, .morie_rg_fsub(one, prev))
      )
    } else {
      prev <- as.numeric(prevalence)
      if (prev < 0 || prev > 1) stop("the prevalence must lie in [0, 1]")
      weighted <- se * prev + sp * (1 - prev)
    }
  }

  chosen <- kind
  if (is.null(chosen)) {
    chosen <- if (!is.null(prevalence)) "weighted" else "raw"
  }
  if (chosen == "weighted" && is.null(weighted)) {
    stop(
      "kind='weighted' needs a prevalence; without the priors the ",
      "book falls back on eq. (10.103), kind='raw'"
    )
  }
  headline <- switch(chosen,
    raw = raw,
    weighted = weighted,
    balanced = balanced
  )

  list(
    accuracy = headline, kind = chosen, raw_accuracy = raw,
    weighted_accuracy = weighted, balanced_accuracy = balanced,
    sensitivity = se, specificity = sp, prevalence = prev,
    test_set_prevalence = tprev,
    counts = list(tp = TP, tn = TN, fp = FP, fn = FN),
    n = total, exact = isTRUE(exact),
    prior_weighted = chosen == "weighted",
    balanced_is_eq_10_102_at_one_half = TRUE,
    eq_10_103_is_eq_10_102_at_the_test_set_prevalence = TRUE,
    method = paste(
      "Rangayyan (2024) eqs. (10.102)-(10.103), with",
      "the balanced form at P(A) = 1/2"
    )
  )
}

#' Section 10.9.1.  The area the book calls A_z, by the trapezoidal
#'
#' rule, which for tied scores is exactly the Mann-Whitney statistic --
#' the probability a random diseased subject outscores a random healthy
#' one, ties counted as half.  Summing rectangles instead biases the
#' area whenever scores tie, which they do whenever a classifier emits a
#' class rather than a probability.
#'
#' @param scores See Usage.
#' @param labels A vector; its length is taken.
#' @param positive Defaults to \code{1}.
#' @return A list with \code{fpf}, \code{tpf}, \code{sensitivity}, \code{one_minus_specificity}, \code{thresholds}, \code{auc}, \code{az}, \code{mann_whitney}, \code{trapezoidal_equals_mann_whitney}, \code{n_positive}, \code{n_negative}, \code{best_index}, \code{best_operating_point}, \code{ties_counted_as_half}, \code{method}.
#' @export
Roc <- function(scores, labels, positive = 1) {
  # Section 10.9.1.  The area the book calls A_z, by the trapezoidal
  # rule, which for tied scores is exactly the Mann-Whitney statistic --
  # the probability a random diseased subject outscores a random healthy
  # one, ties counted as half.  Summing rectangles instead biases the
  # area whenever scores tie, which they do whenever a classifier emits
  # a class rather than a probability.
  s <- as.numeric(scores)
  if (length(s) != length(labels)) {
    stop("scores and labels must have the same length")
  }
  if (!length(s)) stop("need at least one observation")
  pos <- s[labels == positive]
  neg <- s[labels != positive]
  if (!length(pos) || !length(neg)) {
    stop("the ROC needs both classes present")
  }
  thr <- sort(unique(s), decreasing = TRUE)
  tpf <- c(0, vapply(thr, function(t) mean(pos >= t), numeric(1)))
  fpf <- c(0, vapply(thr, function(t) mean(neg >= t), numeric(1)))
  area <- sum(0.5 * (tpf[-1] + tpf[-length(tpf)]) * diff(fpf))
  wins <- sum(outer(pos, neg, function(a, b) {
    ifelse(a > b, 1, ifelse(a == b, 0.5, 0))
  }))
  mw <- wins / (length(pos) * length(neg))
  best <- which.min((1 - tpf)^2 + fpf^2)
  list(
    fpf = fpf, tpf = tpf, sensitivity = tpf,
    one_minus_specificity = fpf, thresholds = thr,
    auc = area, az = area, mann_whitney = mw,
    trapezoidal_equals_mann_whitney = abs(area - mw) < 1e-9,
    n_positive = length(pos), n_negative = length(neg),
    best_index = best - 1L,
    best_operating_point = c(fpf[best], tpf[best]),
    ties_counted_as_half = TRUE,
    method = "Rangayyan (2024) Section 10.9.1 (ROC, A_z)"
  )
}

#' Section 10.9.2, McNemar\'s test of SYMMETRY.  The book states it on a
#'
#' general contingency table -- its worked example, Table 10.4, is 3x3
#' with normal / indeterminate / abnormal -- so any k x k is accepted. k
#' = 2 gives McNemar with Yates\' correction, k > 2 its generalization,
#' Bowker\'s test.  Only the OFF-DIAGONAL disagreements enter: the
#' diagonal, usually most of the cases, contributes nothing, because the
#' question is whether the disagreements are one-sided.
#'
#' @param table A matrix; passed to \code{as.matrix}.
#' @param correct Optional; may be \code{NULL}. A flag; the body branches on it.
#' @return A list with \code{statistic}, \code{df}, \code{p_value}, \code{pairs}, \code{n}, \code{n_agree}, \code{continuity_correction}, \code{is_bowker}, \code{k}, \code{diagonal_contributes_nothing}, \code{method}.
#' @export
McNemar <- function(table, correct = NULL) {
  # Section 10.9.2, McNemar's test of SYMMETRY.  The book states it on a
  # general contingency table -- its worked example, Table 10.4, is 3x3
  # with normal / indeterminate / abnormal -- so any k x k is accepted.
  # k = 2 gives McNemar with Yates' correction, k > 2 its generalization,
  # Bowker's test.  Only the OFF-DIAGONAL disagreements enter: the
  # diagonal, usually most of the cases, contributes nothing, because the
  # question is whether the disagreements are one-sided.
  t <- as.matrix(table)
  k <- nrow(t)
  if (k < 2L || ncol(t) != k) stop("the table must be square and at least 2x2")
  if (any(t < 0)) stop("counts cannot be negative")
  yates <- if (is.null(correct)) (k == 2L) else isTRUE(correct)
  stat <- 0
  df <- 0L
  pairs <- list()
  for (i in seq_len(k)) {
    for (j in seq_len(k)) {
      if (j <= i) next
      a <- t[i, j]
      b <- t[j, i]
      if (a + b <= 0) next
      d <- abs(a - b)
      if (yates) d <- max(0, d - 1)
      stat <- stat + d * d / (a + b)
      df <- df + 1L
      pairs[[length(pairs) + 1L]] <- list(
        i = i - 1L, j = j - 1L,
        n_ij = a, n_ji = b
      )
    }
  }
  if (df == 0L) {
    stop(
      "the table is symmetric with no off-diagonal counts; the test ",
      "is undefined"
    )
  }
  list(
    statistic = stat, df = df,
    p_value = stats::pchisq(stat, df, lower.tail = FALSE),
    pairs = pairs, n = sum(t), n_agree = sum(diag(t)),
    continuity_correction = yates, is_bowker = k > 2L, k = k,
    diagonal_contributes_nothing = TRUE,
    method = paste(
      "Rangayyan (2024) Section 10.9.2 (McNemar's test",
      "of symmetry; Bowker's generalization for k > 2)"
    )
  )
}

#' Eq (10.112): d_n = |m1 - m2| / (sigma1 + sigma2).  The denominator is
#'
#' the SUM of the SDs, not their quadrature sum -- this is not the
#' Fisher criterion.  The book states the limitation: d_n = 0 whenever
#' m1 = m2, however different the dispersions, so classes separated only
#' by variance score zero.  The divergence has no such blind spot.
#'
#' @param m1 See Usage.
#' @param m2 See Usage.
#' @param s1 See Usage.
#' @param s2 See Usage.
#' @return A list with \code{dn}, \code{mean_difference}, \code{sd_sum}, \code{blind_to_variance_when_means_match}, \code{denominator_is_the_sum_not_the_quadrature_sum}, \code{method}.
#' @export
NormDist <- function(m1, m2, s1, s2) {
  # eq (10.112): d_n = |m1 - m2| / (sigma1 + sigma2).  The denominator is
  # the SUM of the SDs, not their quadrature sum -- this is not the
  # Fisher criterion.  The book states the limitation: d_n = 0 whenever
  # m1 = m2, however different the dispersions, so classes separated only
  # by variance score zero.  The divergence has no such blind spot.
  a <- as.numeric(m1)
  b <- as.numeric(m2)
  p <- as.numeric(s1)
  q <- as.numeric(s2)
  if (p < 0 || q < 0) stop("a standard deviation cannot be negative")
  if (p + q <= 0) {
    stop(
      "both standard deviations are zero; the normalized distance is ",
      "undefined"
    )
  }
  list(
    dn = abs(a - b) / (p + q), mean_difference = abs(a - b),
    sd_sum = p + q,
    blind_to_variance_when_means_match = abs(a - b) < 1e-300,
    denominator_is_the_sum_not_the_quadrature_sum = TRUE,
    method = "Rangayyan (2024) eq. (10.112)"
  )
}

#' Eq (10.117), the closed form of the symmetric divergence of
#'
#' eq (10.115): D = (1/2) tr[(Ci - Cj)(Cj^-1 - Ci^-1)] + (1/2) tr[(Ci^-1
#' + Cj^-1)(mi - mj)(mi - mj)^T] The second term resembles eq (10.112)
#' and vanishes for equal means; the FIRST does not, so unlike d_n the
#' divergence still separates classes differing only in covariance.
#' This is the measure the book uses; Bhattacharyya appears nowhere in
#' it.
#'
#' @param m1 See Usage.
#' @param m2 See Usage.
#' @param C1 A matrix; passed to \code{as.matrix}.
#' @param C2 A matrix; passed to \code{as.matrix}.
#' @return A list with \code{divergence}, \code{covariance_term}, \code{mean_term}, \code{nonnegative}, \code{symmetric}, \code{zero_for_identical_pdfs}, \code{separates_equal_means_via_the_covariance_term}, \code{additive_over_independent_features}, \code{method}.
#' @export
Divergence <- function(m1, m2, C1, C2) {
  # eq (10.117), the closed form of the symmetric divergence of
  # eq (10.115):
  #   D = (1/2) tr[(Ci - Cj)(Cj^-1 - Ci^-1)]
  #     + (1/2) tr[(Ci^-1 + Cj^-1)(mi - mj)(mi - mj)^T]
  # The second term resembles eq (10.112) and vanishes for equal means;
  # the FIRST does not, so unlike d_n the divergence still separates
  # classes differing only in covariance.  This is the measure the book
  # uses; Bhattacharyya appears nowhere in it.
  a <- as.numeric(m1)
  b <- as.numeric(m2)
  A <- as.matrix(C1)
  B <- as.matrix(C2)
  p <- length(a)
  if (length(b) != p) stop("the two mean vectors must have the same length")
  if (nrow(A) != p || ncol(A) != p || nrow(B) != p || ncol(B) != p) {
    stop("the covariance matrices must be ", p, " x ", p)
  }
  Ai <- solve(A)
  Bi <- solve(B)
  term1 <- 0.5 * sum(diag((A - B) %*% (Bi - Ai)))
  dm <- matrix(a - b, ncol = 1)
  term2 <- 0.5 * sum(diag((Ai + Bi) %*% (dm %*% t(dm))))
  D <- term1 + term2
  list(
    divergence = D, covariance_term = term1, mean_term = term2,
    nonnegative = D >= -1e-9, symmetric = TRUE,
    zero_for_identical_pdfs = abs(D) < 1e-9,
    separates_equal_means_via_the_covariance_term = abs(term1) > 1e-12,
    additive_over_independent_features = TRUE,
    method = "Rangayyan (2024) eqs. (10.115)-(10.117)"
  )
}

#' The book averages the pairwise divergences for a single measure over
#'
#' m classes.  Averaging hides a badly separated PAIR behind well
#' separated ones, so the minimum is returned too -- that is the pair
#' that will actually be confused.
#'
#' @param means A vector; its length is taken and its elements indexed.
#' @param covs A vector; its length is taken and its elements indexed.
#' @return A list with \code{average}, \code{pairwise}, \code{minimum}, \code{worst_pair}, \code{n_classes}, \code{n_pairs}, \code{average_hides_the_worst_pair}, \code{method}.
#' @export
DivAv <- function(means, covs) {
  # The book averages the pairwise divergences for a single measure over
  # m classes.  Averaging hides a badly separated PAIR behind well
  # separated ones, so the minimum is returned too -- that is the pair
  # that will actually be confused.
  m <- length(means)
  if (m < 2L) stop("need at least two classes")
  if (length(covs) != m) stop("give one covariance matrix per class")
  vals <- c()
  pairs <- list()
  for (i in seq_len(m)) {
    for (j in seq_len(m)) {
      if (j <= i) next
      d <- Divergence(means[[i]], means[[j]], covs[[i]], covs[[j]])$divergence
      vals <- c(vals, d)
      pairs[[length(pairs) + 1L]] <- c(i - 1L, j - 1L, d)
    }
  }
  w <- which.min(vapply(pairs, function(p) p[3], numeric(1)))
  list(
    average = mean(vals), pairwise = pairs,
    minimum = pairs[[w]][3],
    worst_pair = c(pairs[[w]][1], pairs[[w]][2]),
    n_classes = m, n_pairs = length(vals),
    average_hides_the_worst_pair = TRUE,
    method = "Rangayyan (2024) Section 10.10.1 (average divergence)"
  )
}

#' Eq (5.33): KLD(p1, p2) = sum_l p2(x_l) ln[p2(x_l) / p1(x_l)]
#'
#' NOTE THE ARGUMENT ORDER -- the book weights by the SECOND PDF, so its
#' KLD(p1, p2) is D_KL(p2 || p1) in standard notation, the REVERSE of
#' what most texts and libraries mean by KL(p, q).  That is the single
#' most likely way to get a wrong number out of this family. KLD is not
#' symmetric, so swapping the arguments gives a different number; both
#' are returned so the asymmetry is visible rather than a trap.  Their
#' sum is exactly the divergence of eq (10.115).
#'
#' @param p1 See Usage.
#' @param p2 See Usage.
#' @return A list with \code{kld}, \code{reversed}, \code{symmetric_sum}, \code{asymmetric}, \code{weighted_by_the_second_pdf}, \code{symmetric_sum_is_the_divergence_of_eq_10_115}, \code{nonnegative}, \code{method}.
#' @export
Kld <- function(p1, p2) {
  # eq (5.33): KLD(p1, p2) = sum_l p2(x_l) ln[p2(x_l) / p1(x_l)].
  # NOTE THE ARGUMENT ORDER -- the book weights by the SECOND PDF, so its
  # KLD(p1, p2) is D_KL(p2 || p1) in standard notation, the REVERSE of
  # what most texts and libraries mean by KL(p, q).  That is the single
  # most likely way to get a wrong number out of this family.
  # KLD is not symmetric, so swapping the arguments gives a different
  # number; both are returned so the asymmetry is visible rather than a
  # trap.  Their sum is exactly the divergence of eq (10.115).
  #
  # The book uses this as a FEATURE: Rangayyan and Wu computed the KLD
  # between a signal's PDF and Parzen-window models of the normal and
  # abnormal VAG classes, reaching 73 per cent with the KLD alone.
  a <- as.numeric(p1)
  b <- as.numeric(p2)
  if (length(a) != length(b)) {
    stop("the two PDFs must be sampled on the same grid")
  }
  if (!length(a)) stop("need at least one bin")
  if (any(a < 0) || any(b < 0)) stop("a PDF cannot be negative")
  bad <- sum(b > 0 & a <= 0)
  if (bad) {
    stop(
      "p1 vanishes at ", bad, " bin(s) where p2 does not; the KLD ",
      "is unbounded there"
    )
  }
  k <- b > 0
  fwd <- .morie_fsum(b[k] * log(b[k] / a[k]))
  j <- a > 0 & b > 0
  rev <- .morie_fsum(a[j] * log(a[j] / b[j]))
  list(
    kld = fwd, reversed = rev, symmetric_sum = fwd + rev,
    asymmetric = abs(fwd - rev) > 1e-12,
    weighted_by_the_second_pdf = TRUE,
    symmetric_sum_is_the_divergence_of_eq_10_115 = TRUE,
    nonnegative = fwd >= -1e-12,
    method = "Rangayyan (2024) eq. (5.33)"
  )
}

#' BC(p1, p2) = sum_l sqrt(p1 p2): the OVERLAP between two PDFs,
#'
#' bounded in [0, 1].  This is what the Bhattacharyya DISTANCE is built
#' from, D_B = -ln BC, and what makes the error bound work: the overlap
#' of the two class-conditional densities IS the region where the
#' optimal classifier must make mistakes.  NOT FROM THIS BOOK.
#'
#' @param p1 See Usage.
#' @param p2 See Usage.
#' @return A list with \code{coefficient}, \code{overlap}, \code{distance}, \code{identical}, \code{disjoint}, \code{in_unit_interval}, \code{the_overlap_is_where_errors_must_happen}, \code{not_from_this_book}, \code{reference}, \code{method}.
#' @export
PdfOverlap <- function(p1, p2) {
  # BC(p1, p2) = sum_l sqrt(p1 p2): the OVERLAP between two PDFs,
  # bounded in [0, 1].  This is what the Bhattacharyya DISTANCE is built
  # from, D_B = -ln BC, and what makes the error bound work: the overlap
  # of the two class-conditional densities IS the region where the
  # optimal classifier must make mistakes.  NOT FROM THIS BOOK.
  a <- as.numeric(p1)
  b <- as.numeric(p2)
  if (length(a) != length(b)) {
    stop("the two PDFs must be sampled on the same grid")
  }
  if (!length(a)) stop("need at least one bin")
  if (any(a < 0) || any(b < 0)) stop("a PDF cannot be negative")
  bc <- .morie_fsum(sqrt(a * b))
  list(
    coefficient = bc, overlap = bc,
    distance = if (bc > 0) -log(bc) else Inf,
    identical = abs(bc - 1) < 1e-12, disjoint = bc <= 1e-15,
    in_unit_interval = bc >= -1e-12 && bc <= 1 + 1e-12,
    the_overlap_is_where_errors_must_happen = TRUE,
    not_from_this_book = TRUE,
    reference = paste(
      "Bhattacharyya A. On a measure of divergence",
      "between two statistical populations defined by",
      "their probability distributions. Bulletin of",
      "the Calcutta Mathematical Society 35:99-109,",
      "1943 (Zbl 0063.00364)."
    ),
    method = paste(
      "Bhattacharyya coefficient; Rangayyan (2024) uses",
      "the KLD of eq. (5.33) and the divergence of",
      "eq. (10.115)"
    )
  )
}

#' Rho_a = sum_l p1^a p2^(1-a); C = -ln min_a rho_a.  The Bhattacharyya
#'
#' coefficient is exactly this at a = 1/2, which is the relationship the
#' whole family turns on.  Leaving alpha unset searches for the
#' minimizing a, which is what makes the Chernoff bound TIGHTER than the
#' Bhattacharyya bound -- the latter is the same bound fixed at a = 1/2
#' rather than the best a.  Every a gives a valid bound P_e <= P1^a
#' P2^(1-a) rho_a; the minimum gives the best of them. NOT FROM
#' RANGAYYAN (2024).
#'
#' @param p1 See Usage.
#' @param p2 See Usage.
#' @param alpha Defaults to \code{NULL}.
#' @param n_grid Defaults to \code{201}.
#' @return A list with \code{coefficient}, \code{alpha}, \code{information}, \code{bhattacharyya_coefficient}, \code{bhattacharyya_is_alpha_one_half}, \code{alpha_searched}, \code{at_least_as_tight_as_bhattacharyya}, \code{reference}, \code{not_from_this_book}, \code{method}.
#' @export
Chernoff <- function(p1, p2, alpha = NULL, n_grid = 201) {
  # rho_a = sum_l p1^a p2^(1-a);  C = -ln min_a rho_a.  The Bhattacharyya
  # coefficient is exactly this at a = 1/2, which is the relationship the
  # whole family turns on.  Leaving alpha unset searches for the
  # minimizing a, which is what makes the Chernoff bound TIGHTER than the
  # Bhattacharyya bound -- the latter is the same bound fixed at a = 1/2
  # rather than the best a.  Every a gives a valid bound
  # P_e <= P1^a P2^(1-a) rho_a; the minimum gives the best of them.
  # NOT FROM RANGAYYAN (2024).
  a <- as.numeric(p1)
  b <- as.numeric(p2)
  if (length(a) != length(b)) {
    stop("the two PDFs must be sampled on the same grid")
  }
  if (!length(a)) stop("need at least one bin")
  if (any(a < 0) || any(b < 0)) stop("a PDF cannot be negative")
  k <- a > 0 & b > 0
  rho <- function(t) .morie_fsum(a[k]^t * b[k]^(1 - t))
  if (!is.null(alpha)) {
    av <- as.numeric(alpha)
    if (av < 0 || av > 1) stop("alpha must lie in [0, 1]")
    best_a <- av
    best_rho <- rho(av)
    searched <- FALSE
  } else {
    m <- as.integer(n_grid)
    if (m < 3L) stop("need at least three grid points")
    grid <- (seq_len(m) - 1) / (m - 1)
    vals <- vapply(grid, rho, numeric(1))
    i <- which.min(vals)
    best_a <- grid[i]
    best_rho <- vals[i]
    searched <- TRUE
  }
  bc <- rho(0.5)
  list(
    coefficient = best_rho, alpha = best_a,
    information = if (best_rho > 0) -log(best_rho) else Inf,
    bhattacharyya_coefficient = bc,
    bhattacharyya_is_alpha_one_half = TRUE,
    alpha_searched = searched,
    at_least_as_tight_as_bhattacharyya = best_rho <= bc + 1e-12,
    reference = paste(
      "Chernoff H. A measure of asymptotic efficiency",
      "for tests of a hypothesis based on the sum of",
      "observations. Annals of Mathematical Statistics",
      "23(4):493-507, 1952,",
      "doi:10.1214/aoms/1177729330. The alpha = 1/2",
      "identity is Nielsen and Nock, Pattern",
      "Recognition Letters, 2014."
    ),
    not_from_this_book = TRUE,
    method = "Chernoff alpha-coefficient and information"
  )
}

#' H = sqrt(1 - BC), so H^2 = 1 - BC.  The Python arm delegates to
#'
#' morie.fn.helld.hellinger_dist; this is the same arithmetic, kept here
#' because the R tree has no separate helld module.  Unlike the
#' Bhattacharyya distance -ln BC, this is a TRUE METRIC: bounded in [0,
#' 1], symmetric, and it satisfies the triangle inequality, which -ln BC
#' does not.  That is the reason to reach for it -- anything needing a
#' metric over distributions needs this and not D_B.  The 1/2
#' normalization is the usual one; unnormalized gives H^2 = 2(1 - BC),
#' so the convention is reported rather than assumed.  NOT FROM
#' RANGAYYAN (2024).
#'
#' @param p1 See Usage.
#' @param p2 See Usage.
#' @return A list with \code{hellinger}, \code{squared}, \code{bhattacharyya_coefficient}, \code{identity_h2_equals_one_minus_bc}, \code{is_a_true_metric}, \code{satisfies_the_triangle_inequality}, \code{bhattacharyya_distance_does_not}, \code{normalization}, \code{in_unit_interval}, \code{reference}, \code{not_from_this_book}, \code{method}.
#' @export
Hellinger <- function(p1, p2) {
  # H = sqrt(1 - BC), so H^2 = 1 - BC.  The Python arm delegates to
  # morie.fn.helld.hellinger_dist; this is the same arithmetic, kept here
  # because the R tree has no separate helld module.  Unlike the Bhattacharyya distance
  # -ln BC, this is a TRUE METRIC: bounded in [0, 1], symmetric, and it
  # satisfies the triangle inequality, which -ln BC does not.  That is
  # the reason to reach for it -- anything needing a metric over
  # distributions needs this and not D_B.  The 1/2 normalization is the
  # usual one; unnormalized gives H^2 = 2(1 - BC), so the convention is
  # reported rather than assumed.  NOT FROM RANGAYYAN (2024).
  a <- as.numeric(p1)
  b <- as.numeric(p2)
  if (length(a) != length(b)) {
    stop("the two PDFs must be sampled on the same grid")
  }
  if (!length(a)) stop("need at least one bin")
  if (any(a < 0) || any(b < 0)) stop("a PDF cannot be negative")
  bc <- .morie_fsum(sqrt(a * b))
  h2 <- max(0, 1 - bc)
  list(
    hellinger = sqrt(h2), squared = h2,
    bhattacharyya_coefficient = bc,
    identity_h2_equals_one_minus_bc = TRUE,
    is_a_true_metric = TRUE,
    satisfies_the_triangle_inequality = TRUE,
    bhattacharyya_distance_does_not = TRUE,
    normalization = "one half; unnormalized gives 2(1 - BC)",
    in_unit_interval = sqrt(h2) >= -1e-12 && sqrt(h2) <= 1 + 1e-12,
    reference = paste(
      "Hellinger E. Neue Begruendung der Theorie",
      "quadratischer Formen von unendlichvielen",
      "Veraenderlichen. Journal fuer die reine und",
      "angewandte Mathematik 136:210-271, 1909,",
      "doi:10.1515/crll.1909.136.210."
    ),
    not_from_this_book = TRUE,
    method = "Hellinger distance, H^2 = 1 - BC"
  )
}

#' NOT FROM THIS BOOK.  A full-text search of the 2024 third edition --
#'
#' Rangayyan and Krishnan -- finds no occurrence of "Bhattacharyya", nor
#' of Chernoff or Hellinger.  The book gives the KLD of eq (5.33) and
#' the divergence of eqs (10.115)-(10.117), which is the symmetric sum
#' of the two KLDs, and cites Swain for them.
#'
#' @param m1 See Usage.
#' @param m2 See Usage.
#' @param C1 A matrix; passed to \code{as.matrix}.
#' @param C2 A matrix; passed to \code{as.matrix}.
#' @return A list with \code{bhattacharyya}, \code{mean_term}, \code{covariance_term}, \code{not_from_this_book}, \code{book_uses_divergence_eq_10_115}, \code{reference}, \code{method}.
#' @export
GaussOverlap <- function(m1, m2, C1, C2) {
  # NOT FROM THIS BOOK.  A full-text search of the 2024 third edition --
  # Rangayyan and Krishnan -- finds no occurrence of "Bhattacharyya", nor
  # of Chernoff or Hellinger.  The book gives the KLD of eq (5.33) and
  # the divergence of eqs (10.115)-(10.117), which is the symmetric sum
  # of the two KLDs, and cites Swain for them.
  #
  # Named for what it MEASURES -- the overlap of two Gaussians -- not
  # for Bhattacharyya, whose surname is not a method name.  The same
  # formula estimated from SAMPLE SETS is morie.fn.bhatt, verified to
  # agree with this to 1e-10.
  #
  # WHAT IT IS FOR: D_B = -ln BC where BC is the Bhattacharyya
  # coefficient, the overlap of the two class-conditional densities.
  # That overlap is precisely where the optimal classifier is forced to
  # err, which is why D_B BOUNDS the Bayes error while the divergence
  # bounds nothing.  The two answer different questions: the divergence
  # says how far apart the classes are, this says how well ANY
  # classifier could possibly do.
  a <- as.numeric(m1)
  b <- as.numeric(m2)
  A <- as.matrix(C1)
  B <- as.matrix(C2)
  p <- length(a)
  if (length(b) != p) stop("the two mean vectors must have the same length")
  if (nrow(A) != p || nrow(B) != p) stop("the covariances must be ", p, " x ", p)
  M <- 0.5 * (A + B)
  dm <- matrix(a - b, ncol = 1)
  quad <- as.numeric(t(dm) %*% solve(M) %*% dm)
  dA <- det(A)
  dB <- det(B)
  dM <- det(M)
  if (dA <= 0 || dB <= 0 || dM <= 0) {
    stop("a covariance matrix is not positive definite")
  }
  list(
    bhattacharyya = 0.125 * quad + 0.5 * log(dM / sqrt(dA * dB)),
    mean_term = 0.125 * quad,
    covariance_term = 0.5 * log(dM / sqrt(dA * dB)),
    not_from_this_book = TRUE,
    book_uses_divergence_eq_10_115 = TRUE,
    reference = paste(
      "Bhattacharyya A. Bulletin of the Calcutta",
      "Mathematical Society 35:99-109, 1943; the",
      "Gaussian closed form and the error bound are",
      "Kailath T, IEEE Transactions on Communication",
      "Technology 15(1):52-60, 1967,",
      "doi:10.1109/TCOM.1967.1089532."
    ),
    method = paste(
      "standard Bhattacharyya distance for Gaussians;",
      "Rangayyan (2024) uses eqs. (10.112) and (10.115)",
      "instead"
    )
  )
}

#' P_e <= sqrt(P1 P2) exp(-D_B).  NOT FROM THIS BOOK -- the standard
#'
#' Kailath bound.  It pairs with GaussOverlap, NOT with Divergence: the
#' divergence does not bound the error this way, and substituting it
#' would give a number that looks like a bound and is not one.  The
#' bound is on the OPTIMAL classifier, a floor no real one can beat.
#'
#' @param p1 See Usage.
#' @param p2 See Usage.
#' @param db See Usage.
#' @return A list with \code{bound}, \code{priors}, \code{bhattacharyya}, \code{tightest_at_equal_priors}, \code{bounds_the_optimal_classifier_not_yours}, \code{not_from_this_book}, \code{pairs_with_the_overlap_not_with_divergence}, \code{reference}, \code{method}.
#' @export
ErrBound <- function(p1, p2, db) {
  # P_e <= sqrt(P1 P2) exp(-D_B).  NOT FROM THIS BOOK -- the standard
  # Kailath bound.  It pairs with GaussOverlap, NOT with Divergence:
  # the divergence does not bound the error this way, and substituting it
  # would give a number that looks like a bound and is not one.  The
  # bound is on the OPTIMAL classifier, a floor no real one can beat.
  a <- as.numeric(p1)
  b <- as.numeric(p2)
  if (a < 0 || b < 0) stop("prior probabilities cannot be negative")
  if (abs(a + b - 1) > 1e-9) stop("the two priors must sum to 1")
  d <- as.numeric(db)
  if (d < 0) stop("the Bhattacharyya distance cannot be negative")
  list(
    bound = sqrt(a * b) * exp(-d), priors = c(a, b),
    bhattacharyya = d, tightest_at_equal_priors = abs(a - b) < 1e-12,
    bounds_the_optimal_classifier_not_yours = TRUE,
    not_from_this_book = TRUE,
    pairs_with_the_overlap_not_with_divergence = TRUE,
    reference = paste(
      "Kailath T. The divergence and Bhattacharyya",
      "distance measures in signal selection. IEEE",
      "Transactions on Communication Technology",
      "15(1):52-60, February 1967,",
      "doi:10.1109/TCOM.1967.1089532."
    ),
    method = "Kailath's Bhattacharyya bound; not given in Rangayyan (2024)"
  )
}

#' J = (m1 - m2)^2 / (s1^2 + s2^2).  Close kin to eq (10.112) but NOT
#'
#' the same measure: that divides |m1 - m2| by (s1 + s2).  They rank
#' features identically only when the dispersions are equal.
#'
#' @param x1 See Usage.
#' @param x2 See Usage.
#' @return A list with \code{j}, \code{means}, \code{variances}, \code{normalized_distance}, \code{agrees_with_eq_10_112_ranking_only_for_equal_spread}, \code{is_not_eq_10_112}, \code{method}.
#' @export
FishCrit <- function(x1, x2) {
  # J = (m1 - m2)^2 / (s1^2 + s2^2).  Close kin to eq (10.112) but NOT
  # the same measure: that divides |m1 - m2| by (s1 + s2).  They rank
  # features identically only when the dispersions are equal.
  a <- as.numeric(x1)
  b <- as.numeric(x2)
  if (length(a) < 2L || length(b) < 2L) {
    stop("each class needs at least two samples")
  }
  m1 <- mean(a)
  m2 <- mean(b)
  v1 <- stats::var(a)
  v2 <- stats::var(b)
  if (v1 + v2 <= 0) {
    stop("both classes have zero variance; the criterion is undefined")
  }
  s1 <- sqrt(v1)
  s2 <- sqrt(v2)
  list(
    j = (m1 - m2)^2 / (v1 + v2), means = c(m1, m2),
    variances = c(v1, v2),
    normalized_distance = if (s1 + s2 > 0) {
      abs(m1 - m2) / (s1 + s2)
    } else {
      Inf
    },
    agrees_with_eq_10_112_ranking_only_for_equal_spread =
      abs(s1 - s2) < 1e-12,
    is_not_eq_10_112 = TRUE,
    method = "Fisher's criterion; compare Rangayyan (2024) eq. (10.112)"
  )
}

#' Section 10.10.1: J = tr(S_B) / tr(S_W).  The trace ratio ignores the
#'
#' OFF-diagonal structure, so it cannot see that a pair of features is
#' jointly discriminating when neither is alone -- for that, the
#' divergence, which uses the full covariance, is the measure to use.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken.
#' @return A list with \code{j}, \code{trace_between}, \code{trace_within}, \code{s_within}, \code{s_between}, \code{classes}, \code{n_classes}, \code{n_features}, \code{ignores_off_diagonal_structure}, \code{method}.
#' @export
SepIndex <- function(X, y) {
  # Section 10.10.1: J = tr(S_B) / tr(S_W).  The trace ratio ignores the
  # OFF-diagonal structure, so it cannot see that a pair of features is
  # jointly discriminating when neither is alone -- for that, the
  # divergence, which uses the full covariance, is the measure to use.
  Xs <- as.matrix(X)
  if (nrow(Xs) != length(y)) {
    stop("X and y must have the same number of rows")
  }
  if (nrow(Xs) < 2L) stop("need at least two samples")
  g <- .morie_rg_groups(Xs, y)
  if (length(g$order) < 2L) stop("need at least two classes")
  p <- ncol(Xs)
  grand <- colMeans(Xs)
  SW <- matrix(0, p, p)
  SB <- matrix(0, p, p)
  for (rows in g$groups) {
    mu <- colMeans(rows)
    SW <- SW + .morie_rg_scatter(rows, mu)
    d <- matrix(mu - grand, ncol = 1)
    SB <- SB + nrow(rows) * (d %*% t(d))
  }
  tw <- sum(diag(SW))
  if (tw <= 0) {
    stop(
      "the within-class scatter vanishes; every class is a single ",
      "repeated point"
    )
  }
  list(
    j = sum(diag(SB)) / tw, trace_between = sum(diag(SB)),
    trace_within = tw, s_within = SW, s_between = SB,
    classes = g$order, n_classes = length(g$order), n_features = p,
    ignores_off_diagonal_structure = TRUE,
    method = "Rangayyan (2024) Section 10.10.1 (separability of features)"
  )
}

#' Section 10.4.2: w = S_W^-1 (m1 - m2), the direction maximizing the
#'
#' ratio of between- to within-class scatter of the PROJECTED data.  Two
#' classes only.  It reduces the data to ONE number chosen for
#' separation, not reconstruction, so unlike a principal component it is
#' not meant to represent the data.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken.
#' @return A list with \code{w}, \code{threshold}, \code{classes}, \code{means}, \code{s_within}, \code{projected}, \code{projected_means}, \code{criterion}, \code{two_class_only}, \code{not_a_reconstruction_basis}, \code{method}.
#' @export
FishLda <- function(X, y) {
  # Section 10.4.2: w = S_W^-1 (m1 - m2), the direction maximizing the
  # ratio of between- to within-class scatter of the PROJECTED data.  Two
  # classes only.  It reduces the data to ONE number chosen for
  # separation, not reconstruction, so unlike a principal component it is
  # not meant to represent the data.
  Xs <- as.matrix(X)
  if (nrow(Xs) != length(y)) {
    stop("X and y must have the same number of rows")
  }
  g <- .morie_rg_groups(Xs, y)
  if (length(g$order) != 2L) {
    stop(
      "Fisher's linear discriminant as stated is a two-class method; ",
      "got ", length(g$order), " classes"
    )
  }
  a <- g$groups[[1]]
  b <- g$groups[[2]]
  if (nrow(a) < 2L || nrow(b) < 2L) {
    stop("each class needs at least two samples")
  }
  m1 <- colMeans(a)
  m2 <- colMeans(b)
  SW <- .morie_rg_scatter(a, m1) + .morie_rg_scatter(b, m2)
  w <- as.numeric(solve(SW, m1 - m2))
  pa <- as.numeric(a %*% w)
  pb <- as.numeric(b %*% w)
  ma <- mean(pa)
  mb <- mean(pb)
  va <- sum((pa - ma)^2)
  vb <- sum((pb - mb)^2)
  proj <- list(pa, pb)
  names(proj) <- as.character(g$order)
  list(
    w = w, threshold = 0.5 * (ma + mb), classes = g$order,
    means = list(m1, m2), s_within = SW, projected = proj,
    projected_means = c(ma, mb),
    criterion = if (va + vb > 0) (ma - mb)^2 / (va + vb) else Inf,
    two_class_only = TRUE, not_a_reconstruction_basis = TRUE,
    method = "Rangayyan (2024) Section 10.4.2 (Fisher LDA)"
  )
}

#' Section 10.4.3: D^2 = (x - mu)^T C^-1 (x - mu).  Distance in units of
#'
#' the data\'s own scatter: a point far along an axis of natural
#' variation is NEAR, one close by across the grain is far.  Euclidean
#' distance would quietly favour whichever feature has the largest
#' units.
#'
#' @param x See Usage.
#' @param mu See Usage.
#' @param C A matrix; passed to \code{as.matrix}.
#' @return A list with \code{d2}, \code{distance}, \code{squared}, \code{euclidean}, \code{differs_from_euclidean}, \code{scale_free}, \code{method}.
#' @export
Mahal <- function(x, mu, C) {
  # Section 10.4.3: D^2 = (x - mu)^T C^-1 (x - mu).  Distance in units of
  # the data's own scatter: a point far along an axis of natural
  # variation is NEAR, one close by across the grain is far.  Euclidean
  # distance would quietly favour whichever feature has the largest units.
  xs <- as.numeric(x)
  m <- as.numeric(mu)
  S <- as.matrix(C)
  p <- length(xs)
  if (length(m) != p) stop("x and mu must have the same length")
  if (nrow(S) != p || ncol(S) != p) {
    stop("the covariance must be ", p, " x ", p)
  }
  d <- matrix(xs - m, ncol = 1)
  d2 <- as.numeric(t(d) %*% solve(S) %*% d)
  eucl <- sqrt(sum((xs - m)^2))
  list(
    d2 = d2, distance = if (d2 >= 0) sqrt(d2) else NaN, squared = d2,
    euclidean = eucl,
    differs_from_euclidean = abs(sqrt(max(d2, 0)) - eucl) > 1e-12,
    scale_free = TRUE,
    method = "Rangayyan (2024) Section 10.4.3 (distance functions)"
  )
}

#' Section 10.4.1: d_i(x) = w_i^T x + w_i0, assign to the largest.  The
#'
#' surfaces between classes are hyperplanes, so a linear machine carves
#' the space into CONVEX regions -- which is why it cannot separate
#' classes whose regions are not convex, however many features are
#' added.
#'
#' @param x See Usage.
#' @param weights A matrix; passed to \code{as.matrix}.
#' @param w0 Defaults to \code{NULL}.
#' @return A list with \code{d}, \code{assigned}, \code{margin}, \code{n_classes}, \code{regions_are_convex}, \code{decision_surfaces_are_hyperplanes}, \code{method}.
#' @export
LinDisc <- function(x, weights, w0 = NULL) {
  # Section 10.4.1: d_i(x) = w_i^T x + w_i0, assign to the largest.  The
  # surfaces between classes are hyperplanes, so a linear machine carves
  # the space into CONVEX regions -- which is why it cannot separate
  # classes whose regions are not convex, however many features are added.
  xs <- as.numeric(x)
  W <- as.matrix(weights)
  m <- nrow(W)
  if (m < 2L) stop("need at least two classes")
  if (ncol(W) != length(xs)) {
    stop("every weight vector must match the length of x")
  }
  b <- if (is.null(w0)) numeric(m) else as.numeric(w0)
  if (length(b) != m) stop("give one offset per class")
  d <- as.numeric(W %*% xs) + b
  srt <- sort(d, decreasing = TRUE)
  list(
    d = d, assigned = which.max(d) - 1L, margin = srt[1] - srt[2],
    n_classes = m, regions_are_convex = TRUE,
    decision_surfaces_are_hyperplanes = TRUE,
    method = paste(
      "Rangayyan (2024) Section 10.4.1 (discriminant and",
      "decision functions)"
    )
  )
}

#' Section 10.4.2 with a fitted cut.  The midpoint of the projected
#'
#' means is optimal only for equal priors AND equal variances, so the
#' error-minimizing cut is the default and the midpoint is reported
#' beside it.  Both errors are resubstitution errors -- measured on the
#' data that chose the cut -- so they are optimistic; Section 10.10.3 is
#' the book\'s warning, and KFoldCv or LooCv gives an honest figure.
#'
#' @param X See Usage.
#' @param y See Usage.
#' @return A list with \code{w}, \code{threshold}, \code{midpoint_threshold}, \code{classes}, \code{first_class_is_above}, \code{training_errors}, \code{midpoint_errors}, \code{training_accuracy}, \code{n}, \code{projected}, \code{midpoint_optimal_only_for_equal_priors_and_spread}, \code{resubstitution_error_is_optimistic}, \code{method}.
#' @export
LinDSep <- function(X, y) {
  # Section 10.4.2 with a fitted cut.  The midpoint of the projected
  # means is optimal only for equal priors AND equal variances, so the
  # error-minimizing cut is the default and the midpoint is reported
  # beside it.  Both errors are resubstitution errors -- measured on the
  # data that chose the cut -- so they are optimistic; Section 10.10.3 is
  # the book's warning, and KFoldCv or LooCv gives an honest figure.
  f <- FishLda(X, y)
  a <- f$projected[[1]]
  b <- f$projected[[2]]
  ma <- f$projected_means[1]
  mb <- f$projected_means[2]
  mid <- 0.5 * (ma + mb)
  hi_first <- ma > mb
  cand <- sort(unique(c(a, b)))
  errf <- function(t) {
    if (hi_first) sum(a <= t) + sum(b > t) else sum(a > t) + sum(b <= t)
  }
  cuts <- c(cand[1] - 1, cand, cand[length(cand)] + 1)
  ts <- 0.5 * (cuts[-length(cuts)] + cuts[-1])
  errs <- vapply(ts, errf, numeric(1))
  w <- which.min(errs)
  n <- length(a) + length(b)
  list(
    w = f$w, threshold = ts[w], midpoint_threshold = mid,
    classes = f$classes, first_class_is_above = hi_first,
    training_errors = errs[w], midpoint_errors = errf(mid),
    training_accuracy = 1 - errs[w] / n, n = n,
    projected = f$projected,
    midpoint_optimal_only_for_equal_priors_and_spread = TRUE,
    resubstitution_error_is_optimistic = TRUE,
    method = "Rangayyan (2024) Sections 10.4.2 and 10.10.3"
  )
}

#' Eq (10.29) and Section 10.4.4.  The book is explicit about why k > 1:
#'
#' with k = 1 "the nearest neighbor may happen to be an outlier that is
#' not representative of its class", so one freak point owns a whole
#' region.  The Mahalanobis metric is the one to use when features have
#' different units, since Euclidean distance is otherwise dominated by
#' whichever feature has the largest numbers.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken and its elements indexed.
#' @param query See Usage.
#' @param k Defaults to \code{1}.
#' @param metric One of \code{"euclidean"}, \code{"mahalanobis"}. Defaults to \code{"euclidean"}.
#' @param C Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{assigned}, \code{votes}, \code{k}, \code{metric}, \code{neighbours}, \code{tie}, \code{tied_classes}, \code{nearest_distance}, \code{nearest_label}, \code{single_neighbour_may_be_an_outlier}, \code{method}.
#' @export
Knn <- function(X, y, query, k = 1, metric = "euclidean", C = NULL) {
  # eq (10.29) and Section 10.4.4.  The book is explicit about why k > 1:
  # with k = 1 "the nearest neighbor may happen to be an outlier that is
  # not representative of its class", so one freak point owns a whole
  # region.  The Mahalanobis metric is the one to use when features have
  # different units, since Euclidean distance is otherwise dominated by
  # whichever feature has the largest numbers.
  Xs <- as.matrix(X)
  q <- as.numeric(query)
  if (nrow(Xs) != length(y)) {
    stop("X and y must have the same number of rows")
  }
  if (!nrow(Xs)) stop("need at least one training sample")
  p <- length(q)
  if (ncol(Xs) != p) stop("every row of X must match the query length")
  kk <- as.integer(k)
  if (kk < 1L) stop("k must be at least 1")
  if (kk > nrow(Xs)) stop("k exceeds the number of training samples")
  if (!metric %in% c("euclidean", "mahalanobis")) {
    stop("metric must be 'euclidean' or 'mahalanobis'")
  }
  d <- sweep(Xs, 2, q, "-")
  if (metric == "mahalanobis") {
    if (is.null(C)) stop("the Mahalanobis metric needs the covariance C")
    Ci <- solve(as.matrix(C))
    dist <- sqrt(pmax(0, rowSums((d %*% Ci) * d)))
  } else {
    dist <- sqrt(rowSums(d * d))
  }
  ord <- order(dist)[seq_len(kk)]
  labs <- y[ord]
  tab <- table(labs)
  top <- max(tab)
  tied <- names(tab)[tab == top]
  if (length(tied) == 1L) {
    winner <- labs[match(tied, as.character(labs))]
  } else {
    sums <- vapply(tied, function(l) {
      sum(dist[ord][as.character(labs) == l])
    }, numeric(1))
    winner <- labs[match(tied[which.min(sums)], as.character(labs))]
  }
  list(
    assigned = winner, votes = as.list(tab), k = kk, metric = metric,
    neighbours = lapply(seq_along(ord), function(i) {
      list(
        index = ord[i] - 1L, label = labs[i],
        distance = dist[ord][i]
      )
    }),
    tie = length(tied) > 1L, tied_classes = tied,
    nearest_distance = dist[ord][1], nearest_label = labs[1],
    single_neighbour_may_be_an_outlier = kk == 1L,
    method = "Rangayyan (2024) eq. (10.29) and Section 10.4.4"
  )
}

#' Eq (10.70): d_i(x) = p(x|C_i) P(C_i), assign to the largest -- the
#'
#' MAXIMUM A POSTERIORI rule.  Comparing likelihoods alone is maximum
#' likelihood, a different classifier, and they differ whenever the
#' classes are unequally common: for a rare disease the prior is
#' precisely what stops the classifier calling everything positive.
#'
#' @param likelihoods See Usage.
#' @param priors Defaults to \code{NULL}.
#' @return A list with \code{d}, \code{posterior}, \code{assigned}, \code{maximum_likelihood_choice}, \code{prior_changed_the_decision}, \code{priors}, \code{uniform_priors}, \code{method}.
#' @export
BayesCls <- function(likelihoods, priors = NULL) {
  # eq (10.70): d_i(x) = p(x|C_i) P(C_i), assign to the largest -- the
  # MAXIMUM A POSTERIORI rule.  Comparing likelihoods alone is maximum
  # likelihood, a different classifier, and they differ whenever the
  # classes are unequally common: for a rare disease the prior is
  # precisely what stops the classifier calling everything positive.
  lk <- as.numeric(likelihoods)
  m <- length(lk)
  if (m < 2L) stop("need at least two classes")
  if (any(lk < 0)) stop("a likelihood cannot be negative")
  if (is.null(priors)) {
    pr <- rep(1 / m, m)
  } else {
    pr <- as.numeric(priors)
    if (length(pr) != m) stop("give one prior per class")
    if (any(pr < 0)) stop("a prior cannot be negative")
    if (abs(sum(pr) - 1) > 1e-9) stop("the priors must sum to 1")
  }
  d <- lk * pr
  tot <- sum(d)
  list(
    d = d, posterior = if (tot > 0) d / tot else rep(0, m),
    assigned = which.max(d) - 1L,
    maximum_likelihood_choice = which.max(lk) - 1L,
    prior_changed_the_decision = which.max(d) != which.max(lk),
    priors = pr, uniform_priors = is.null(priors),
    method = "Rangayyan (2024) eq. (10.70)"
  )
}

#' Eq (10.72).  The book takes logarithms at eq (10.71) because the
#'
#' normal PDF is an exponential and ln is monotonic: the ranking is
#' unchanged while the arithmetic stops underflowing.  It then drops the
#' (n/2) ln(2 pi) term, which "does not depend upon i", giving eq
#' (10.73) -- safe for CLASSIFYING, wrong for reading the value as a log
#' density, so both forms are returned.  The surfaces are hyperquadrics
#' and reduce to hyperplanes exactly when the covariances are equal.
#'
#' @param x See Usage.
#' @param means A vector; its length is taken and its elements indexed.
#' @param covs A vector; its length is taken and its elements indexed.
#' @param priors Defaults to \code{NULL}.
#' @param full A flag; the body branches on it. Defaults to \code{FALSE}.
#' @return A list with \code{d}, \code{d_full}, \code{d_dropped_constant}, \code{assigned}, \code{priors}, \code{constant_term}, \code{surfaces_are_hyperquadrics}, \code{linear_when_covariances_are_equal}, \code{log_form_avoids_underflow}, \code{method}.
#' @export
BayesNorm <- function(x, means, covs, priors = NULL, full = FALSE) {
  # eq (10.72).  The book takes logarithms at eq (10.71) because the
  # normal PDF is an exponential and ln is monotonic: the ranking is
  # unchanged while the arithmetic stops underflowing.  It then drops the
  # (n/2) ln(2 pi) term, which "does not depend upon i", giving
  # eq (10.73) -- safe for CLASSIFYING, wrong for reading the value as a
  # log density, so both forms are returned.  The surfaces are
  # hyperquadrics and reduce to hyperplanes exactly when the covariances
  # are equal.
  xs <- as.numeric(x)
  m <- length(means)
  if (m < 2L) stop("need at least two classes")
  if (length(covs) != m) stop("give one covariance matrix per class")
  n <- length(xs)
  if (is.null(priors)) {
    pr <- rep(1 / m, m)
  } else {
    pr <- as.numeric(priors)
    if (length(pr) != m) stop("give one prior per class")
    if (abs(sum(pr) - 1) > 1e-9) stop("the priors must sum to 1")
  }
  const <- 0.5 * n * log(2 * pi)
  dshort <- numeric(m)
  for (i in seq_len(m)) {
    if (pr[i] <= 0) {
      dshort[i] <- -Inf
      next
    }
    S <- as.matrix(covs[[i]])
    dt <- det(S)
    if (dt <= 0) stop("covariance ", i, " is not positive definite")
    d <- matrix(xs - as.numeric(means[[i]]), ncol = 1)
    quad <- as.numeric(t(d) %*% solve(S) %*% d)
    dshort[i] <- log(pr[i]) - 0.5 * log(dt) - 0.5 * quad
  }
  dfull <- dshort - const
  use <- if (full) dfull else dshort
  eq <- all(vapply(covs, function(S) {
    max(abs(as.matrix(S) - as.matrix(covs[[1]]))) < 1e-12
  }, logical(1)))
  list(
    d = use, d_full = dfull, d_dropped_constant = dshort,
    assigned = which.max(use) - 1L, priors = pr,
    constant_term = const, surfaces_are_hyperquadrics = TRUE,
    linear_when_covariances_are_equal = eq,
    log_form_avoids_underflow = TRUE,
    method = "Rangayyan (2024) eqs. (10.71)-(10.73)"
  )
}

#' Eq (10.73) with the mean and covariance estimated per class by
#'
#' eqs (10.68)-(10.69).  Each class keeps its OWN covariance, so the
#' boundaries are quadrics; a single pooled covariance would make this
#' LDA.  A class needs more samples than features or its covariance is
#' singular -- QDA estimates p(p+1)/2 covariance parameters PER CLASS,
#' so it is the first thing to break on small samples.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken.
#' @param query See Usage.
#' @param priors Defaults to \code{NULL}.
#' @return A list with \code{g}, \code{assigned}, \code{assigned_index}, \code{classes}, \code{means}, \code{covariances}, \code{priors}, \code{reduces_to_lda_when_covariances_are_equal}, \code{parameters_per_class}, \code{method}.
#' @export
Qda <- function(X, y, query, priors = NULL) {
  # eq (10.73) with the mean and covariance estimated per class by
  # eqs (10.68)-(10.69).  Each class keeps its OWN covariance, so the
  # boundaries are quadrics; a single pooled covariance would make this
  # LDA.  A class needs more samples than features or its covariance is
  # singular -- QDA estimates p(p+1)/2 covariance parameters PER CLASS,
  # so it is the first thing to break on small samples.
  Xs <- as.matrix(X)
  q <- as.numeric(query)
  if (nrow(Xs) != length(y)) {
    stop("X and y must have the same number of rows")
  }
  p <- length(q)
  if (ncol(Xs) != p) stop("every row of X must match the query length")
  g <- .morie_rg_groups(Xs, y)
  m <- length(g$order)
  if (m < 2L) stop("need at least two classes")
  for (i in seq_len(m)) {
    if (nrow(g$groups[[i]]) <= p) {
      stop(
        "class ", g$order[i], " has ", nrow(g$groups[[i]]),
        " samples for ", p, " features; QDA needs more samples than ",
        "features per class or the covariance is singular"
      )
    }
  }
  pr <- if (is.null(priors)) {
    vapply(g$groups, function(r) nrow(r) / nrow(Xs), numeric(1))
  } else {
    as.numeric(priors)
  }
  means <- lapply(g$groups, colMeans)
  covs <- lapply(g$groups, function(r) stats::cov(r))
  r <- BayesNorm(q, means, covs, priors = pr)
  list(
    g = r$d, assigned = g$order[r$assigned + 1L],
    assigned_index = r$assigned, classes = g$order, means = means,
    covariances = covs, priors = pr,
    reduces_to_lda_when_covariances_are_equal =
      r$linear_when_covariances_are_equal,
    # NOTE the parentheses: in R %/% binds TIGHTER than *, so
    # p * (p + 1) %/% 2 would compute p * ((p + 1) %/% 2)
    parameters_per_class = (p * (p + 1)) %/% 2,
    method = paste(
      "Rangayyan (2024) eqs. (10.68)-(10.73), per-class",
      "covariances"
    )
  )
}

#' Section 10.7, fitted by Newton-Raphson on the log-likelihood.  Unlike
#'
#' the Bayes classifier it models the POSTERIOR directly and assumes
#' nothing about the shape of p(x|C), which is why it survives features
#' that are plainly not Gaussian.  Perfectly separable classes have no
#' finite maximum, so a small ridge is added and `separable` is reported
#' -- without it the coefficients merely record where the optimizer
#' stopped.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y See Usage.
#' @param maxiter Defaults to \code{100}.
#' @param tol Defaults to \code{1e-08}.
#' @param ridge Defaults to \code{1e-08}.
#' @return A list with \code{intercept}, \code{coefficients}, \code{w}, \code{fitted}, \code{predicted}, \code{loglik}, \code{iterations}, \code{converged}, \code{separable}, \code{ridge}, \code{training_accuracy}, \code{n}, \code{models_the_posterior_directly}, \code{no_gaussian_assumption}, \code{method}.
#' @export
LogReg <- function(X, y, maxiter = 100, tol = 1e-8, ridge = 1e-8) {
  # Section 10.7, fitted by Newton-Raphson on the log-likelihood.  Unlike
  # the Bayes classifier it models the POSTERIOR directly and assumes
  # nothing about the shape of p(x|C), which is why it survives features
  # that are plainly not Gaussian.  Perfectly separable classes have no
  # finite maximum, so a small ridge is added and `separable` is reported
  # -- without it the coefficients merely record where the optimizer
  # stopped.
  Xs <- as.matrix(X)
  ys <- as.numeric(y)
  if (nrow(Xs) != length(ys)) {
    stop("X and y must have the same number of rows")
  }
  if (any(!ys %in% c(0, 1))) {
    stop("logistic regression needs 0/1 labels")
  }
  if (length(unique(ys)) < 2L) stop("both classes must be present")
  n <- nrow(Xs)
  A <- cbind(1, Xs)
  p <- ncol(A)
  w <- numeric(p)
  lam <- as.numeric(ridge)
  it <- 0L
  for (it in seq_len(as.integer(maxiter))) {
    eta <- pmin(500, pmax(-500, as.numeric(A %*% w)))
    mu <- 1 / (1 + exp(-eta))
    g <- as.numeric(t(A) %*% (ys - mu)) - lam * w
    H <- t(A) %*% (A * (mu * (1 - mu))) + diag(lam, p)
    step <- tryCatch(as.numeric(solve(H, g)), error = function(e) NULL)
    if (is.null(step)) break
    w <- w + step
    if (max(abs(step)) < tol) break
  }
  sep <- it >= as.integer(maxiter) && sqrt(sum(w * w)) > 50
  eta <- pmin(500, pmax(-500, as.numeric(A %*% w)))
  mu <- 1 / (1 + exp(-eta))
  ll <- sum(ys * log(pmax(mu, 1e-300)) + (1 - ys) * log(pmax(1 - mu, 1e-300)))
  pred <- as.numeric(mu >= 0.5)
  list(
    intercept = w[1], coefficients = w[-1], w = w, fitted = mu,
    predicted = pred, loglik = ll, iterations = it,
    converged = it < as.integer(maxiter), separable = sep,
    ridge = lam, training_accuracy = mean(pred == ys), n = n,
    models_the_posterior_directly = TRUE,
    no_gaussian_assumption = TRUE,
    method = "Rangayyan (2024) Section 10.7 (logistic regression)"
  )
}

#' Section 10.5.1.  WCSS falls at every step, so the iteration always
#'
#' terminates -- at a LOCAL minimum that depends on where the centroids
#' started.  The method is unsupervised: it finds groups, and whether
#' those groups are the diagnostic classes is a question it cannot
#' answer.  Starting centroids are the first k distinct patterns, so the
#' result is reproducible; random starts would return different
#' clusterings from the same call.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param k See Usage.
#' @param maxiter Defaults to \code{100}.
#' @param tol Defaults to \code{1e-10}.
#' @param init Optional; may be \code{NULL}. A matrix; passed to \code{as.matrix}.
#' @return A list with \code{labels}, \code{centroids}, \code{wcss}, \code{k}, \code{sizes}, \code{iterations}, \code{converged}, \code{local_minimum_only}, \code{depends_on_the_starting_centroids}, \code{unsupervised_groups_need_not_be_the_classes}, \code{method}.
#' @export
KMeans <- function(X, k, maxiter = 100, tol = 1e-10, init = NULL) {
  # Section 10.5.1.  WCSS falls at every step, so the iteration always
  # terminates -- at a LOCAL minimum that depends on where the centroids
  # started.  The method is unsupervised: it finds groups, and whether
  # those groups are the diagnostic classes is a question it cannot
  # answer.  Starting centroids are the first k distinct patterns, so the
  # result is reproducible; random starts would return different
  # clusterings from the same call.
  Xs <- as.matrix(X)
  n <- nrow(Xs)
  kk <- as.integer(k)
  if (kk < 1L) stop("k must be at least 1")
  if (kk > n) stop("k exceeds the number of patterns")
  p <- ncol(Xs)
  if (is.null(init)) {
    uniq <- unique(Xs)
    if (nrow(uniq) < kk) stop("fewer than k distinct patterns")
    cent <- uniq[seq_len(kk), , drop = FALSE]
  } else {
    cent <- as.matrix(init)
    if (nrow(cent) != kk || ncol(cent) != p) stop("init must be k x p")
  }
  lab <- integer(n)
  prev <- NA_real_
  it <- 0L
  for (it in seq_len(as.integer(maxiter))) {
    for (i in seq_len(n)) {
      d <- rowSums(sweep(cent, 2, Xs[i, ], "-")^2)
      lab[i] <- which.min(d)
    }
    for (c in seq_len(kk)) {
      rows <- Xs[lab == c, , drop = FALSE]
      if (!nrow(rows)) {
        dd <- vapply(seq_len(n), function(i) {
          sum((Xs[i, ] - cent[lab[i], ])^2)
        }, numeric(1))
        far <- which.max(dd)
        cent[c, ] <- Xs[far, ]
        lab[far] <- c
        rows <- Xs[far, , drop = FALSE]
      }
      cent[c, ] <- colMeans(rows)
    }
    wcss <- sum(vapply(seq_len(n), function(i) {
      sum((Xs[i, ] - cent[lab[i], ])^2)
    }, numeric(1)))
    if (!is.na(prev) && abs(prev - wcss) <= tol) {
      prev <- wcss
      break
    }
    prev <- wcss
  }
  list(
    labels = lab - 1L, centroids = cent, wcss = prev, k = kk,
    sizes = vapply(seq_len(kk), function(c) sum(lab == c), numeric(1)),
    iterations = it, converged = it < as.integer(maxiter),
    local_minimum_only = TRUE,
    depends_on_the_starting_centroids = TRUE,
    unsupervised_groups_need_not_be_the_classes = TRUE,
    method = "Rangayyan (2024) Section 10.5.1 (cluster seeking)"
  )
}

#' WCSS falls monotonically with k and reaches zero at k = n, so it
#'
#' cannot be minimized -- the choice is the KNEE.  Located here as the
#' point of maximum distance from the chord joining the curve\'s ends,
#' which is a definite rule rather than an eye judgement.  Still a
#' heuristic: on data with no cluster structure the curve is smooth and
#' the knee is wherever the arithmetic puts it.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param kmax Defaults to \code{8}.
#' @param kmin Defaults to \code{1}.
#' @return A list with \code{k}, \code{wcss}, \code{knee}, \code{monotonic}, \code{wcss_cannot_be_minimized}, \code{heuristic_only}, \code{method}.
#' @export
Elbow <- function(X, kmax = 8, kmin = 1) {
  # WCSS falls monotonically with k and reaches zero at k = n, so it
  # cannot be minimized -- the choice is the KNEE.  Located here as the
  # point of maximum distance from the chord joining the curve's ends,
  # which is a definite rule rather than an eye judgement.  Still a
  # heuristic: on data with no cluster structure the curve is smooth and
  # the knee is wherever the arithmetic puts it.
  Xs <- as.matrix(X)
  n <- nrow(Xs)
  lo <- as.integer(kmin)
  hi <- as.integer(kmax)
  if (lo < 1L) stop("kmin must be at least 1")
  if (hi > n) stop("kmax exceeds the number of patterns")
  if (hi <= lo) stop("kmax must exceed kmin")
  ks <- lo:hi
  wcss <- vapply(ks, function(k) KMeans(Xs, k)$wcss, numeric(1))
  x1 <- ks[1]
  y1 <- wcss[1]
  x2 <- ks[length(ks)]
  y2 <- wcss[length(wcss)]
  den <- sqrt((x2 - x1)^2 + (y2 - y1)^2)
  knee <- if (den <= 0) {
    ks[1]
  } else {
    ks[which.max(abs((y2 - y1) * ks - (x2 - x1) * wcss +
      x2 * y1 - y2 * x1) / den)]
  }
  list(
    k = ks, wcss = wcss, knee = knee,
    monotonic = all(diff(wcss) <= 1e-9),
    wcss_cannot_be_minimized = TRUE, heuristic_only = TRUE,
    method = paste(
      "elbow criterion on the k-means WCSS;",
      "Rangayyan (2024) Section 10.5.1"
    )
  )
}

#' Section 10.5.1.  Single linkage CHAINS -- it will string distant
#'
#' clusters together through a bridge of intermediate points -- while
#' complete linkage is compact and splits elongated clusters.  The
#' choice is not cosmetic: on the same data they routinely give
#' different partitions, which is why the full merge history is returned
#' rather than only a labelling.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param linkage One of \code{"average"}, \code{"complete"}, \code{"single"}. Defaults to \code{"single"}.
#' @param k Defaults to \code{NULL}.
#' @return A list with \code{history}, \code{labels}, \code{linkage}, \code{n}, \code{k}, \code{merge_distances}, \code{monotonic_merges}, \code{single_linkage_chains}, \code{linkage_changes_the_partition}, \code{method}.
#' @export
HClust <- function(X, linkage = "single", k = NULL) {
  # Section 10.5.1.  Single linkage CHAINS -- it will string distant
  # clusters together through a bridge of intermediate points -- while
  # complete linkage is compact and splits elongated clusters.  The
  # choice is not cosmetic: on the same data they routinely give
  # different partitions, which is why the full merge history is
  # returned rather than only a labelling.
  Xs <- as.matrix(X)
  n <- nrow(Xs)
  if (n < 2L) stop("need at least two patterns")
  if (!linkage %in% c("single", "complete", "average")) {
    stop("linkage must be 'single', 'complete' or 'average'")
  }
  D <- as.matrix(stats::dist(Xs))
  groups <- lapply(seq_len(n), function(i) i)
  names(groups) <- as.character(seq_len(n))
  history <- list()
  while (length(groups) > 1L) {
    keys <- names(groups)
    best <- NULL
    for (a in seq_along(keys)) {
      for (b in seq_along(keys)) {
        if (b <= a) next
        ga <- groups[[keys[a]]]
        gb <- groups[[keys[b]]]
        ds <- as.numeric(D[ga, gb])
        dd <- switch(linkage,
          single = min(ds),
          complete = max(ds),
          average = mean(ds)
        )
        if (is.null(best) || dd < best$d) {
          best <- list(d = dd, a = keys[a], b = keys[b])
        }
      }
    }
    history[[length(history) + 1L]] <- list(
      merged = c(as.integer(best$a) - 1L, as.integer(best$b) - 1L),
      distance = best$d,
      size = length(groups[[best$a]]) + length(groups[[best$b]]),
      n_clusters_after = length(groups) - 1L
    )
    groups[[best$a]] <- c(groups[[best$a]], groups[[best$b]])
    groups[[best$b]] <- NULL
  }
  labels <- NULL
  if (!is.null(k)) {
    kk <- as.integer(k)
    if (kk < 1L || kk > n) stop("k must lie in 1..n")
    g <- lapply(seq_len(n), function(i) i)
    names(g) <- as.character(seq_len(n))
    for (step in history) {
      if (length(g) == kk) break
      ka <- as.character(step$merged[1] + 1L)
      kb <- as.character(step$merged[2] + 1L)
      g[[ka]] <- c(g[[ka]], g[[kb]])
      g[[kb]] <- NULL
    }
    labels <- integer(n)
    keys <- names(g)[order(as.integer(names(g)))]
    for (c in seq_along(keys)) labels[g[[keys[c]]]] <- c - 1L
  }
  md <- vapply(history, function(h) h$distance, numeric(1))
  list(
    history = history, labels = labels, linkage = linkage, n = n,
    k = k, merge_distances = md,
    monotonic_merges = all(diff(md) >= -1e-12),
    single_linkage_chains = linkage == "single",
    linkage_changes_the_partition = TRUE,
    method = "Rangayyan (2024) Section 10.5.1 (cluster seeking)"
  )
}

#' Section 10.10.3.  The book\'s point is that the training and test
#'
#' steps must use SEPARATE data: an error rate measured on the samples
#' that trained the classifier is optimistic, and with enough free
#' parameters it reaches zero while the classifier generalizes not at
#' all.  Folds are stratified by default; unstratified folds on
#' unbalanced data can leave a class absent from a training fold, which
#' measures luck rather than generalization.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken and its elements indexed.
#' @param k Defaults to \code{5}.
#' @param classifier Defaults to \code{NULL}.
#' @param stratified A flag; the body branches on it. Defaults to \code{TRUE}.
#' @return A list with \code{error_rate}, \code{accuracy}, \code{errors}, \code{n}, \code{k}, \code{per_fold}, \code{stratified}, \code{train_and_test_must_be_separate}, \code{method}.
#' @export
KFoldCv <- function(X, y, k = 5, classifier = NULL, stratified = TRUE) {
  # Section 10.10.3.  The book's point is that the training and test
  # steps must use SEPARATE data: an error rate measured on the samples
  # that trained the classifier is optimistic, and with enough free
  # parameters it reaches zero while the classifier generalizes not at
  # all.  Folds are stratified by default; unstratified folds on
  # unbalanced data can leave a class absent from a training fold, which
  # measures luck rather than generalization.
  Xs <- as.matrix(X)
  n <- nrow(Xs)
  if (n != length(y)) stop("X and y must have the same number of rows")
  kk <- as.integer(k)
  if (kk < 2L || kk > n) stop("k must lie in 2..n")
  if (is.null(classifier)) {
    classifier <- function(Xt, yt, q) Knn(Xt, yt, q, k = 1)$assigned
  }
  folds <- vector("list", kk)
  for (i in seq_len(kk)) folds[[i]] <- integer(0)
  if (stratified) {
    c <- 0L
    for (lab in unique(y)) {
      for (i in which(y == lab)) {
        folds[[c %% kk + 1L]] <- c(folds[[c %% kk + 1L]], i)
        c <- c + 1L
      }
    }
  } else {
    for (f in seq_len(kk)) {
      folds[[f]] <- seq_len(n)[seq_len(n) %% kk == (f - 1L)]
    }
  }
  errors <- 0L
  per_fold <- list()
  for (f in seq_len(kk)) {
    test <- folds[[f]]
    if (!length(test)) next
    tr <- setdiff(seq_len(n), test)
    if (length(unique(y[tr])) < 2L) {
      stop(
        "fold ", f, " leaves fewer than two classes in the training ",
        "set; use stratified folds or a smaller k"
      )
    }
    e <- sum(vapply(
      test, function(i) {
        classifier(Xs[tr, , drop = FALSE], y[tr], Xs[i, ]) != y[i]
      },
      logical(1)
    ))
    errors <- errors + e
    per_fold[[length(per_fold) + 1L]] <- list(
      fold = f - 1L, n = length(test), errors = e,
      error_rate = e / length(test)
    )
  }
  list(
    error_rate = errors / n, accuracy = 1 - errors / n,
    errors = errors, n = n, k = kk, per_fold = per_fold,
    stratified = isTRUE(stratified),
    train_and_test_must_be_separate = TRUE,
    method = "Rangayyan (2024) Section 10.10.3 (training and test steps)"
  )
}

#' K-fold with K = N.  It uses the most training data of any split, so
#'
#' it is nearly unbiased, and it is deterministic -- there is only one
#' way to leave one out, so unlike 5-fold it gives the same answer every
#' time.  The cost is N fits and a high variance: the training sets
#' differ by a single sample, so the errors are heavily correlated.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y A vector; its length is taken and its elements indexed.
#' @param classifier Defaults to \code{NULL}.
#' @return A list with \code{error_rate}, \code{accuracy}, \code{errors}, \code{misclassified}, \code{n}, \code{n_fits}, \code{deterministic}, \code{nearly_unbiased}, \code{high_variance}, \code{method}.
#' @export
LooCv <- function(X, y, classifier = NULL) {
  # K-fold with K = N.  It uses the most training data of any split, so
  # it is nearly unbiased, and it is deterministic -- there is only one
  # way to leave one out, so unlike 5-fold it gives the same answer every
  # time.  The cost is N fits and a high variance: the training sets
  # differ by a single sample, so the errors are heavily correlated.
  Xs <- as.matrix(X)
  n <- nrow(Xs)
  if (n != length(y)) stop("X and y must have the same number of rows")
  if (n < 3L) stop("need at least three samples")
  if (is.null(classifier)) {
    classifier <- function(Xt, yt, q) Knn(Xt, yt, q, k = 1)$assigned
  }
  wrong <- integer(0)
  for (i in seq_len(n)) {
    tr <- setdiff(seq_len(n), i)
    if (length(unique(y[tr])) < 2L) {
      stop(
        "removing sample ", i, " leaves one class; the classifier ",
        "cannot be trained"
      )
    }
    if (classifier(Xs[tr, , drop = FALSE], y[tr], Xs[i, ]) != y[i]) {
      wrong <- c(wrong, i)
    }
  }
  e <- length(wrong)
  list(
    error_rate = e / n, accuracy = 1 - e / n, errors = e,
    misclassified = wrong - 1L, n = n, n_fits = n,
    deterministic = TRUE, nearly_unbiased = TRUE,
    high_variance = TRUE,
    method = "Rangayyan (2024) Section 10.10.3 (leave-one-out)"
  )
}

#' Shared SMO-style coordinate ascent on pairs, which respects the
#'
#' equality constraint sum a_i y_i = 0 that single-coordinate updates
#' cannot
#'
#' @param K A matrix; indexed by row and column.
#' @param ys A vector; its length is taken and its elements indexed.
#' @param Cv Numeric; passed to \code{min}.
#' @param maxiter See Usage.
#' @param tol Numeric; combined arithmetically in the body.
#' @return A list with \code{a}, \code{b}, \code{it}.
#' @export
.morie_rg_smo <- function(K, ys, Cv, maxiter, tol) {
  # shared SMO-style coordinate ascent on pairs, which respects the
  # equality constraint sum a_i y_i = 0 that single-coordinate updates
  # cannot
  n <- length(ys)
  a <- numeric(n)
  b <- 0
  it <- 0L
  for (it in seq_len(as.integer(maxiter))) {
    changed <- 0L
    for (i in seq_len(n)) {
      fi <- sum(a * ys * K[, i]) + b
      Ei <- fi - ys[i]
      if ((ys[i] * Ei < -tol && a[i] < Cv) ||
        (ys[i] * Ei > tol && a[i] > 0)) {
        j <- (i + it) %% n + 1L
        if (j == i) next
        Ej <- sum(a * ys * K[, j]) + b - ys[j]
        ai <- a[i]
        aj <- a[j]
        if (ys[i] != ys[j]) {
          L <- max(0, aj - ai)
          H <- min(Cv, Cv + aj - ai)
        } else {
          L <- max(0, ai + aj - Cv)
          H <- min(Cv, ai + aj)
        }
        if (H - L < 1e-12) next
        eta <- 2 * K[i, j] - K[i, i] - K[j, j]
        if (eta >= -1e-12) next
        anj <- min(H, max(L, aj - ys[j] * (Ei - Ej) / eta))
        if (abs(anj - aj) < 1e-12) next
        ani <- ai + ys[i] * ys[j] * (aj - anj)
        b1 <- b - Ei - ys[i] * (ani - ai) * K[i, i] -
          ys[j] * (anj - aj) * K[i, j]
        b2 <- b - Ej - ys[i] * (ani - ai) * K[i, j] -
          ys[j] * (anj - aj) * K[j, j]
        b <- if (ani > 0 && ani < Cv) b1 else if (anj > 0 && anj < Cv) b2 else 0.5 * (b1 + b2)
        a[i] <- ani
        a[j] <- anj
        changed <- changed + 1L
      }
    }
    if (changed == 0L) break
  }
  list(a = a, b = b, it = it)
}

#' Section 10.4.5.  Only the patterns with a_i > 0 -- the SUPPORT
#'
#' VECTORS -- enter the solution, so the boundary is set by the samples
#' nearest it and is untouched by the bulk of the data.  That is the
#' strength on small samples and the weakness against one mislabelled
#' point near the boundary, which C controls: small C tolerates
#' violations, large C insists on separating and contorts around an
#' outlier.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y See Usage.
#' @param C Defaults to \code{1}.
#' @param maxiter Passed to \code{.morie_rg_smo}. Defaults to \code{2000}.
#' @param tol Passed to \code{.morie_rg_smo}. Defaults to \code{1e-06}.
#' @return A list with \code{w}, \code{b}, \code{alpha}, \code{support_vectors}, \code{n_support}, \code{margin}, \code{C}, \code{iterations}, \code{converged}, \code{training_accuracy}, \code{boundary_set_by_the_support_vectors_only}, \code{large_c_contorts_around_outliers}, \code{method}.
#' @export
Svm <- function(X, y, C = 1, maxiter = 2000, tol = 1e-6) {
  # Section 10.4.5.  Only the patterns with a_i > 0 -- the SUPPORT
  # VECTORS -- enter the solution, so the boundary is set by the samples
  # nearest it and is untouched by the bulk of the data.  That is the
  # strength on small samples and the weakness against one mislabelled
  # point near the boundary, which C controls: small C tolerates
  # violations, large C insists on separating and contorts around an
  # outlier.
  Xs <- as.matrix(X)
  ys <- as.numeric(y)
  n <- nrow(Xs)
  if (n != length(ys)) stop("X and y must have the same number of rows")
  if (length(setdiff(unique(ys), c(-1, 1)))) {
    stop("the SVM needs labels -1 and +1")
  }
  if (length(unique(ys)) < 2L) stop("both classes must be present")
  Cv <- as.numeric(C)
  if (Cv <= 0) stop("C must be positive")
  K <- Xs %*% t(Xs)
  r <- .morie_rg_smo(K, ys, Cv, maxiter, tol)
  w <- as.numeric(t(Xs) %*% (r$a * ys))
  sv <- which(r$a > 1e-8)
  nw <- sqrt(sum(w * w))
  pred <- ifelse(as.numeric(Xs %*% w) + r$b >= 0, 1, -1)
  list(
    w = w, b = r$b, alpha = r$a, support_vectors = sv - 1L,
    n_support = length(sv), margin = if (nw > 0) 2 / nw else Inf,
    C = Cv, iterations = r$it,
    converged = r$it < as.integer(maxiter),
    training_accuracy = mean(pred == ys),
    boundary_set_by_the_support_vectors_only = TRUE,
    large_c_contorts_around_outliers = TRUE,
    method = "Rangayyan (2024) Section 10.4.5 (support vector machine)"
  )
}

#' SvmKern
#'
#' A step of the rangayyan_class implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' the source it follows.
#'
#' @param X A matrix; passed to \code{as.matrix}.
#' @param y See Usage.
#' @param query Defaults to \code{NULL}.
#' @param kernel One of \code{"linear"}, \code{"poly"}, \code{"rbf"}, \code{"sigmoid"}. Defaults to \code{"rbf"}.
#' @param gamma Defaults to \code{NULL}.
#' @param degree Defaults to \code{3}.
#' @param coef0 Defaults to \code{0}.
#' @param C Defaults to \code{1}.
#' @param maxiter Passed to \code{.morie_rg_smo}. Defaults to \code{2000}.
#' @param tol Passed to \code{.morie_rg_smo}. Defaults to \code{1e-06}.
#' @return The value of \code{out}, as built in the body.
#' @export
SvmKern <- function(X, y, query = NULL, kernel = "rbf", gamma = NULL,
                    degree = 3, coef0 = 0, C = 1, maxiter = 2000,
                    tol = 1e-6) {
  # Section 10.4.5.  The dual depends on the data only through inner
  # products, so replacing that product with a kernel fits a linear
  # boundary in a space the data is never mapped into.  The boundary in
  # the ORIGINAL space is curved and there is no weight vector to report:
  # the classifier IS the support vectors and their coefficients, which
  # is why kernel SVMs grow with the training set where the linear one
  # does not.  The sigmoid kernel is not positive definite for all
  # parameters, so the dual is not guaranteed concave.
  Xs <- as.matrix(X)
  ys <- as.numeric(y)
  n <- nrow(Xs)
  if (n != length(ys)) stop("X and y must have the same number of rows")
  if (length(setdiff(unique(ys), c(-1, 1)))) {
    stop("the SVM needs labels -1 and +1")
  }
  p <- ncol(Xs)
  g <- if (is.null(gamma)) 1 / p else as.numeric(gamma)
  if (!kernel %in% c("rbf", "poly", "linear", "sigmoid")) {
    stop("kernel must be 'rbf', 'poly', 'linear' or 'sigmoid'")
  }
  kf <- function(u, v) {
    dot <- sum(u * v)
    switch(kernel,
      linear = dot,
      poly = (dot + as.numeric(coef0))^as.integer(degree),
      sigmoid = tanh(g * dot + as.numeric(coef0)),
      rbf = exp(-g * sum((u - v)^2))
    )
  }
  K <- matrix(0, n, n)
  for (i in seq_len(n)) for (j in seq_len(n)) K[i, j] <- kf(Xs[i, ], Xs[j, ])
  Cv <- as.numeric(C)
  r <- .morie_rg_smo(K, ys, Cv, maxiter, tol)
  sv <- which(r$a > 1e-8)
  pred <- ifelse(vapply(seq_len(n), function(i) {
    sum(r$a * ys * K[, i]) + r$b
  }, numeric(1)) >= 0, 1, -1)
  out <- list(
    alpha = r$a, b = r$b, support_vectors = sv - 1L,
    n_support = length(sv), kernel = kernel, gamma = g, C = Cv,
    iterations = r$it,
    converged = r$it < as.integer(maxiter),
    training_accuracy = mean(pred == ys),
    no_weight_vector_in_the_original_space = kernel != "linear",
    model_grows_with_the_training_set = TRUE,
    sigmoid_kernel_is_not_always_positive_definite =
      kernel == "sigmoid",
    method = "Rangayyan (2024) Section 10.4.5 (kernel SVM)"
  )
  if (!is.null(query)) {
    q <- as.numeric(query)
    if (length(q) != p) stop("the query must match the feature length")
    s <- sum(r$a * ys * vapply(
      seq_len(n), function(t) kf(Xs[t, ], q),
      numeric(1)
    )) + r$b
    out$decision <- s
    out$assigned <- if (s >= 0) 1 else -1
  }
  out
}
