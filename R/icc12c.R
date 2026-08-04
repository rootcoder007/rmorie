# SPDX-License-Identifier: AGPL-3.0-or-later
#' The six Shrout-Fleiss intraclass correlations from one rating table
#'
#' Shrout, P. E. and Fleiss, J. L. (1979), "Intraclass correlations: uses in
#' assessing rater reliability", Psychological Bulletin 86(2), 420-428,
#' doi:10.1037/0033-2909.86.2.420. The paper was opened directly and every
#' formula below was read off a rendered page image, not a text layer.
#'
#' There is no such thing as "the" ICC. Six coefficients are defined on the
#' SAME n-targets-by-k-judges table and they are different numbers: on the
#' example of the paper itself (Table 2, p. 423) they run from .17 to .91.
#' The choice is a statement about the design, not about the arithmetic.
#' Case 1 (one-way): each target is rated by a DIFFERENT set of k judges, so
#' no judge main effect exists and rater bias is pooled into within-target
#' error. Case 2 (two-way random): the same k judges rate every target and are
#' a RANDOM SAMPLE, so systematic judge differences are charged against
#' reliability -- absolute agreement. Case 3 (two-way mixed): these k judges
#' are the ONLY ones of interest, so judge offsets cost nothing -- consistency,
#' not agreement, and always at least as large as Case 2. The second index is
#' the unit reported: a single rating, or the mean of all k. ICC(3,k) is the
#' Cronbach (1951) alpha (p. 426).
#'
#' Estimators, read from the rendered pages (p. 423 and p. 426):
#' \code{ICC(1,1) = (BMS - WMS)/(BMS + (k-1) WMS)},
#' \code{ICC(2,1) = (BMS - EMS)/(BMS + (k-1) EMS + k (JMS - EMS)/n)},
#' \code{ICC(3,1) = (BMS - EMS)/(BMS + (k-1) EMS)},
#' \code{ICC(1,k) = (BMS - WMS)/BMS},
#' \code{ICC(2,k) = (BMS - EMS)/(BMS + (JMS - EMS)/n)},
#' \code{ICC(3,k) = (BMS - EMS)/BMS},
#' with BMS between targets, WMS within target, JMS between judges and EMS the
#' residual mean square of the targets-by-judges ANOVA (Table 1, p. 422; the
#' values for the example are in Table 3, p. 423).
#'
#' All six are returned every time, so the gap between the case a study assumed
#' and the case it could defend is visible rather than hidden behind one number.
#'
#' @param X n-by-k matrix; row i holds the k ratings of target i, one per
#'   judge, in a common judge order. The design must be complete and crossed.
#' @param model Which of the six forms \code{estimate} reports. Default
#'   \code{"2,k"}, the two-way random average-measure coefficient.
#' @return List with \code{estimate}, \code{model}, \code{icc11}, \code{icc1k},
#'   \code{icc21}, \code{icc2k}, \code{icc31}, \code{icc3k}, \code{BMS},
#'   \code{WMS}, \code{JMS}, \code{EMS}, \code{n}, \code{k}, \code{method}.
#' @references Shrout, P. E. and Fleiss, J. L. (1979), Psychological Bulletin
#'   86(2):420-428, doi:10.1037/0033-2909.86.2.420.
#' @examples
#' # Table 2, p. 423: four ratings on six targets
#' X <- rbind(c(9, 2, 5, 8), c(6, 1, 3, 2), c(8, 4, 6, 8),
#'            c(7, 1, 2, 6), c(10, 5, 6, 9), c(6, 2, 4, 7))
#' round(Icc12c(X, "3,1")$estimate, 2)  # .71, Table 4, p. 424
#' @export
Icc12c <- function(X, model = "2,k") {
  Xm <- .s03mat(X)
  n <- nrow(Xm)
  k <- ncol(Xm)
  if (n < 2L) stop("icc_two_way: need at least two targets")
  if (k < 2L) stop("icc_two_way: need at least two judges")
  if (any(!is.finite(Xm))) {
    stop("icc_two_way: the design must be complete and crossed; every target ",
         "must be rated by the same k judges")
  }
  ms <- .s4_icc_ms(as.vector(Xm), rep(seq_len(n), times = k),
                   rep(seq_len(k), each = n))
  bms <- ms$ms_r
  jms <- ms$ms_c
  ems <- ms$ms_e
  # SS_W = SS_C + SS_E, so MSW follows from the two-way pieces
  wms <- (jms * (k - 1) + ems * (n - 1) * (k - 1)) / (n * (k - 1))
  dv <- function(a, b) if (b != 0) a / b else NaN
  icc <- list(
    "11" = dv(bms - wms, bms + (k - 1) * wms),
    "1k" = dv(bms - wms, bms),
    "21" = dv(bms - ems, bms + (k - 1) * ems + k * (jms - ems) / n),
    "2k" = dv(bms - ems, bms + (jms - ems) / n),
    "31" = dv(bms - ems, bms + (k - 1) * ems),
    "3k" = dv(bms - ems, bms))
  f <- .icc12c_form(model, k)
  list(estimate = icc[[f]], model = f,
       icc11 = icc[["11"]], icc1k = icc[["1k"]],
       icc21 = icc[["21"]], icc2k = icc[["2k"]],
       icc31 = icc[["31"]], icc3k = icc[["3k"]],
       BMS = bms, WMS = wms, JMS = jms, EMS = ems,
       n = as.integer(n), k = as.integer(k),
       method = paste0("Shrout-Fleiss (1979) ICC(", substr(f, 1, 1), ",",
                       substr(f, 2, 2), ")"))
}

# Normalise a form label to one of the six keys. Anything spelling out a form
# is accepted -- "ICC(2,k)", "2-k", "2k" -- and, because the paper labels its
# own table with the concrete number of judges (ICC(1,4) for k = 4), a trailing
# integer equal to k is read as the average-measure form.
.icc12c_form <- function(model, k) {
  s <- gsub("[^0-9k]", "", tolower(as.character(model)))
  if (nchar(s) == 2L && grepl("^[0-9]$", substr(s, 2L, 2L)) &&
      as.integer(substr(s, 2L, 2L)) == k) {
    s <- paste0(substr(s, 1L, 1L), "k")
  }
  if (!(s %in% c("11", "1k", "21", "2k", "31", "3k"))) {
    stop("icc_two_way: model must name one of the six Shrout-Fleiss forms ",
         "ICC(1,1), ICC(1,k), ICC(2,1), ICC(2,k), ICC(3,1), ICC(3,k)")
  }
  s
}
