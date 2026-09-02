# SPDX-License-Identifier: AGPL-3.0-or-later
#' Serendipity of a recommendation list
#'
#' The share of a recommendation list that is both unexpected and relevant.
#' Let RS be the recommended set, PM the set a primitive prediction model
#' (the baseline the user would have expected anyway) would have produced,
#' and REL the set the user actually found useful.  The unexpected set is
#' \code{UNEXP = RS \\ PM} and serendipity is
#' \code{SRDP = card(UNEXP and REL) / card(RS)}.
#'
#' Adamopoulos & Tuzhilin's unexpectedness of the list relative to the
#' expected set is reported alongside as \code{card(RS \\ PM) / card(RS)};
#' serendipity is that quantity restricted to items the user valued, so
#' \code{SRDP <= unexpectedness} and \code{SRDP <= precision} always.
#'
#' Sets are compared by identity, not by position: repeated ids collapse.
#'
#' @param pred Item ids in the recommendation list RS.
#' @param baseline Item ids the primitive/baseline model would recommend, PM.
#' @param relevant Item ids the user found useful, REL.
#' @return List with \code{estimate} (serendipity), \code{serendipity},
#'   \code{unexpectedness}, \code{precision}, \code{recall},
#'   \code{n_unexpected}, \code{n_serendipitous}, \code{n_recommended},
#'   \code{n_baseline}, \code{n_relevant}, \code{n_universe}, \code{tp},
#'   \code{fp}, \code{fn}, \code{tn}, \code{method}.
#' @references Ge, M., Delgado-Battenfeld, C. & Jannach, D. (2010). Beyond
#'   accuracy: evaluating recommender systems by coverage and serendipity.
#'   Proceedings of the Fourth ACM Conference on Recommender Systems,
#'   257-260. \doi{10.1145/1864708.1864761};
#'   Adamopoulos, P. & Tuzhilin, A. (2014). On unexpectedness in recommender
#'   systems: or how to better expect the unexpected. ACM Transactions on
#'   Intelligent Systems and Technology 5(4), 54. \doi{10.1145/2559952}
#' @export
#' @examples
#' ServR(pred = c(1, 2, 3, 4, 5, 6, 7, 8), baseline = c(1, 2, 3, 4, 5, 6, 7, 8), relevant = c(1, 2, 3, 4, 5, 6, 7, 8))
ServR <- function(pred, baseline, relevant) {
  ids <- function(v) unique(as.integer(.s03vec(v)))
  rs <- ids(pred); pm <- ids(baseline); rel <- ids(relevant)
  if (length(rs) == 0L)
    stop("serendipity: pred must be a non-empty set of item ids")

  unexp <- rs[!(rs %in% pm)]
  hit <- unexp[unexp %in% rel]
  inter_rel <- rs[rs %in% rel]

  universe <- unique(c(rs, pm, rel))
  tp <- sum(universe %in% rs & universe %in% rel)
  fp <- sum(universe %in% rs & !(universe %in% rel))
  fn <- sum(!(universe %in% rs) & universe %in% rel)
  tn <- sum(!(universe %in% rs) & !(universe %in% rel))

  nrs <- length(rs)
  .t1_result(estimate = length(hit) / nrs, serendipity = length(hit) / nrs,
             unexpectedness = length(unexp) / nrs,
             precision = length(inter_rel) / nrs,
             recall = if (length(rel) > 0L) length(inter_rel) / length(rel) else NA_real_,
             n_unexpected = length(unexp), n_serendipitous = length(hit),
             n_recommended = nrs, n_baseline = length(pm),
             n_relevant = length(rel), n_universe = length(universe),
             tp = tp, fp = fp, fn = fn, tn = tn,
             method = "Serendipity of a recommendation list (Ge et al. 2010; Adamopoulos & Tuzhilin 2014)")
}
