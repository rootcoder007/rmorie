# SPDX-License-Identifier: AGPL-3.0-or-later
#' Catalogue coverage of a recommender
#'
#' coverage = |unique items recommended| / |catalogue|.  Source consulted:
#' Herlocker, Konstan, Terveen and Riedl (2004), Evaluating collaborative
#' filtering recommender systems, ACM Transactions on Information Systems
#' 22(1), 5-53.
#'
#' @param recommendations list of recommendation vectors, one per user.
#' @param catalog vector of all items available to recommend.
#' @return list: estimate, coverage, covered, ncatalog, nlists, meanlen, n, method.
#' @keywords internal
#' @examples
#' covLst(list(c(1, 2), c(2, 3)), 1:6)
#' @export
covLst <- function(recommendations, catalog) {
  cat0 <- unique(as.numeric(catalog))
  seen <- numeric(0)
  nlists <- 0L
  total <- 0L
  for (lst in recommendations) {
    nlists <- nlists + 1L
    items <- as.numeric(lst)
    total <- total + length(items)
    seen <- c(seen, items[items %in% cat0])
  }
  seen <- unique(seen)
  ncat <- length(cat0)
  cov <- if (ncat > 0) length(seen) / ncat else NA_real_
  list(estimate = as.numeric(cov), coverage = as.numeric(cov),
       covered = as.integer(length(seen)), ncatalog = as.integer(ncat),
       nlists = as.integer(nlists),
       meanlen = if (nlists > 0) total / nlists else NA_real_,
       n = as.integer(nlists),
       method = "Catalogue coverage (Herlocker et al. 2004)")
}

# CANONICAL TEST
# r <- covLst(list(c(1, 2), c(2, 3)), 1:6)
# stopifnot(abs(r$estimate - 0.5) < 1e-12, r$covered == 3L)

#' @rdname covLst
#' @keywords internal
#' @export
morie_catalog_coverage <- covLst
