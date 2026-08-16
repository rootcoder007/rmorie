# morie native arm -- timesf
#
# The wave-3 ledger carries this method twice, as timesf and as
# timesfm, both citing Das et al. (2024) and both describing the same
# decoder-only foundation model. One paper, one method. This arm
# re-exports the timesfm implementation rather than duplicating it, so
# the two entries cannot drift apart -- matching the Python arm, which
# re-exports morie.fn.timesfm.
#
# Das, A., Kong, W., Sen, R. & Zhou, Y. (2024) "A decoder-only
# foundation model for time-series forecasting", Proceedings of the
# 41st International Conference on Machine Learning, PMLR 235,
# arXiv:2310.10688.

#' morie_timesf
#'
#' A step of the timesf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param history Passed to \code{morie_timesfm}.
#' @param predictor Passed to \code{morie_timesfm}.
#' @param horizon Passed to \code{morie_timesfm}.
#' @param input_patch_len Passed to \code{morie_timesfm}.
#' @param output_patch_len Passed to \code{morie_timesfm}.
#' @return The value of \code{morie_timesfm}.
#' @export
morie_timesf <- function(history, predictor, horizon, input_patch_len,
                         output_patch_len) {
  morie_timesfm(history, predictor, horizon, input_patch_len,
                output_patch_len)
}

# compact alias per ledger/NAMING.md
.timesf_foundation <- morie_timesf

# public name resolved by fn/_lazy_map.json
.timesfm_foundation <- morie_timesf

# Cheatsheet summarising the re-export relationship.
#' Cheatsheet summarising the re-export relationship
#'
#' A step of the timesf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @return A character value.
#' @export
.timesf_cheatsheet <- function() {
  paste0("timesf: the same ledger method as `timesfm` -- one ",
         "paper, one implementation, re-exported so the two ",
         "entries cannot drift. ", morie_timesfm_cheatsheet())
}
