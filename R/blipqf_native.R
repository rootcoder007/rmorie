# morie native arm -- blipqf
# BLIP-2 Q-Former: the same ledger method as blip2v.
#
# blipqf and blip2v are two ledger rows citing one paper (Li, Li,
# Savarese & Hoi 2023). The R side delegates rather than carrying a
# second copy, so the two entries cannot drift apart -- the same
# reason the Python module re-exports instead of duplicating.

#' morie_blipqf
#'
#' A step of the blipqf_native implementation. No other function in the package calls it.
#' See the file header for the source the module follows.
#' source it follows.
#'
#' @param ... Passed through.
#' @return The value of \code{morie_blip2v}.
#' @export
morie_blipqf <- function(...) {
  if (!exists("morie_blip2v", mode = "function")) {
    stop(paste0("blipqf: the blip2v arm must be loaded first; this ",
                "module is a re-export of it, not a second ",
                "implementation"))
  }
  morie_blip2v(...)
}
