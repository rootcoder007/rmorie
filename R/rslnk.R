# SPDX-License-Identifier: AGPL-3.0-or-later

#' Residual / skip connection
#'
#' R parity for \code{morie.fn.rslnk.residual_connection}.
#'
#' \deqn{y = \mathcal{F}(x) + x}{y = F(x) + x}
#'
#' @param x Numeric array.
#' @param f Function applied as the residual branch; defaults to identity.
#' @return Named list \code{(y, estimate, Fx, method)}.
#' @references He, Zhang, Ren & Sun (2016), CVPR.
#' @examples
#' morie_rslnk_residual_connection(x = rnorm(50))
#' @export
morie_rslnk_residual_connection <- function(x, f = NULL) {
  x <- as.array(x)
  Fx <- if (is.null(f)) x else as.array(f(x))
  if (!identical(dim(Fx), dim(x)) && length(Fx) != length(x)) {
    stop("Residual branch shape does not match identity shape.")
  }
  y <- Fx + x
  list(
    y = y, estimate = y, Fx = Fx,
    method = "Residual identity shortcut"
  )
}

#' @rdname morie_rslnk_residual_connection
#' @keywords internal
#' @export
morie_residual_connection <- morie_rslnk_residual_connection
