% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ProxGrad}
\alias{ProxGrad}
\title{\\' Minimise f + g by proximal forward-backward splitting}
\usage{
ProxGrad(f, grad_f, prox_g, x0, lr, n_iter = 200L, relaxation = 1) 
}
\arguments{
\item{grad_f}{The body requires: grad_f returned the wrong length.}
\item{prox_g}{The body requires: prox_g returned the wrong length.}
\item{x0}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{lr}{Numeric; combined arithmetically in the body.}
\item{n_iter}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{relaxation}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}

\item{f}{Argument \code{f}; see Usage.}
}
\description{
\\' \\' x_\{n+1\} = x_n + lambda (prox_\{lr g\}(x_n - lr grad f(x_n)) - x_n), \\' equation (21) of Combettes & Pesquet (2011), arXiv:0912.3522. \\' Runs a fixed number of iterations. \\' \\'
}
\value{
A list with \code{x}, \code{objective}, \code{n_iter}, \code{lr}, \code{relaxation}, \code{step_norm}, \code{method}.
}
\keyword{internal}
\examples{
f <- function(x) 0.5 * sum((x - c(2, -1))^2)
gf <- function(x) x - c(2, -1)
soft <- function(x, t) sign(x) * pmax(abs(x) - t, 0)
prox <- function(x, t) soft(x, 0.5 * t)
r <- ProxGrad(f, gf, prox, x0 = c(0, 0), lr = 0.5)
str(r, max.level = 1)
}
