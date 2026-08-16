% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ProxGrad}
\alias{ProxGrad}
\title{\\\' Minimise f + g by proximal forward-backward splitting}
\usage{
ProxGrad(f, grad_f, prox_g, x0, lr, n_iter = 200L, relaxation = 1)
}
\arguments{
\item{f}{See Usage.}
\item{grad_f}{See Usage.}
\item{prox_g}{See Usage.}
\item{x0}{See Usage.}
\item{lr}{See Usage.}
\item{n_iter}{Defaults to \code{200L}.}
\item{relaxation}{Defaults to \code{1}.}
}
\description{
\\\' \\\' x_{n+1} = x_n + lambda (prox_{lr g}(x_n - lr grad f(x_n)) - x_n), \\\' equation (21) of Combettes & Pesquet (2011), arXiv:0912.3522. \\\' Runs a fixed number of iterations. \\\' \\\'
}
\value{
A list with \code{x}, \code{objective}, \code{n_iter}, \code{lr}, \code{relaxation}, \code{step_norm}, \code{method}.
}
\keyword{internal}
