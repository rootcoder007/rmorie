% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sarima_minimize_nm}
\alias{.sarima_minimize_nm}
\title{Small Nelder-Mead simplex minimiser in base R, with the same}
\usage{
.sarima_minimize_nm(fn, x0, maxit = 200L)
}
\arguments{
\item{fn}{See Usage.}
\item{x0}{A vector; its length is taken and its elements indexed.}
\item{maxit}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{200L}.}
}
\description{
restart-until-stuck shape as the Python arm\\'s call to _sci_core.minimize(method="Nelder-Mead").
}
\value{
A list with \code{x}, \code{fun}, \code{success}.
}
\keyword{internal}
