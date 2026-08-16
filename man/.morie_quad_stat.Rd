% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_quad_stat}
\alias{.morie_quad_stat}
\title{Quadratic-form statistic c = (T-mu)\\\' Sigma^+ (T-mu) ~ chi-square(df)}
\usage{
.morie_quad_stat(m, tol = 1e-08)
}
\arguments{
\item{m}{See Usage.}
\item{tol}{Defaults to \code{1e-08}.}
}
\description{
Part of the coin_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{statistic}, \code{df}, \code{p.value}.
}
\keyword{internal}
