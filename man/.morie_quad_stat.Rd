% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_quad_stat}
\alias{.morie_quad_stat}
\title{Quadratic-form statistic c = (T-mu)\\' Sigma^+ (T-mu) ~ chi-square(df)}
\usage{
.morie_quad_stat(m, tol = 1e-08)
}
\arguments{
\item{m}{A list; the body reads \code{$mu}, \code{$Sigma}, \code{$T} from it.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1e-08}.}
}
\description{
A step of the coin_native implementation. Called by \code{morie_oneway_test}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{df}, \code{p.value}.
}
\keyword{internal}
