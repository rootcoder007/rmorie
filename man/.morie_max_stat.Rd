% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_max_stat}
\alias{.morie_max_stat}
\title{Standardized (scalar / maximum) statistic and asymptotic p-value}
\usage{
.morie_max_stat(m, alternative = "two.sided")
}
\arguments{
\item{m}{A list; the body reads \code{$mu}, \code{$Sigma}, \code{$T} from it.}
\item{alternative}{The body requires: bad alternative. Defaults to \code{"two.sided"}.}
}
\description{
A step of the coin_native implementation. Called by \code{morie_indep_test}. See the file header for the source the module follows.
}
\value{
A list with \code{statistic}, \code{p.value}, \code{cor}, \code{z}.
}
\keyword{internal}
