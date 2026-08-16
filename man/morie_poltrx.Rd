% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_poltrx}
\alias{morie_poltrx}
\title{Polya tree (morie.fn.poltrx) -- main entry point}
\usage{
morie_poltrx(levels, c = 1, rule = "m_squared", seed = 0)
}
\arguments{
\item{levels}{Truncation level \code{M}, integer \code{>= 1}.}
\item{c}{Positive scaling constant.}
\item{rule}{One of \code{"m_squared"}, \code{"constant"},   \code{"linear"}.}
\item{seed}{Seed for the generator shared with the Python arm.}
}
\description{
A thin wrapper that mirrors the Python module\'s public surface. The main entry point builds a finite tree, the level parameter rule and the implied density in one call, and reports the same payload the Python arm returns.
}
\value{
A named list with \code{level_parameters},   \code{continuity_regime}, \code{tree}, \code{density} and   \code{method}.
}
\keyword{internal}
\examples{
morie_poltrx(levels = 5L)
}
