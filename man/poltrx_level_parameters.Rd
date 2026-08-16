% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_level_parameters}
\alias{poltrx_level_parameters}
\title{Polya tree level parameter}
\usage{
poltrx_level_parameters(level, c = 1, rule = "m_squared")
}
\arguments{
\item{level}{Level index \code{m}, an integer \code{>= 1}.}
\item{c}{Positive scaling constant.}
\item{rule}{One of \code{"m_squared"}, \code{"constant"},   \code{"linear"}.}
}
\description{
Computes \code{alpha_eps} at level \code{m} under a stated rule. \code{"m_squared"} (\code{c m^2}) is the choice that buys absolute continuity of the limit; \code{"constant"} reproduces the DP-like, atomic behaviour; \code{"linear"} sits between them.
}
\value{
A list with \code{alpha}, \code{level} and \code{rule}.
}
\keyword{internal}
