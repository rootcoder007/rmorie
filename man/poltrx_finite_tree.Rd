% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{poltrx_finite_tree}
\alias{poltrx_finite_tree}
\title{Draw a truncated Polya tree}
\usage{
poltrx_finite_tree(levels, c = 1, rule = "m_squared", rng = NULL, seed = 0)
}
\arguments{
\item{levels}{Truncation level \code{M}, integer \code{>= 1}.}
\item{c}{Positive scaling constant.}
\item{rule}{One of \code{"m_squared"}, \code{"constant"},   \code{"linear"}.}
\item{rng}{Optional generator environment; if \code{NULL} a fresh   one is built from \code{seed}.}
\item{seed}{Seed for the generator shared with the Python arm.}
}
\description{
Draws the branch probabilities \code{Y_eps ~ Beta(a, a)} down to a stated level by two Gamma draws per node. Truncated and honest about it: the partition depth is part of the model.
}
\value{
A list with \code{Y}, \code{levels}, \code{rule}, \code{c},   \code{n_nodes} and \code{note}.
}
\keyword{internal}
\examples{
poltrx_finite_tree(levels = 5L)
}
