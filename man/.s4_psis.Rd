% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_psis}
\alias{.s4_psis}
\title{# Pareto-smoothed importance sampling on log weights}
\usage{
.s4_psis(lw)
}
\arguments{
\item{lw}{A vector; its length is taken and its elements indexed.}
}
\description{
A step of the s04core implementation. Called by \code{Infcrt}, \code{Khatd}, \code{Loopr}. See the file header for the source the module follows.
}
\value{
A list with \code{lw}, \code{k}.
}
\keyword{internal}
