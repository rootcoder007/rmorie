% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{decompose_effects}
\alias{decompose_effects}
\title{Direct and spillover effects, separately}
\usage{
decompose_effects(Q_fn, W, friends, p_high = 1, p_low = 0, seed = 0, draws = 200) 
}
\description{
Direct: own exposure varies with the neighbourhood held at ``p_low``. Spillover: own held at ``p_low`` while the neighbourhood varies. Reporting only the total hides which mechanism produced it.
}
\arguments{
\item{Q_fn}{Argument \code{Q_fn}; see Usage.}
\item{W}{Argument \code{W}; see Usage.}
\item{friends}{Argument \code{friends}; see Usage.}
\item{p_high}{Argument \code{p_high}; see Usage.}
\item{p_low}{Argument \code{p_low}; see Usage.}
\item{seed}{Argument \code{seed}; see Usage.}
\item{draws}{Argument \code{draws}; see Usage.}
}
\value{
A list with \code{estimate}, \code{direct}, \code{spillover}, \code{total}, \code{method}, \code{note}.
}
\keyword{internal}
