% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_miprgr_solve_relaxation}
\alias{morie_miprgr_solve_relaxation}
\title{LP relaxation at one node}
\usage{
morie_miprgr_solve_relaxation(A, b, c, bounds = list(), n = NULL, maximise = TRUE, solver = "simplex")
}
\arguments{
\item{A}{List of row vectors of the original inequality system.}
\item{b}{Right-hand side.}
\item{c}{Objective coefficients.}
\item{bounds}{List of list(var, sense, value) for branch bounds.}
\item{n}{Number of decision variables.}
\item{maximise}{Maximise if TRUE.}
\item{solver}{"simplex" or "interior".}
}
\description{
Solves the LP relaxation by simplex (the only choice safe for branch and bound) or by a Mehrotra-style interior point method (better on large sparse LPs, but unsafe once a branch collapses the region to a single point, so kept but not default).
}
\value{
A list with feasible, x, value, note.
}
\keyword{internal}
