% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Dpll}
\alias{Dpll}
\title{\\' Decide satisfiability of a CNF formula by DPLL}
\usage{
Dpll(cnf)
}
\arguments{
\item{cnf}{See Usage.}
}
\description{
\\' \\' Unit propagation, pure literal elimination, then a split on the \\' lowest-indexed unassigned variable trying TRUE first.  The index rule \\' is deterministic, so decision and propagation counts are reproducible \\' and match the Python arm exactly. \\' \\' @param cnf list of integer vectors, DIMACS literals (v, -v); 0 is not \\' a literal \\' @return list(satisfiable, assignment, model, n_vars, n_clauses, \\' decisions, propagations, pure_literals, method) \\'
}
\value{
A list with \code{satisfiable}, \code{assignment}, \code{model}, \code{n_vars}, \code{n_clauses}, \code{decisions}, \code{propagations}, \code{pure_literals}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
Dpll(V)
}
