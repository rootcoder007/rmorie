% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_miprgr_round_relaxation}
\alias{morie_miprgr_round_relaxation}
\title{Round the relaxation and report what it gives}
\usage{
morie_miprgr_round_relaxation(x, A, b, integer_vars)
}
\arguments{
\item{x}{Relaxed solution.}
\item{A}{Original inequalities.}
\item{b}{Right-hand side.}
\item{integer_vars}{Indices that must be integral.}
}
\description{
Round the relaxation and report what it gives
}
\value{
A list with x, feasible, violations, note.
}
\keyword{internal}
