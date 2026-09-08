% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_progeny_required}
\alias{morie_progeny_required}
\title{Number of progeny for even odds of detection}
\usage{
morie_progeny_required(var_qtl, var_residual, alpha = 0.05) 
}
\arguments{
\item{var_qtl}{QTL variance, strictly positive for detection.}
\item{var_residual}{Residual variance, strictly positive.}
\item{alpha}{Significance level in \code{(0, 1)}, default 0.05.}
}
\description{
Equation (6) of Lander & Botstein (1989): the ratio of the single-marker threshold to the ELOD. Refuses the degenerate case of zero QTL variance.
}
\value{
A named list with keys \code{n}, \code{threshold},   \code{elod}.
}
\keyword{internal}
\examples{
morie_progeny_required(var_qtl = 5L, var_residual = 5L)
}
