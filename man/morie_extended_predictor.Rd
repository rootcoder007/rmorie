% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_extended_predictor}
\alias{morie_extended_predictor}
\title{morie_extended_predictor}
\usage{
morie_extended_predictor(n, X_E = NULL, X = NULL, X_EM = NULL) 
}
\description{
morie_extended_predictor
}
\arguments{
\item{n}{Argument \code{n}; see Usage.}
\item{X_E}{Argument \code{X_E}; see Usage.}
\item{X}{Argument \code{X}; see Usage.}
\item{X_EM}{Argument \code{X_EM}; see Usage.}
}
\value{
A list with \code{design}, \code{widths}, \code{n_columns}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
rmorie:::morie_extended_predictor(V)
}
