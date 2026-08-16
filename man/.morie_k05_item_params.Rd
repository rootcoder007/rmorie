% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_k05_item_params}
\alias{.morie_k05_item_params}
\title{.morie_k05_item_params}
\usage{
.morie_k05_item_params(a, b, c = NULL, upper = NULL)
}
\arguments{
\item{a}{Passed to \code{rep_to}.}
\item{b}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{c}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{upper}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
}
\description{
A step of the k05_tranche2 implementation. Called by \code{morie_tcc}, \code{morie_test_information}. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{b}, \code{c}, \code{u}, \code{n}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.morie_k05_item_params(V, V)
}
