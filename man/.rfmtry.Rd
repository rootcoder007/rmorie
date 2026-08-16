% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rfmtry}
\alias{.rfmtry}
\title{.rfmtry}
\usage{
.rfmtry(p, kind = "regression")
}
\arguments{
\item{p}{Numeric; passed to \code{sqrt}.}
\item{kind}{Defaults to \code{"regression"}.}
}
\description{
A step of the mvsml_rf_shared implementation. Called by \code{Rfmdi}, \code{Rfmlt}, \code{Rfpmi}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.rfmtry(V)
}
