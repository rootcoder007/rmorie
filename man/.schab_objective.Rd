% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_objective}
\alias{.schab_objective}
\title{.schab_objective}
\usage{
.schab_objective(kind, lags, ghat, counts, model)
}
\arguments{
\item{kind}{Passed to \code{identical}.}
\item{lags}{A vector; indexed elementwise.}
\item{ghat}{A vector; indexed elementwise.}
\item{counts}{A vector; indexed elementwise.}
\item{model}{Passed to \code{.sp_semivariogram}.}
}
\description{
A step of the schab_fit_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{function}.
}
\keyword{internal}
\examples{
.schab_objective(kind = c(1, 2, 3, 4, 5, 6, 7, 8), lags = 5L, ghat = c(1, 2, 3, 4, 5, 6, 7, 8), counts = c(1, 2, 3, 4, 5, 6, 7, 8), model = c(1, 2, 3, 4, 5, 6, 7, 8))
}
