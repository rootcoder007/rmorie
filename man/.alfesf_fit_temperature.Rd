% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.alfesf_fit_temperature}
\alias{.alfesf_fit_temperature}
\title{.alfesf_fit_temperature}
\usage{
.alfesf_fit_temperature(L, y, iters = 200L, lr = 0.5)
}
\arguments{
\item{L}{A matrix; indexed by row and column.}
\item{y}{A vector; indexed elementwise.}
\item{iters}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{200L}.}
\item{lr}{Numeric; combined arithmetically in the body. Defaults to \code{0.5}.}
}
\description{
A step of the alfesf_native implementation. Called by \code{morie_alfesf_esmfold_confidence}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
.alfesf_fit_temperature(M, V)
}
