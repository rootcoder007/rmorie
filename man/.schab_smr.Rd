% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_smr}
\alias{.schab_smr}
\title{.schab_smr}
\usage{
.schab_smr(counts, expected)
}
\arguments{
\item{counts}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{expected}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the schab_glmm_shared implementation. Called by \code{spbayr}, \code{spbym}. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_smr(V, V)
}
