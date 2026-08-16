% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{univariate_mcd}
\alias{univariate_mcd}
\title{univariate_mcd}
\usage{
univariate_mcd(values, h = NULL, consistent = TRUE)
}
\arguments{
\item{values}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{h}{Optional; may be \code{NULL}. Numeric; combined arithmetically in the body.}
\item{consistent}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the robpca_native implementation. Called by \code{.robpca_od_cutoff}, \code{outlyingness}. See the file header for the source the module follows.
}
\value{
A vector, from \code{c}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
univariate_mcd(V)
}
