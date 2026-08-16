% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.schab_st_gneiting}
\alias{.schab_st_gneiting}
\title{.schab_st_gneiting}
\usage{
.schab_st_gneiting(h, k, sigma2 = 1, a = 1, c = 1, alpha = 1, beta = 1, gamma = 1, d = 2)
}
\arguments{
\item{h}{Passed to \code{.schab_st_as_lags}.}
\item{k}{Passed to \code{.schab_st_as_lags}.}
\item{sigma2}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{a}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{c}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{alpha}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{beta}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{gamma}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{d}{Numeric; combined arithmetically in the body. Defaults to \code{2}.}
}
\description{
A step of the schab_st_shared implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A numeric value.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
.schab_st_gneiting(V, V)
}
