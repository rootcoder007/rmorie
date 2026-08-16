% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{beta_polak_ribiere}
\alias{beta_polak_ribiere}
\title{beta_polak_ribiere}
\usage{
beta_polak_ribiere(g_new, g_old, plus = FALSE)
}
\arguments{
\item{g_new}{Numeric; combined arithmetically in the body.}
\item{g_old}{Numeric; combined arithmetically in the body.}
\item{plus}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
A step of the cgnonl_native implementation. Called by \code{.cgnonl_beta}. See the file header for the source the module follows.
}
\value{
One of two values, depending on the branch taken.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
beta_polak_ribiere(V, V)
}
