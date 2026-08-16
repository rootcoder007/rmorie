% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prewhiten_var}
\alias{prewhiten_var}
\title{prewhiten_var}
\usage{
prewhiten_var(v, order = 1, cap = 0.97, adjust = TRUE)
}
\arguments{
\item{v}{A matrix; passed to \code{as.matrix}.}
\item{order}{A count; the body uses it as \code{vector(...)}. Defaults to \code{1}.}
\item{cap}{Numeric; combined arithmetically in the body. Defaults to \code{0.97}.}
\item{adjust}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the andmnh_native implementation. Called by \code{andrews_monahan_hac}. See the file header for the source the module follows.
}
\value{
A list with \code{A}, \code{residuals}, \code{D}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
prewhiten_var(V)
}
