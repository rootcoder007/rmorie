% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{softplus}
\alias{softplus}
\title{softplus}
\usage{
softplus(z) 
}
\arguments{
\item{z}{Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the mambss_native implementation. Called by \code{morie_geron_blip_itm_itc}, \code{selective_scan}. See the file header for the source the module follows.
}
\value{
The value of \code{log1p}.
}
\keyword{internal}
\examples{
softplus(z = 5L)
}
