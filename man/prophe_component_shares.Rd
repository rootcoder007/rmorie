% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{prophe_component_shares}
\alias{prophe_component_shares}
\title{prophe_component_shares}
\usage{
prophe_component_shares(components) 
}
\arguments{
\item{components}{A vector; indexed elementwise.}
}
\description{
A step of the prophe_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{sd}, \code{relative}, \code{ranked}, \code{note}.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
prophe_component_shares(D)
}
