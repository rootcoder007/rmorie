% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{focal_loss}
\alias{focal_loss}
\title{focal_loss}
\usage{
focal_loss(prob, target, gamma = 2, alpha = 0.25) 
}
\arguments{
\item{prob}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{target}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{gamma}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{2}.}
\item{alpha}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.25}.}
}
\description{
A step of the samdec_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{loss}, \code{modulating}, \code{gamma}, \code{note}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
focal_loss(V, V)
}
