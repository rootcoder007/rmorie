% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_unclr_Phi}
\alias{morie_unclr_Phi}
\title{morie_unclr_Phi}
\usage{
morie_unclr_Phi(z)
}
\arguments{
\item{z}{See Usage.}
}
\description{
A step of the unclr implementation. Called by \code{morie_unclr_lr_impute}. See the file header for the source the module follows.
}
\value{
The value of \code{stats::pnorm}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_unclr_Phi(V)
}
