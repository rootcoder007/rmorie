% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{cnvlfc_embed}
\alias{cnvlfc_embed}
\title{cnvlfc_embed}
\usage{
cnvlfc_embed(series, E = 2, tau = 1)
}
\arguments{
\item{series}{Passed to \code{.cnvlfc_embed}.}
\item{E}{Passed to \code{.cnvlfc_embed}. Defaults to \code{2}.}
\item{tau}{Passed to \code{.cnvlfc_embed}. Defaults to \code{1}.}
}
\description{
A step of the cnvlfc_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{points}, \code{index}, \code{E}, \code{tau}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
cnvlfc_embed(V)
}
