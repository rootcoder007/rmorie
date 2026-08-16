% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{preliminary_estimates}
\alias{preliminary_estimates}
\title{preliminary_estimates}
\usage{
preliminary_estimates(w, s = 12)
}
\arguments{
\item{w}{See Usage.}
\item{s}{Defaults to \code{12}.}
}
\description{
A step of the sarima_native implementation. Called by \code{.sarima_fit}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{theta}, \code{Theta}, \code{r_1}, \code{r_s}, \code{method}.
}
\keyword{internal}
