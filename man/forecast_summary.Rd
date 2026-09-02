% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{forecast_summary}
\alias{forecast_summary}
\title{forecast_summary}
\usage{
forecast_summary(token_probs, bins, quantiles = c(0.1, 0.5, 0.9)) 
}
\arguments{
\item{token_probs}{Passed to \code{.chronos_vec}.}
\item{bins}{A list; the body reads \code{$centers} from it.}
\item{quantiles}{Defaults to \code{c(0.1, 0.5, 0.9)}.}
}
\description{
A step of the chronos_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{mean}, \code{quantiles}, \code{mode}, \code{note}.
}
\keyword{internal}
\examples{
b <- uniform_bins(-3, 3, n_bins = 8L)
forecast_summary(rep(1 / 8, 8), b)
}
