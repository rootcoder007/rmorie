% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{nbeats_block}
\alias{nbeats_block}
\title{nbeats_block}
\usage{
nbeats_block(window, horizon, kind = "generic", degree = 2, harmonics = 3, 
    ridge = 1e-08) 
}
\arguments{
\item{window}{A vector; its length is taken.}
\item{horizon}{Coerced to integer by the body, with \code{as.integer}.}
\item{kind}{One of \code{"generic"}, \code{"seasonality"}, \code{"trend"}. Defaults to \code{"generic"}.}
\item{degree}{Passed to \code{nbeats_trend_basis}. Defaults to \code{2}.}
\item{harmonics}{Passed to \code{nbeats_seasonality_basis}. Defaults to \code{3}.}
\item{ridge}{Passed to \code{nbeats_lstsq}. Defaults to \code{1e-08}.}
}
\description{
A step of the nbeats_native implementation. Called by \code{nbeats_stack}. See the file header for the source the module follows.
}
\value{
A list with \code{backcast}, \code{forecast}, \code{theta}.
}
\keyword{internal}
\examples{
nbeats_block(window = 5L, horizon = 5L)
}
