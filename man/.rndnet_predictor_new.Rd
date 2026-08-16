% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rndnet_predictor_new}
\alias{.rndnet_predictor_new}
\title{Private: predictor with the same random feature layer but a trainable}
\usage{
.rndnet_predictor_new(n_in, n_hidden, n_out, rng, scale = 1)
}
\arguments{
\item{n_in}{Passed to \code{.rndnet_random_features_new}.}
\item{n_hidden}{A count; the body uses it as \code{matrix(...)}.}
\item{n_out}{A count; the body uses it as \code{matrix(...)}.}
\item{rng}{Passed to \code{.rndnet_random_features_new}.}
\item{scale}{Passed to \code{.rndnet_random_features_new}. Defaults to \code{1}.}
}
\description{
output weight matrix W, updated by plain SGD on the MSE.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
