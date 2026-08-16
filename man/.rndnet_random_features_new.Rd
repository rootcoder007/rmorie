% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.rndnet_random_features_new}
\alias{.rndnet_random_features_new}
\title{Private: frozen random target net x -> tanh(W1 x + b1) W2, W fixed at}
\usage{
.rndnet_random_features_new(n_in, n_hidden, n_out, rng, scale = 1)
}
\arguments{
\item{n_in}{See Usage.}
\item{n_hidden}{See Usage.}
\item{n_out}{See Usage.}
\item{rng}{See Usage.}
\item{scale}{Defaults to \code{1}.}
}
\description{
init Part of the rndnet_native implementation; see the file header for the source it follows.
}
\value{
The value of \code{e}, as built in the body.
}
\keyword{internal}
