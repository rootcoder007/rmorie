% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{AICorder}
\alias{AICorder}
\title{Rangayyan eq. (7.60): I(P) = log(eps_P) + 2P/Ne, Ne = 0.4 N for a}
\usage{
AICorder(prediction_errors, n_samples, window = "hamming")
}
\arguments{
\item{prediction_errors}{See Usage.}
\item{n_samples}{See Usage.}
\item{window}{Defaults to \code{"hamming"}.}
}
\description{
Hamming window -- the EFFECTIVE sample count after windowing, which is what distinguishes this from the textbook N log(sigma^2) + 2p.
}
\value{
A list with \code{order}, \code{criterion}, \code{n_effective}, \code{method}.
}
\keyword{internal}
