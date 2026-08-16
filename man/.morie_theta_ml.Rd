% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_theta_ml}
\alias{.morie_theta_ml}
\title{Theta MLE by Fisher scoring (reproduces MASS::theta.ml)}
\usage{
.morie_theta_ml(y, mu, n = sum(weights), weights, limit = 10, eps = .Machine$double.eps^0.25)
}
\arguments{
\item{y}{See Usage.}
\item{mu}{See Usage.}
\item{n}{Defaults to \code{sum(weights)}.}
\item{weights}{See Usage.}
\item{limit}{Defaults to \code{10}.}
\item{eps}{Defaults to \code{.Machine$double.eps^0.25}.}
}
\description{
Part of the mass_native implementation; see the file header for the source it follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
