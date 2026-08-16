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
\item{y}{A vector; its length is taken.}
\item{mu}{Numeric; combined arithmetically in the body.}
\item{n}{Numeric; combined arithmetically in the body. Defaults to \code{sum(weights)}.}
\item{weights}{Numeric; combined arithmetically in the body.}
\item{limit}{Defaults to \code{10}.}
\item{eps}{Defaults to \code{.Machine$double.eps^0.25}.}
}
\description{
A step of the mass_native implementation. Called by \code{morie_glm_nb}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
