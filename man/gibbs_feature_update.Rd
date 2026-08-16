% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gibbs_feature_update}
\alias{gibbs_feature_update}
\title{gibbs_feature_update}
\usage{
gibbs_feature_update(Z, i, kk, likelihood, alpha)
}
\arguments{
\item{Z}{A matrix; passed to \code{as.matrix}.}
\item{i}{Numeric; combined arithmetically in the body.}
\item{kk}{See Usage.}
\item{likelihood}{Accepted by the signature and not used anywhere in the body.}
\item{alpha}{Accepted by the signature and not used anywhere in the body.}
}
\description{
A step of the ibpfa_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{p_on}, \code{prior}, \code{z}, \code{note}.
}
\keyword{internal}
