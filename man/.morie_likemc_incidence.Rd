% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_likemc_incidence}
\alias{.morie_likemc_incidence}
\title{.morie_likemc_incidence}
\usage{
.morie_likemc_incidence(beta, gamma, S0, I0, N, n_steps, dt = 1)
}
\arguments{
\item{beta}{Numeric; combined arithmetically in the body.}
\item{gamma}{Numeric; combined arithmetically in the body.}
\item{S0}{See Usage.}
\item{I0}{See Usage.}
\item{N}{Numeric; combined arithmetically in the body.}
\item{n_steps}{A count; the body uses it as \code{seq_len(...)}.}
\item{dt}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
}
\description{
A step of the likemc_native implementation. Called by \code{morie_likemc}. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
