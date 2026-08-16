% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.phylby_step}
\alias{.phylby_step}
\title{.phylby_step}
\usage{
.phylby_step(state, seqs, pi, prior_mean, partitions, rnd, beta, tune)
}
\arguments{
\item{state}{A list; the body reads \code{$rate}, \code{$rates}, \code{$tree} from it.}
\item{seqs}{Passed to \code{.phylby_log_posterior}.}
\item{pi}{Passed to \code{.phylby_log_posterior}.}
\item{prior_mean}{Passed to \code{.phylby_log_posterior}.}
\item{partitions}{Passed to \code{.phylby_log_posterior}.}
\item{rnd}{Accepted by the signature and not used anywhere in the body.}
\item{beta}{Numeric; combined arithmetically in the body.}
\item{tune}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the phylby_native implementation. Called by \code{morie_phylby}. See the file header for the source the module follows.
}
\value{
A list with \code{state}, \code{accepted}.
}
\keyword{internal}
