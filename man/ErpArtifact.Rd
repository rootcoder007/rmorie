% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{ErpArtifact}
\alias{ErpArtifact}
\title{Section 3.5: averaging M aligned epochs raises the SNR by sqrt(M).  A}
\usage{
ErpArtifact(epochs, reject = NULL)
}
\arguments{
\item{epochs}{See Usage.}
\item{reject}{Defaults to \code{NULL}.}
}
\description{
large artifact is NOT zero-mean over the epochs it contaminates, so it does not average away at any M; the remedy is to reject those epochs first.  Averaging without rejection and quoting the sqrt(M) gain overstates the result by exactly the artifact that survived.
}
\value{
A list with \code{average}, \code{sd}, \code{m}, \code{m_kept}, \code{rejected}, \code{n_rejected}, \code{peaks}, \code{n}, \code{snr_gain}, \code{snr_gain_db}, \code{artifact_factor}, \code{method}.
}
\keyword{internal}
