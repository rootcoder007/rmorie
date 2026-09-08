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
\item{epochs}{Iterated over elementwise, with \code{lapply}.}
\item{reject}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
large artifact is NOT zero-mean over the epochs it contaminates, so it does not average away at any M; the remedy is to reject those epochs first.  Averaging without rejection and quoting the sqrt(M) gain overstates the result by exactly the artifact that survived.
}
\value{
A list with \code{average}, \code{sd}, \code{m}, \code{m_kept}, \code{rejected}, \code{n_rejected}, \code{peaks}, \code{n}, \code{snr_gain}, \code{snr_gain_db}, \code{artifact_factor}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
ErpArtifact(V)
}
