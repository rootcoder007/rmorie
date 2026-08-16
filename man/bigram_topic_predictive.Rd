% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{bigram_topic_predictive}
\alias{bigram_topic_predictive}
\title{bigram_topic_predictive}
\usage{
bigram_topic_predictive(N_ijk, N_jk, beta, m, prior = 1)
}
\arguments{
\item{N_ijk}{See Usage.}
\item{N_jk}{See Usage.}
\item{beta}{See Usage.}
\item{m}{See Usage.}
\item{prior}{Defaults to \code{1}.}
}
\description{
A step of the bigtm_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{predictive}, \code{prior}, \code{smoothed_by}.
}
\keyword{internal}
\examples{
bigram_topic_predictive(N_ijk = c(1, 2, 3, 4, 5, 6, 7, 8), N_jk = c(1, 2, 3, 4, 5, 6, 7, 8), beta = 0.5, m = 5L)
}
