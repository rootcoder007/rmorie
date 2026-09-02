% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MatchedFilt}
\alias{MatchedFilt}
\title{MatchedFilt}
\usage{
MatchedFilt(ref, x = NULL, noise_psd = NULL, freqs = NULL, t0 = NULL, gain = 1, 
    dt = 1) 
}
\arguments{
\item{ref}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{x}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{noise_psd}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{freqs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{t0}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{gain}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{dt}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
}
\description{
A step of the rangayyan_corr implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MatchedFilt(V)
}
