% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecMoments}
\alias{SpecMoments}
\title{Eqs (6.32)-(6.43).  The sums run over ONE HALF of the periodic PSD:}
\usage{
SpecMoments(psd, fs = 1, freqs = NULL) 
}
\arguments{
\item{psd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{1}.}
\item{freqs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
for a real signal S(k) is even-symmetric about fs/2, so summing the whole period double-counts every frequency and pins the mean at fs/2 regardless of the signal.
}
\value{
A list with \code{total_power}, \code{mean_frequency}, \code{median_frequency}, \code{variance}, \code{bandwidth}, \code{skewness}, \code{kurtosis}, \code{fm3}, \code{fm4}, \code{n_bins}, \code{uniformity}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SpecMoments(V)
}
