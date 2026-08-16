% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MfTfEeg}
\alias{MfTfEeg}
\title{Eq (4.55), the same expression as eq (4.48) restated in Section}
\usage{
MfTfEeg(X, freqs, t0, gain = 1)
}
\arguments{
\item{X}{See Usage.}
\item{freqs}{See Usage.}
\item{t0}{See Usage.}
\item{gain}{Defaults to \code{1}.}
}
\description{
4.6.2, so it delegates.  What is specific here is the DFT caveat.
}
\value{
The value of \code{r}, as built in the body.
}
\keyword{internal}
\examples{
MfTfEeg(X = c(1, 2, 3, 4, 5, 6, 7, 8), freqs = c(1, 2, 3, 4, 5, 6, 7, 8), t0 = c(1, 2, 3, 4, 5, 6, 7, 8))
}
