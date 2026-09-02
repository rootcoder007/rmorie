% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{PsdHz}
\alias{PsdHz}
\title{Section 6.4: bin k sits at k fs / N, and a band power is the sum of}
\usage{
PsdHz(psd, fs, n = NULL, bands = NULL) 
}
\arguments{
\item{psd}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{n}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{bands}{Optional; may be \code{NULL}. Iterated over elementwise, with \code{lapply}.}
}
\description{
its bins times the bin width fs/N.  The bin width turns a density into a power; omitting it leaves a quantity that changes with the record length, so two band powers at different N are not comparable.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
\examples{
PsdHz(psd = c(1, 2, 3, 4, 5, 6, 7, 8), fs = 5L)
}
