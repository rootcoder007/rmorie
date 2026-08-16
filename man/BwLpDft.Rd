% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{BwLpDft}
\alias{BwLpDft}
\title{Eq (3.146), valid for k = 0..K/2 with the upper half a reflection,}
\usage{
BwLpDft(K, kc = NULL, N = 2, fc = NULL, fs = NULL)
}
\arguments{
\item{K}{Coerced to integer by the body, with \code{as.integer}.}
\item{kc}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
\item{N}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{2}.}
\item{fc}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{fs}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
H(k) = H(K-k).  The book defines kc = ceil(K wc/ws) and that CEILING matters: rounding down puts the realized cutoff below the request.
}
\value{
A list with \code{squared_magnitude}, \code{magnitude}, \code{half_spectrum}, \code{K}, \code{kc}, \code{N}, \code{dc_gain}, \code{reflected}, \code{cutoff_index_uses_a_ceiling}, \code{method}.
}
\keyword{internal}
\examples{
BwLpDft(16, kc = 4, N = 2)
}
