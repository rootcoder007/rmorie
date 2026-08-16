% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{jtt_matrix}
\alias{jtt_matrix}
\title{jtt_matrix}
\usage{
jtt_matrix(pam = 200L, scale = 10)
}
\arguments{
\item{pam}{Defaults to \code{200L}.}
\item{scale}{Numeric; combined arithmetically in the body. Defaults to \code{10}.}
}
\description{
A step of the mafft_native implementation. Called by \code{.mafft_default_raw_matrix}, \code{morie_mafft}. See the file header for the source the module follows.
}
\value{
A list with \code{matrix}, \code{freqs}, \code{P}, \code{Q}, \code{pam}, \code{rate}.
}
\keyword{internal}
\examples{
jtt_matrix()
}
