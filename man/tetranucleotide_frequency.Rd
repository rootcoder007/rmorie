% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{tetranucleotide_frequency}
\alias{tetranucleotide_frequency}
\title{tetranucleotide_frequency}
\usage{
tetranucleotide_frequency(seq, kk = 4L, canonical = TRUE)
}
\arguments{
\item{seq}{See Usage.}
\item{kk}{Defaults to \code{4L}.}
\item{canonical}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the metabd_native implementation. Called by \code{morie_metabd}. See the file header for the source the module follows.
}
\value{
A list with \code{frequency}, \code{vector}, \code{kmers}, \code{n_kmers}, \code{canonical}.
}
\keyword{internal}
