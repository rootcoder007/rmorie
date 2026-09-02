% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_kmer_diagonals}
\alias{morie_sv_dl_kmer_diagonals}
\title{morie_sv_dl_kmer_diagonals}
\usage{
morie_sv_dl_kmer_diagonals(read, ref, k = 7, k_min = 3, require_half = TRUE) 
}
\arguments{
\item{read}{Coerced to character by the body, with \code{as.character}.}
\item{ref}{Coerced to character by the body, with \code{as.character}.}
\item{k}{Numeric; combined arithmetically in the body. Defaults to \code{7}.}
\item{k_min}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{3}.}
\item{require_half}{A flag; the body branches on it. Defaults to \code{TRUE}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_refine_breakpoint}. See the file header for the source the module follows.
}
\value{
The value of \code{[}.
}
\keyword{internal}
\examples{
morie_sv_dl_kmer_diagonals(read = 5L, ref = c(1, 2, 3, 4, 5, 6, 7, 8))
}
