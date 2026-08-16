% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sv_dl_gotoh_score_vectors}
\alias{morie_sv_dl_gotoh_score_vectors}
\title{morie_sv_dl_gotoh_score_vectors}
\usage{
morie_sv_dl_gotoh_score_vectors(consensus, ref, match = 1, mismatch = -2, gap_open = -4, gap_extend = -1)
}
\arguments{
\item{consensus}{Coerced to character by the body, with \code{as.character}.}
\item{ref}{Coerced to character by the body, with \code{as.character}.}
\item{match}{Passed to \code{.sv_dl_gotoh}. Defaults to \code{1}.}
\item{mismatch}{Passed to \code{.sv_dl_gotoh}. Defaults to \code{-2}.}
\item{gap_open}{Passed to \code{.sv_dl_gotoh}. Defaults to \code{-4}.}
\item{gap_extend}{Passed to \code{.sv_dl_gotoh}. Defaults to \code{-1}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_refine_breakpoint}. See the file header for the source the module follows.
}
\value{
A list with \code{f}, \code{f_at}, \code{r}, \code{r_at}.
}
\keyword{internal}
\examples{
morie_sv_dl_gotoh_score_vectors(consensus = 5L, ref = 5L)
}
