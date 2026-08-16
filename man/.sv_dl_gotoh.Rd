% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sv_dl_gotoh}
\alias{.sv_dl_gotoh}
\title{.sv_dl_gotoh}
\usage{
.sv_dl_gotoh(query, ref, match = 1, mismatch = -2, gap_open = -4, gap_extend = -1)
}
\arguments{
\item{query}{Character; passed to \code{strsplit}.}
\item{ref}{Character; passed to \code{strsplit}.}
\item{match}{Defaults to \code{1}.}
\item{mismatch}{Defaults to \code{-2}.}
\item{gap_open}{Numeric; combined arithmetically in the body. Defaults to \code{-4}.}
\item{gap_extend}{Numeric; combined arithmetically in the body. Defaults to \code{-1}.}
}
\description{
A step of the sv_dl_native implementation. Called by \code{morie_sv_dl_gotoh_score_vectors}. See the file header for the source the module follows.
}
\value{
A list with \code{best}, \code{best_at}.
}
\keyword{internal}
\examples{
S <- c("a", "b", "c")
.sv_dl_gotoh(S, S)
}
