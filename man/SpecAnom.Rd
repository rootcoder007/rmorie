% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{SpecAnom}
\alias{SpecAnom}
\title{Spectral residual saliency (Hou & Zhang 2007).  NOT in Schabenberger}
\usage{
SpecAnom(x, q = 3L) 
}
\arguments{
\item{x}{Passed to \code{.morie_spx_chkv}.}
\item{q}{Numeric; combined arithmetically in the body. Defaults to \code{3L}.}
}
\description{
& Gotway.  THE PHASE IS KEPT: rebuilding from the residual amplitude with the ORIGINAL phase is the whole mechanism.  The moving average is circular, matching the periodicity of the DFT.
}
\value{
A list with \code{saliency}, \code{peak}, \code{peak_index}, \code{residual}, \code{log_amplitude}, \code{floored}, \code{phase_is_preserved}, \code{q}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
SpecAnom(V)
}
