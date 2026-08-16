% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Fhn}
\alias{Fhn}
\title{Fhn: FitzHugh-Nagumo two-variable excitable-medium neuron model}
\usage{
Fhn(duration = 200, dt = 0.01, I_ext = 0.5, a = 0.7, b = 0.8, eps = 0.08, v0 = -1.2, w0 = -0.6, stim_start = 0, stim_stop = NULL)
}
\arguments{
\item{duration}{Defaults to \code{200}.}
\item{dt}{Defaults to \code{0.01}.}
\item{I_ext}{Defaults to \code{0.5}.}
\item{a}{Defaults to \code{0.7}.}
\item{b}{Defaults to \code{0.8}.}
\item{eps}{Defaults to \code{0.08}.}
\item{v0}{Defaults to \code{-1.2}.}
\item{w0}{Defaults to \code{-0.6}.}
\item{stim_start}{Defaults to \code{0}.}
\item{stim_stop}{Defaults to \code{NULL}.}
}
\description{
Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t}, \code{v}, \code{w}, \code{peak}, \code{min}, \code{n_spikes}, \code{spike_times}, \code{period}, \code{a}, \code{b}, \code{eps}, \code{I_ext}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
Fhn()
}
