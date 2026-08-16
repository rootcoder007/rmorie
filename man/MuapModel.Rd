% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{MuapModel}
\alias{MuapModel}
\title{MuapModel: motor unit action potential as summed single-fibre}
\usage{
MuapModel(t, n_fibers = 25, conduction_vel = 4, spread_mm = 3, amp_uV = 8, width_ms = 1, phases = 3)
}
\arguments{
\item{t}{See Usage.}
\item{n_fibers}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{25}.}
\item{conduction_vel}{Numeric; combined arithmetically in the body. Defaults to \code{4}.}
\item{spread_mm}{Numeric; combined arithmetically in the body. Defaults to \code{3}.}
\item{amp_uV}{Numeric; combined arithmetically in the body. Defaults to \code{8}.}
\item{width_ms}{Numeric; combined arithmetically in the body. Defaults to \code{1}.}
\item{phases}{Defaults to \code{3}.}
}
\description{
potentials Part of the rangayyan_phys implementation; see the file header for the source it follows.
}
\value{
A list with \code{t_ms}, \code{muap_uV}, \code{peak_to_peak_uV}, \code{peak_uV}, \code{duration_ms}, \code{n_phases_observed}, \code{delays_ms}, \code{n_fibers}, \code{conduction_vel_m_s}, \code{in_normal_duration_band}, \code{units}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
MuapModel(V)
}
