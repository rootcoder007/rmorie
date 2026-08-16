% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_chemsc_smooth_block}
\alias{morie_chemsc_smooth_block}
\title{The block function convolved with a Gaussian, in closed form}
\usage{
morie_chemsc_smooth_block(d, d_ideal, d_max, sigma)
}
\arguments{
\item{d}{The deviation.}
\item{d_ideal}{The tolerance window.}
\item{d_max}{The maximum deviation.}
\item{sigma}{The Gaussian smearing width.}
}
\description{
A trapezoid against a normal density integrates to normal distribution and density values, so this is exact arithmetic and not a quadrature -- which matters here, because a numerical convolution would be the one part of the score that two implementations could disagree about while both being "right". With sigma at or below zero this is the plain block function, which is the honest limit rather than a special case.
}
\value{
A value in the unit interval.
}
\references{
  Eldridge, M.D., Murray, C.W., Auton, T.R., Paolini, G.V. and Mee,
    R.P. (1997) "Empirical scoring functions: I. The development of a
    fast empirical scoring function to estimate the binding affinity
    of ligands in receptor complexes." Journal of Computer-Aided
    Molecular Design 11(5), 425-445. doi:10.1023/A:1007996124545.
  Baxter, C.A., Murray, C.W., Clark, D.E., Westhead, D.R. and
    Eldridge, M.D. (1998) "Flexible docking using Tabu search and an
    empirical estimate of binding affinity." Proteins 33(3), 367-382.
  Cambridge Crystallographic Data Centre, "GOLD User Guide," sections
    8.4.1 to 8.4.6. Every default parameter value used here.
  Verdonk, M.L., Cole, J.C., Hartshorn, M.J., Murray, C.W. and Taylor,
    R.D. (2003) "Improved protein-ligand docking using GOLD."
    Proteins 52(4), 609-623.
}
\keyword{internal}
