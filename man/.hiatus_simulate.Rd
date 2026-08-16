% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.hiatus_simulate}
\alias{.hiatus_simulate}
\title{.hiatus_simulate}
\usage{
.hiatus_simulate(beta, nu, mu, sigma, S0 = NULL, I0 = NULL, t_end = 2000, dt = 0.05, mutation = 0, record_every = 100L)
}
\arguments{
\item{beta}{Passed to \code{._check}.}
\item{nu}{Passed to \code{._check}.}
\item{mu}{Passed to \code{._check}.}
\item{sigma}{Passed to \code{._check}.}
\item{S0}{Defaults to \code{NULL}.}
\item{I0}{Defaults to \code{NULL}.}
\item{t_end}{Numeric; combined arithmetically in the body. Defaults to \code{2000}.}
\item{dt}{Defaults to \code{0.05}.}
\item{mutation}{Defaults to \code{0}.}
\item{record_every}{Defaults to \code{100L}.}
}
\description{
A step of the hiatus_native implementation. Called by \code{morie_hiatus}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{S}, \code{I}, \code{t}, \code{S_traj}, \code{I_traj}, \code{n_strains}, \code{R0}, \code{n_variables}, \code{n_variables_history_based}, \code{surviving}, \code{method}.
}
\keyword{internal}
