% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{Shedcurve}
\alias{Shedcurve}
\title{Piecewise log-linear shedding curve (standard)}
\usage{
Shedcurve(days, load, t_peak, t_plateau) 
}
\description{
Piecewise log-linear shedding curve (standard)
}
\arguments{
\item{days}{Argument \code{days}; see Usage.}
\item{load}{Argument \code{load}; see Usage.}
\item{t_peak}{Argument \code{t_peak}; see Usage.}
\item{t_plateau}{Argument \code{t_plateau}; see Usage.}
}
\value{
A list with \code{rise_slope}, \code{rise_intercept}, \code{plateau_level}, \code{decay_slope}, \code{decay_intercept}, \code{peak_load}, \code{peak_day}, \code{n}, \code{n_rise}, \code{n_plateau}, \code{n_decay}.
}
\keyword{internal}
\examples{
set.seed(2)
days <- 0:14
load <- 10^(c(seq(2, 6, length.out = 5), seq(5.8, 4, length.out = 5),
              rep(3.9, 5)) + rnorm(15, 0, 0.05))
rmorie:::Shedcurve(days, load, t_peak = 4, t_plateau = 9)
}
