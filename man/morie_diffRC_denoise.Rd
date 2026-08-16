% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_diffRC_denoise}
\alias{morie_diffRC_denoise}
\title{Run the reverse chain from t back to 0}
\usage{
morie_diffRC_denoise(x_t, model, schedule, t_start = NULL)
}
\arguments{
\item{x_t}{Numeric vector.}
\item{model}{A function \code{(x, t)} returning a numeric vector.}
\item{schedule}{A list as returned by \code{noise_schedule}.}
\item{t_start}{Optional integer start time.}
}
\description{
With \code{scale = 0} the schedule is the identity and this returns its input unchanged.
}
\value{
A list with the denoised estimate, the path and the   schedule summary.
}
\keyword{internal}
