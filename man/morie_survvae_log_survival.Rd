% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_log_survival}
\alias{morie_survvae_log_survival}
\title{Log survival of one expert -- what a censored case needs}
\usage{
morie_survvae_log_survival(t, shape, scale, primitive = "weibull") 
}
\arguments{
\item{t}{Time, must be non-negative.}
\item{shape}{Shape parameter.}
\item{scale}{Scale parameter.}
\item{primitive}{One of \code{"weibull"} or \code{"lognormal"}.}
}
\description{
Closed-form log survival of the chosen primitive, used by \code{morie_survvae_elbo} for censored observations and by \code{morie_survvae_predict_survival}. \code{t = 0} returns \code{0} for both primitives (an event at time zero has survival one by convention). The Python arm does not validate shape/scale here; invalid values propagate as arithmetic errors and we mirror that.
}
\value{
Numeric scalar, the log survival.
}
\keyword{internal}
\examples{
morie_survvae_log_survival(t = 5L, shape = c(1, 2, 3, 4, 5, 6, 7, 8), scale = TRUE)
}
