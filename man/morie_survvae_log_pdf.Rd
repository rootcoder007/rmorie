% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_survvae_log_pdf}
\alias{morie_survvae_log_pdf}
\title{Log density of one expert}
\usage{
morie_survvae_log_pdf(t, shape, scale, primitive = "weibull")
}
\arguments{
\item{t}{Time, must be positive.}
\item{shape}{Shape parameter, must be positive.}
\item{scale}{Scale parameter, must be positive.}
\item{primitive}{One of \code{"weibull"} or \code{"lognormal"}.}
}
\description{
Closed-form log density of the chosen primitive, used by \code{morie_survvae_elbo} for uncensored observations.
}
\value{
Numeric scalar, the log density.
}
\keyword{internal}
\examples{
morie_survvae_log_pdf(t = 5L, shape = 5L, scale = TRUE)
}
