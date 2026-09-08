% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_evt_xi_ci_profile}
\alias{morie_evt_xi_ci_profile}
\title{Profile-likelihood CI for the shape xi (Coles 2001 sec. 2.6.5)}
\usage{
morie_evt_xi_ci_profile(x, alpha = 0.05, model = "gev") 
}
\arguments{
\item{x}{data (block maxima or excesses); model "gev" or "gpd"}
\item{alpha}{1 - confidence level}

\item{model}{Argument \code{model}; see Usage.}
}
\description{
Profile-likelihood CI for the shape xi (Coles 2001 sec. 2.6.5)
}
\value{
A list with \code{ci_lo}, \code{ci_hi}, \code{xi_hat}.
}
\keyword{internal}
\examples{
u <- runif(600)
x <- morie_evt_gev_quantile(u, 10, 2, 0.1)
morie_evt_xi_ci_profile(x)
}
