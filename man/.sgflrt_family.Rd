% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.sgflrt_family}
\alias{.sgflrt_family}
\title{Return (linkinv, V, W, loglik). V is the variance function and W =}
\usage{
.sgflrt_family(family, disp = 1)
}
\arguments{
\item{family}{One of \code{"binomial"}, \code{"gaussian"}, \code{"poisson"}.}
\item{disp}{Defaults to \code{1}.}
}
\description{
V/phi the IRLS weight. They coincide for the canonical links with a fixed dispersion, which is why writing the working response as eta + (y - mu) / W looks correct and is -- right up until a family with a real dispersion parameter arrives, where it is off by exactly that factor. The working response is eta + (y - mu) / V(mu) and nothing else.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
