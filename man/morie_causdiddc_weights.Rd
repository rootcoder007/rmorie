% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc_weights}
\alias{morie_causdiddc_weights}
\title{The w_\{g,t\} of Theorem 1}
\usage{
morie_causdiddc_weights(D, group, period, weights = NULL) 
}
\arguments{
\item{weights}{Optional observation weights.}

\item{D}{Argument \code{D}; see Usage.}

\item{group}{Argument \code{group}; see Usage.}

\item{period}{Argument \code{period}; see Usage.}
}
\description{
The weights of de Chaisemartin & D\'Haultfoeuille (2020) Theorem 1, from the residual of the treatment indicator on the two sets of fixed effects, scaled to sum to one over the treated cells.  They can be NEGATIVE -- the regression everyone runs is not an average treatment effect.
}
\value{
A list with \code{weights} (named numeric keyed by   "(group,period)" strings) and \code{residual} (the within-   transformed treatment).
}
\keyword{internal}
\examples{
set.seed(3)
n_g <- 6; n_t <- 4
grid <- expand.grid(g = seq_len(n_g), t = seq_len(n_t))
D <- as.numeric(grid$g <= 2 & grid$t >= 3)
r <- morie_causdiddc_weights(D, grid$g, grid$t)
str(r, max.level = 1)
}
