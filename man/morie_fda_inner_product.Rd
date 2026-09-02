% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_fda_inner_product}
\alias{morie_fda_inner_product}
\title{Basis inner-product matrix (MVSML eq. 14.7)}
\usage{
morie_fda_inner_product(t, L1, L2, kind = "fourier") 
}
\arguments{
\item{t}{numeric grid of observation points}
\item{L1}{number of coefficient-function basis terms}
\item{L2}{number of curve basis terms}
\item{kind}{basis kind, "fourier" or "poly"}
}
\description{
Q_\{jl\} = integral phi_j(t) psi_l(t) dt evaluated by the trapezoid rule over the observation grid.
}
\value{
an L1 x L2 numeric matrix
}
\keyword{internal}
\examples{
rmorie:::morie_fda_inner_product(t = seq(0, 1, length.out = 50), L1 = 2, L2 = 3)
}
