% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc_twfe}
\alias{morie_causdiddc_twfe}
\title{The two-way fixed effects coefficient}
\usage{
morie_causdiddc_twfe(Y, D, group, period)
}
\description{
The TWFE coefficient of de Chaisemartin & D\'Haultfoeuille (2020) Theorem 1 and its decomposition: the cell weights, the count and mass of the negative weights, and the treated-cell count.
}
\value{
A list with \code{estimate}, \code{beta_fe}, \code{weights},   \code{n_negative}, \code{negative_mass}, \code{weight_sum},   \code{n_treated_cells}, \code{n}, \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(3)
n_g <- 6; n_t <- 4
grid <- expand.grid(g = seq_len(n_g), t = seq_len(n_t))
D <- as.numeric(grid$g <= 2 & grid$t >= 3)
Y <- 1 + 0.5 * grid$t + 2 * D + rnorm(nrow(grid), 0, 0.2)
r <- morie_causdiddc_twfe(Y, D, grid$g, grid$t)
str(r, max.level = 1)
}
