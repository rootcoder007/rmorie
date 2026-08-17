% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_causdiddc}
\alias{morie_causdiddc}
\title{de Chaisemartin-D\'Haultfoeuille (2020) TWFE vs DID_M}
\usage{
morie_causdiddc(Y, D, group, period)
}
\arguments{
\item{Y}{Outcome, long format.}
\item{D}{Binary 0/1 treatment, long format.  Treated status must   be constant within every (group, period) cell.}
\item{group,period}{Panel identifiers.}
}
\description{
The two-way fixed effects coefficient \code{beta_fe} and the paper\'s \code{DID_M} alternative, side by side, plus the Theorem 1 weights that show the gap.  \code{beta_fe} can take the wrong sign under heterogeneity; \code{DID_M} is unbiased for the average effect among switchers under common trends.
}
\value{
A list mirroring the Python RichResult payload:   \code{estimate} (DID_M), \code{beta_fe}, \code{did_m},   \code{weights}, \code{n_negative}, \code{negative_mass},   \code{weight_sum}, \code{n_switches}, \code{gap}, \code{n},   \code{method}, \code{note}.
}
\keyword{internal}
\examples{
set.seed(3)
n_g <- 6; n_t <- 4
grid <- expand.grid(g = seq_len(n_g), t = seq_len(n_t))
D <- as.numeric(grid$g <= 2 & grid$t >= 3)
Y <- 1 + 0.5 * grid$t + 2 * D + rnorm(nrow(grid), 0, 0.2)
r <- morie_causdiddc(Y, D, grid$g, grid$t)
str(r, max.level = 1)
}
