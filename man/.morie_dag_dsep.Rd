% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_dag_dsep}
\alias{.morie_dag_dsep}
\title{Internal helper: d-separation of x and y given z (Bayes-Ball)}
\usage{
.morie_dag_dsep(g, x, y, z)
}
\description{
Internal helper: d-separation of x and y given z (Bayes-Ball)
}
\value{
The terminal expression of the body.
}
\keyword{internal}
\examples{
set.seed(1)
z <- rnorm(400)
x <- rbinom(400, 1, plogis(z))
y <- 0.8 * x + z + rnorm(400)
df <- data.frame(z = z, x = x, y = y)
g <- morie_dag(c("z -> x", "z -> y", "x -> y"), "x", "y")
morie_dag_estimate(g, df, method = "backdoor.linear")
.morie_dag_dsep(g = g, x = x, y = y, z = z)
}
