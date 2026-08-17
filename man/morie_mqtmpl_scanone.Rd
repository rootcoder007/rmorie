% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mqtmpl_scanone}
\alias{morie_mqtmpl_scanone}
\title{Single-QTL genome scan}
\usage{
morie_mqtmpl_scanone(y, markers, positions, method = "em", step = 0.02, covariates = list(), error_rate = 0)
}
\arguments{
\item{y}{Phenotype vector.}
\item{markers}{List of per-marker genotype vectors.}
\item{positions}{Marker positions in cM.}
\item{method}{"em", "mr", "imp" or "hk".}
\item{step}{Grid step in cM.}
\item{covariates}{Optional list of covariate columns.}
\item{error_rate}{Genotyping error rate.}
}
\description{
Single-QTL genome scan
}
\value{
A list with peak LOD, position, full grid, and bookkeeping.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 40
m1 <- rbinom(n, 1, 0.5)
m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
r <- morie_mqtmpl_scanone(y, list(m1, m2), positions = c(0, 0.1),
                          method = "mr")
str(r, max.level = 1)
}
