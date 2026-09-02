% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mqtmpl_lod_support_interval}
\alias{morie_mqtmpl_lod_support_interval}
\title{LOD-drop support interval}
\usage{
morie_mqtmpl_lod_support_interval(scan_result, drop = 1.5) 
}
\arguments{
\item{scan_result}{Result of \code{scanone}.}
\item{drop}{LOD drop.}
}
\description{
LOD-drop support interval
}
\value{
A list with peak, lower, upper, drop, peak_lod.
}
\keyword{internal}
\examples{
set.seed(2)
n <- 40
m1 <- rbinom(n, 1, 0.5)
m2 <- ifelse(runif(n) < 0.9, m1, 1 - m1)
y <- 1 + 0.8 * m1 + rnorm(n, 0, 0.5)
sc <- morie_mqtmpl_scanone(y, list(m1, m2), positions = c(0, 0.1),
                           method = "mr")
r <- morie_mqtmpl_lod_support_interval(sc)
str(r, max.level = 1)
}
