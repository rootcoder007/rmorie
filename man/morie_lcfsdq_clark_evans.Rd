% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_lcfsdq_clark_evans}
\alias{morie_lcfsdq_clark_evans}
\title{The Clark-Evans index and its z statistic}
\usage{
morie_lcfsdq_clark_evans(dists, n, area, perimeter, edge = "none") 
}
\arguments{
\item{dists}{Nearest-neighbour distances.}
\item{n}{Number of points in the pattern.}
\item{area}{Window area.}
\item{perimeter}{Window perimeter.}
\item{edge}{"none" or "donnelly".}
}
\description{
R is the observed mean over the expectation 1 / (2 sqrt(lambda)) under complete spatial randomness. Donnelly's correction adds a perimeter term to both the expectation and the variance, which is what stops a small window reading as regular.
}
\value{
A list with R, the observed and expected means, the standard   error, z and p.
}
\references{
  Clark, P.J. and Evans, F.C. (1954) "Distance to nearest neighbor as
    a measure of spatial relationships in populations." Ecology 35(4),
    445-453.
  Donnelly, K. (1978) "Simulations to determine the variance and
    edge-effect of total nearest neighbour distance." In I. Hodder
    (ed.), Simulation Methods in Archaeology, Cambridge University
    Press, 91-95.
  Diggle, P.J. (2003) "Statistical Analysis of Spatial Point
    Patterns," 2nd edition. Arnold, chapter 2.
}
\keyword{internal}
\examples{
morie_lcfsdq_clark_evans(dists = c(1, 2, 3, 4, 5, 6, 7, 8), n = 5L, area = 5L, perimeter = c(1, 2, 3, 4, 5, 6, 7, 8))
}
