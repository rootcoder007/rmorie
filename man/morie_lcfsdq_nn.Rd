% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_lcfsdq_nn}
\alias{morie_lcfsdq_nn}
\title{Distance from each point to its k-th nearest OTHER point}
\usage{
morie_lcfsdq_nn(coords, k = 1L, metric = "euclidean")
}
\arguments{
\item{coords}{Point coordinate matrix.}
\item{k}{Which neighbour.}
\item{metric}{A member of the metric list.}
}
\description{
Ties are broken by index so the two arms select the same neighbour when two are equidistant -- which happens constantly on a lattice and would otherwise make the local statistics disagree while the distances matched.
}
\value{
A list with the distances and the neighbour indices, the   latter zero-based to match the Python arm.
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
M <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
morie_lcfsdq_nn(M)
}
