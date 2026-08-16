% Generated from the module source by ledger/wave3/gen_rd_any.R --
% roxygen2 hangs on this package. Usage comes from args(), so it
% tracks the formals.
\name{morie_lcfsdq}
\alias{morie_lcfsdq}
\title{Nearest-neighbour summary of a pattern and a local query on x}
\usage{
morie_lcfsdq(x, coords, k = 1L, metric = "euclidean", edge = "none", sd_multiplier = 1, area = NULL, perimeter = NULL, grid = NULL)
}
\arguments{
\item{x}{A value attached to each point. Pass a constant if only the   pattern is of interest.}
\item{coords}{Point coordinates. The first two are taken as the plane   for the window; the distance uses all of them.}
\item{k}{Which nearest neighbour to use for the first-order distance.}
\item{metric}{A member of the metric list.}
\item{edge}{A member of the edge list.}
\item{sd_multiplier}{The neighbourhood radius is the mean distance   plus this times its standard deviation.}
\item{area}{Window area. Taken from the bounding box when omitted,   which UNDERSTATES the true window whenever the points do not reach   its edges -- so the index reads as more clustered than it is, and   passing the real window is worth doing.}
\item{perimeter}{Window perimeter, likewise.}
\item{grid}{Radii at which the empirical G function is reported.}
}
\description{
Nearest-neighbour summary of a pattern and a local query on x
}
\value{
A list with the nearest-neighbour distances, the Clark-Evans   index and test, the neighbourhood radius, per-point local means and   counts, the points flagged as locally clustered or isolated, and   the G function against its complete-spatial-randomness expectation.
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
ii <- 0:35
LAT <- cbind(ii\%\%6L, ii\%/\%6L)
XATT <- sin(0.7 * ii) + 0.4 * (ii\%\%4L)
morie_lcfsdq(XATT, LAT)
}
