% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_hybrid_gibbs_slice}
\alias{morie_hybrid_gibbs_slice}
\title{Entry point: hybrid Gibbs sweep with a slice update per coordinate}
\usage{
morie_hybrid_gibbs_slice(log_conditionals, x0, n = 2000L, ...)
}
\description{
Currently every coordinate is slice-updated; this name is preserved to mirror the Python arm\'s hybrid interface, so a future module that mixes conjugate draws and slice draws can slot in here without breaking callers.
}
\value{
Same payload as \code{morie_gibbs_slice}.
}
\keyword{internal}
\examples{
lcs <- list(
  function(x, others) -0.5 * (x - 0.5 * others[1])^2,
  function(x, others) -0.5 * (x - 0.5 * others[1])^2)
r <- morie_hybrid_gibbs_slice(lcs, x0 = c(0, 0), n = 300L, seed = 1)
str(r, max.level = 1)
}
