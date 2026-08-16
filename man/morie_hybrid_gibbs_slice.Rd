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
