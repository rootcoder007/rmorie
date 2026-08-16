% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_ll}
\alias{.psy_ll}
\title{.psy_ll}
\usage{
.psy_ll(grid, yv, it)
}
\arguments{
\item{grid}{Passed to \code{.psy_p3pl}.}
\item{yv}{A matrix; passed to \code{\%*\%}.}
\item{it}{A list; the body reads \code{$a}, \code{$b}, \code{$c} from it.}
}
\description{
A step of the psycho_native implementation. Called by \code{morie_psy_eap_theta}, \code{morie_psy_map_theta}, \code{morie_psy_mle_theta} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
