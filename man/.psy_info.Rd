% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_info}
\alias{.psy_info}
\title{.psy_info}
\usage{
.psy_info(theta, it)
}
\arguments{
\item{theta}{Passed to \code{.psy_p3pl}.}
\item{it}{A list; the body reads \code{$a}, \code{$b}, \code{$c} from it.}
}
\description{
A step of the psycho_native implementation. Called by \code{morie_psy_map_theta}, \code{morie_psy_mle_theta}, \code{morie_psy_wle_theta}. See the file header for the source the module follows.
}
\value{
A vector, from \code{as.numeric}.
}
\keyword{internal}
