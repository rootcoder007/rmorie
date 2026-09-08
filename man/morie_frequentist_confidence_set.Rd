% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_frequentist_confidence_set}
\alias{morie_frequentist_confidence_set}
\title{morie_frequentist_confidence_set}
\usage{
morie_frequentist_confidence_set(theta_set, se_phi, level = 0.95, target = "parameter") 
}
\arguments{
\item{theta_set}{A list; the body reads \code{$lower}, \code{$upper}, \code{$width} from it.}
\item{se_phi}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{level}{Coerced to numeric by the body, with \code{as.numeric}. Defaults to \code{0.95}.}
\item{target}{One of \code{"parameter"}, \code{"set"}. Defaults to \code{"parameter"}.}
}
\description{
A step of the bndbye_native implementation. Called by \code{morie_compare_sets}. See the file header for the source the module follows.
}
\value{
A list with \code{lower}, \code{upper}, \code{width}, \code{critical_value}, \code{target}, \code{level}, \code{note}.
}
\keyword{internal}
\examples{
morie_frequentist_confidence_set(theta_set = data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9)),
  se_phi = 5L)
}
