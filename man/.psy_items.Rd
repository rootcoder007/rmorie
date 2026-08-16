% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_items}
\alias{.psy_items}
\title{.psy_items}
\usage{
.psy_items(m, a, b, cc)
}
\arguments{
\item{m}{A count; the body uses it as \code{rep(...)}.}
\item{a}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{b}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
\item{cc}{Optional; may be \code{NULL}. Coerced to numeric by the body, with \code{as.numeric}.}
}
\description{
A step of the psycho_native implementation. Called by \code{morie_psy_eap_theta}, \code{morie_psy_map_theta}, \code{morie_psy_mle_theta} and 1 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{a}, \code{b}, \code{c}.
}
\keyword{internal}
