% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_opt_vec}
\alias{.morie_opt_vec}
\title{.morie_opt_vec}
\usage{
.morie_opt_vec(g, name = "g")
}
\arguments{
\item{g}{Coerced to numeric by the body, with \code{as.numeric}.}
\item{name}{Defaults to \code{"g"}.}
}
\description{
A step of the optim_native implementation. Called by \code{morie_adagrad}, \code{morie_adam}, \code{morie_adamw_step} and 5 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{a}, as built in the body.
}
\keyword{internal}
