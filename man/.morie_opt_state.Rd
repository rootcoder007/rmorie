% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_opt_state}
\alias{.morie_opt_state}
\title{.morie_opt_state}
\usage{
.morie_opt_state(state, n, keys = c("m", "v"))
}
\arguments{
\item{state}{Optional; may be \code{NULL}. A list; the body reads \code{$t} from it.}
\item{n}{A count; the body uses it as \code{numeric(...)}.}
\item{keys}{Defaults to \code{c("m", "v")}.}
}
\description{
A step of the optim_native implementation. Called by \code{morie_adagrad}, \code{morie_adam}, \code{morie_adamw_step} and 6 others in the module. See the file header for the source the module follows.
}
\value{
The value of \code{out}, as built in the body.
}
\keyword{internal}
