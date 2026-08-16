% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.propinf_init_net}
\alias{.propinf_init_net}
\title{.propinf_init_net}
\usage{
.propinf_init_net(n_in, hidden, rnd)
}
\arguments{
\item{n_in}{Coerced to integer by the body, with \code{as.integer}.}
\item{hidden}{Coerced to integer by the body, with \code{as.integer}.}
\item{rnd}{Passed to \code{.propinf_normal_lcg}.}
}
\description{
A step of the propinf_native implementation. Called by \code{morie_propinf_train_fcnn}. See the file header for the source the module follows.
}
\value{
The value of \code{net}, as built in the body.
}
\keyword{internal}
