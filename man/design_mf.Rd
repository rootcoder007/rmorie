% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{design_mf}
\alias{design_mf}
\title{Encoding under which an FM IS matrix factorisation -------------}
\usage{
design_mf(u, i, n_users, n_items)
}
\arguments{
\item{u}{Coerced to integer by the body, with \code{as.integer}.}
\item{i}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_users}{Coerced to integer by the body, with \code{as.integer}.}
\item{n_items}{Coerced to integer by the body, with \code{as.integer}.}
}
\description{
One indicator for the user, one for the item. The only non-zero interaction is then <v_u, v_i>.
}
\value{
The value of \code{x}, as built in the body.
}
\keyword{internal}
