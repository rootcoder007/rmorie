% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.ot_complete_tree}
\alias{.ot_complete_tree}
\title{.ot_complete_tree}
\usage{
.ot_complete_tree(basis, n, m)
}
\arguments{
\item{basis}{See Usage.}
\item{n}{A count; the body uses it as \code{seq_len(...)}.}
\item{m}{A count; the body uses it as \code{seq_len(...)}.}
}
\description{
A step of the helpers_ot implementation. Called by \code{.ot_emd}. See the file header for the source the module follows.
}
\value{
The value of \code{.ot_sortbasis}.
}
\keyword{internal}
