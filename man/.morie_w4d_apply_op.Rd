% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_w4d_apply_op}
\alias{.morie_w4d_apply_op}
\title{.morie_w4d_apply_op}
\usage{
.morie_w4d_apply_op(op, x)
}
\arguments{
\item{op}{A list; the body reads \code{$kind}, \code{$param} from it.}
\item{x}{A matrix; passed to \code{\%*\%}.}
}
\description{
A step of the geron_w4d_native implementation. Called by \code{morie_geron_run_graph}, \code{morie_geron_trace}. See the file header for the source the module follows.
}
\value{
Nothing; this branch always raises.
}
\keyword{internal}
