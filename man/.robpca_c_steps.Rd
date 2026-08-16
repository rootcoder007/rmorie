% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.robpca_c_steps}
\alias{.robpca_c_steps}
\title{.robpca_c_steps}
\usage{
.robpca_c_steps(rows, idx, max_iter = 100L)
}
\arguments{
\item{rows}{A matrix; indexed by row and column.}
\item{idx}{A vector; its length is taken.}
\item{max_iter}{A count; the body uses it as \code{seq_len(...)}. Defaults to \code{100L}.}
}
\description{
A step of the robpca_native implementation. Called by \code{.robpca_fast_mcd}, \code{morie_robpca}. See the file header for the source the module follows.
}
\value{
A list with \code{idx}, \code{mu}, \code{C}, \code{det}.
}
\keyword{internal}
