% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{wbwc_invert}
\alias{wbwc_invert}
\title{wbwc_invert}
\usage{
wbwc_invert(tree, known, candidates, priors, sensitive = 0L, unknown = NULL)
}
\arguments{
\item{tree}{Passed to \code{tree_paths}.}
\item{known}{See Usage.}
\item{candidates}{A vector; its length is taken.}
\item{priors}{A vector; indexed elementwise.}
\item{sensitive}{Coerced to character by the body, with \code{as.character}. Defaults to \code{0L}.}
\item{unknown}{Optional; may be \code{NULL}. Coerced to integer by the body, with \code{as.integer}.}
}
\description{
A step of the attrInf_native implementation. Called by \code{morie_attrInf}. See the file header for the source the module follows.
}
\value{
A list with \code{estimate}, \code{scores}, \code{n_paths}, \code{N}.
}
\keyword{internal}
\examples{
tree <- list(feature = 0L, branches = list(
  "0" = list(label = 0L, count = 30),
  "1" = list(label = 1L, count = 20)))
priors <- list("0" = list("0" = 0.6, "1" = 0.4))
r <- wbwc_invert(tree, known = list(), candidates = list("0", "1"),
                 priors = priors)
str(r, max.level = 1)
}
