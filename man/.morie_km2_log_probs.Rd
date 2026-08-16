% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_km2_log_probs}
\alias{.morie_km2_log_probs}
\title{.morie_km2_log_probs}
\usage{
.morie_km2_log_probs(items, scorer, name)
}
\arguments{
\item{items}{Coerced to list by the body, with \code{as.list}.}
\item{scorer}{Optional; may be \code{NULL}. Passed to \code{is.null}.}
\item{name}{Passed to \code{sprintf}.}
}
\description{
A step of the kamath_llm2_native implementation. Called by \code{morie_kamath_ch6_cat_metric}, \code{morie_kamath_ch6_pll}. See the file header for the source the module follows.
}
\value{
A vector, from \code{vapply}.
}
\keyword{internal}
