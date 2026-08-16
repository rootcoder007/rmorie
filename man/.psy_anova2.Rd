% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.psy_anova2}
\alias{.psy_anova2}
\title{Two-way ANOVA mean squares for a complete crossed subjects-by-raters}
\usage{
.psy_anova2(y, subject, rater)
}
\arguments{
\item{y}{See Usage.}
\item{subject}{See Usage.}
\item{rater}{See Usage.}
}
\description{
table. An unbalanced table makes the mean squares non-orthogonal and every downstream ICC ill-defined, so it is an error rather than a number from whatever cells happen to be present.
}
\value{
A list with \code{MSR}, \code{MSC}, \code{MSE}, \code{MSW}, \code{n}, \code{k}, \code{matrix}.
}
\keyword{internal}
