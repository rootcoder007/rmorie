% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.s4_icc_ms}
\alias{.s4_icc_ms}
\title{# Two-way ANOVA mean squares for the intraclass correlations}
\usage{
.s4_icc_ms(y, subject, rater)
}
\arguments{
\item{y}{See Usage.}
\item{subject}{See Usage.}
\item{rater}{See Usage.}
}
\description{
A step of the s04core implementation. Called by \code{Icc12c}, \code{IccA}, \code{IccC}. See the file header for the source the module follows.
}
\value{
A list with \code{ms_r}, \code{ms_c}, \code{ms_e}, \code{k}, \code{n}.
}
\keyword{internal}
