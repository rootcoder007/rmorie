% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_rg_quad}
\alias{.morie_rg_quad}
\title{Adaptive Simpson, the same rule the Python arm\\'s quad() uses, so the}
\usage{
.morie_rg_quad(f, a, b, tol = 1.49e-08, maxdepth = 50L)
}
\arguments{
\item{f}{See Usage.}
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
\item{tol}{Numeric; combined arithmetically in the body. Defaults to \code{1.49e-08}.}
\item{maxdepth}{Defaults to \code{50L}.}
}
\description{
two languages agree to their shared tolerance rather than to whatever a black-box integrator happens to do.
}
\value{
The value of \code{rec}.
}
\keyword{internal}
