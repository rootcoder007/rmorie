% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_gvar_sigmoid}
\alias{morie_gvar_sigmoid}
\title{Logistic sigmoid on a reverse-mode tape node}
\usage{
morie_gvar_sigmoid(x) 
}
\arguments{

\item{x}{Argument \code{x}; see Usage.}
}
\description{
Logistic sigmoid on a reverse-mode tape node
}
\value{
The value of \code{.morie_gvar}.
}
\keyword{internal}
\examples{
morie_gvar_sigmoid(.morie_gvar(0))$value
}
