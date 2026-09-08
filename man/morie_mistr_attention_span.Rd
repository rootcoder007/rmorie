% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_attention_span}
\alias{morie_mistr_attention_span}
\title{Theoretical attention span}
\usage{
morie_mistr_attention_span(window, n_layers) 
}
\arguments{
\item{window}{Window size.}
\item{n_layers}{Number of layers.}
}
\description{
Theoretical attention span
}
\value{
The span (window * n_layers).
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_mistr_attention_span(V, V)
}
