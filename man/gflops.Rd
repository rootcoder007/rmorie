% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{gflops}
\alias{gflops}
\title{Forward-pass Gflops for a DiT stack}
\usage{
gflops(tokens, depth, width, mlp_ratio = 4) 
}
\arguments{
\item{tokens}{   integer T, number of tokens.}
\item{depth}{    integer L, number of DiT blocks.}
\item{width}{    integer d, model width.}
\item{mlp_ratio}{numeric, MLP hidden expansion (default 4.0).}
}
\description{
attention: 4*T*d^2 + 2*T^2*d. mlp: 2*mlp_ratio*T*d^2. Per block doubled (multiply-adds), scaled by depth L, in Gflops.
}
\value{
list with gflops, tokens, depth, width, attention_share, note.
}
\keyword{internal}
\examples{
gflops(tokens = c(1, 2, 3, 4, 5, 6, 7, 8), depth = c(1, 2, 3, 4, 5, 6, 7, 8), width = 5L)
}
