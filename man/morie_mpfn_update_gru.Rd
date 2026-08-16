% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mpfn_update_gru}
\alias{morie_mpfn_update_gru}
\title{U_t as a GRU}
\usage{
morie_mpfn_update_gru(h, m, Wz, Uz, Wr, Ur, Wh, Uh)
}
\arguments{
\item{h}{Current state.}
\item{m}{Message.}
\item{Wz,Uz,Wr,Ur,Wh,Uh}{Update and reset gate and candidate   projections.}
}
\description{
U_t as a GRU
}
\value{
The updated state.
}
\keyword{internal}
