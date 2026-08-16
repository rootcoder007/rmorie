% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.abcnnt_made_layer}
\alias{.abcnnt_made_layer}
\title{A single Masked Autoregressive layer as a list of named matrices. The}
\usage{
.abcnnt_made_layer(dim_x, dim_t, hidden, e, reverse = FALSE)
}
\arguments{
\item{dim_x}{A count; the body uses it as \code{seq_len(...)}.}
\item{dim_t}{A count; the body uses it as \code{seq_len(...)}.}
\item{hidden}{A count; the body uses it as \code{seq_len(...)}.}
\item{e}{Passed to \code{.ghc_norm}.}
\item{reverse}{A flag; the body branches on it. Defaults to \code{FALSE}.}
}
\description{
masks are stored as 0/1 floats so element-wise multiply drops blocked connections, which is what the masked matrix product reduces to in any case.
}
\value{
A list with \code{W1}, \code{b1}, \code{Wm}, \code{bm}, \code{Wa}, \code{ba}, \code{M1}, \code{M2}, \code{dim_x}, \code{dim_t}, \code{hidden}, \code{order}.
}
\keyword{internal}
