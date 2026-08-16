% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_mistr_sliding_window_mask}
\alias{morie_mistr_sliding_window_mask}
\title{Sliding-window attention mask}
\usage{
morie_mistr_sliding_window_mask(L, window, causal = TRUE)
}
\arguments{
\item{L}{Sequence length.}
\item{window}{Window size.}
\item{causal}{Apply causal mask.}
}
\description{
Sliding-window attention mask
}
\value{
An L x L logical matrix.
}
\keyword{internal}
