% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.morie_gr_attend}
\alias{.morie_gr_attend}
\title{Attsdp / grsdpa kernel. mask: logical matrix, TRUE = attend}
\usage{
.morie_gr_attend(Q, K, V, mask = NULL)
}
\arguments{
\item{Q}{A matrix; passed to \code{ncol}.}
\item{K}{A matrix; passed to \code{nrow}.}
\item{V}{A matrix; passed to \code{nrow}.}
\item{mask}{Optional; may be \code{NULL}. Coerced to logical by the body, with \code{as.logical}.}
}
\description{
A step of the geron_train_native implementation. Called by \code{morie_geron_block_multi_head_attention}, \code{morie_geron_flash_attention_tile}, \code{morie_geron_multi_head_attention} and 5 others in the module. See the file header for the source the module follows.
}
\value{
A list with \code{output}, \code{weights}, \code{scores}.
}
\keyword{internal}
