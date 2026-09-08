% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_openclip_contrastive}
\alias{morie_alammar_openclip_contrastive}
\title{morie_alammar_openclip_contrastive}
\usage{
morie_alammar_openclip_contrastive(I_emb, T_emb, tau = 0.07) 
}
\arguments{
\item{I_emb,T_emb}{Image and text towers, row-matched.}

\item{tau}{Argument \code{tau}; see Usage.}
}
\description{
morie_alammar_openclip_contrastive
}
\value{
A list with \code{estimate}, \code{image_to_text_loss}, \code{text_to_image_loss}, \code{similarity_matrix}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
morie_alammar_openclip_contrastive(V, V)
}
