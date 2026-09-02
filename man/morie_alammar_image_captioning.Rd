% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_alammar_image_captioning}
\alias{morie_alammar_image_captioning}
\title{Captioning projection, unfreezing, continued pretraining,}
\usage{
morie_alammar_image_captioning(image, visual_encoder, projector, llm, prompt = "Describe the image.") 
}
\arguments{
\item{image}{Opaque image object passed to the encoder.}
\item{visual_encoder}{function(image) -> feature vector.}
\item{projector}{Matrix (d_llm x d_vis) or function.}
\item{llm}{function(projected, prompt) -> caption.}
\item{prompt}{Text prompt.}
}
\description{
augmentation, TSDAE
}
\value{
A list with \code{caption}, \code{projected}, \code{feature_dim}, \code{projected_dim}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
\examples{
morie_alammar_image_captioning("img", function(im) c(1, 2), rbind(c(1, 
    0)), function(z, p) "cap")
}
