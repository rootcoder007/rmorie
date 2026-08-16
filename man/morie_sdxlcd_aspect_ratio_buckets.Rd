% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sdxlcd_aspect_ratio_buckets}
\alias{morie_sdxlcd_aspect_ratio_buckets}
\title{morie_sdxlcd_aspect_ratio_buckets}
\usage{
morie_sdxlcd_aspect_ratio_buckets(ratios, pixels = 1024 * 1024, multiple = 64)
}
\arguments{
\item{ratios}{See Usage.}
\item{pixels}{Defaults to \code{1024 * 1024}.}
\item{multiple}{Defaults to \code{64}.}
}
\description{
Part of the sdxlcd_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{buckets}, \code{max_pixel_error}, \code{note}.
}
\references{
Podell, D., English, Z., Lacey, K., Blattmann, A., Dockhorn, T.,
Muller, J., Penna, J. & Rombach, R. (2023) "SDXL: Improving Latent
Diffusion Models for High-Resolution Image Synthesis",
arXiv:2307.01952 (ICLR 2024). Sec. 2.1 (a three times larger UNet
backbone, more attention blocks, a second text encoder), Sec. 2.2
("Conditioning the Model on Image Size": the two existing approaches of
discarding images below a minimum resolution or upscaling them, the
measured 39% of data that would be discarded at 256 pixels, and the
proposal to condition on the original height and width, each embedded
by Fourier features, concatenated and ADDED to the timestep embedding;
"Conditioning the Model on Cropping Parameters": random cropping during
training leaking into samples as cut-off objects, uniformly sampling
c_top and c_left and feeding them as Fourier-embedded conditioning, and
setting (0,0) at inference to obtain object-centred samples), Sec. 2.3
(multi-aspect finetuning with buckets keeping the pixel count close to
1024^2), and Sec. 2.5 (the separate refinement model applying a
noising-denoising process to SDXL\'s latents).

Rombach, R., Blattmann, A., Lorenz, D., Esser, P. & Ommer, B. (2022)
"High-Resolution Image Synthesis with Latent Diffusion Models",
CVPR 2022, 10684-10695, arXiv:2112.10752. The latent diffusion
model being improved.

Ho, J., Jain, A. & Abbeel, P. (2020) "Denoising Diffusion
Probabilistic Models", NeurIPS 2020, arXiv:2006.11239.
}
\keyword{internal}
