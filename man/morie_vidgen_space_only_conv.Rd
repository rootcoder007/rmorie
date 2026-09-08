% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_vidgen_space_only_conv}
\alias{morie_vidgen_space_only_conv}
\title{morie_vidgen_space_only_conv}
\usage{
morie_vidgen_space_only_conv(video, kernel) 
}
\arguments{
\item{video}{Iterated over elementwise, with \code{lapply}.}
\item{kernel}{Passed to \code{.vidgen_mat}.}
}
\description{
A step of the vidgen_native implementation. No other function in the package calls it. See the file header for the source the module follows.
}
\value{
A list with \code{video}, \code{frames}, \code{note}.
}
\references{
----------
Ho, J., Salimans, T., Gritsenko, A., Chan, W., Norouzi, M. & Fleet,
D. J. (2022) "Video Diffusion Models", Advances in Neural
Information Processing Systems 35 (NeurIPS 2022), arXiv:2204.03458.
Sec. 3: the 3D U-Net factorised over space and time, changing each
3x3 convolution into a 1x3x3 space-only convolution, keeping spatial
attention with the frame axis as a batch axis, and inserting a
temporal attention block after each spatial attention block; that
the factorisation makes it straightforward to mask the model to run
on independent images by fixing the temporal attention matrix to
match each key and query at the same timestep, enabling JOINT
training on video and image objectives, which the experiments find
important for sample quality; and Sec. 4 (reconstruction guidance
for conditional generation, with a weighting factor w_r > 1
improving sample quality, extended to spatial interpolation and
super-resolution by imposing the squared error on a downsampled
model prediction and backpropagating through the downsampling).

Ho, J., Jain, A. & Abbeel, P. (2020) "Denoising Diffusion
Probabilistic Models", NeurIPS 2020, arXiv:2006.11239. The diffusion
model being inflated.

Ho, J. & Salimans, T. (2022) "Classifier-Free Diffusion Guidance",
arXiv:2207.12598. The guidance framework this parallels.
}
\keyword{internal}
\examples{
D <- data.frame(x = c(1, 2, 3, 4), y = c(2, 4, 5, 9))
S <- c("a", "b", "c")
morie_vidgen_space_only_conv(D, S)
}
