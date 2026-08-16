% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{skip_concat}
\alias{skip_concat}
\title{skip_concat}
\usage{
skip_concat(upsampled, contracting)
}
\arguments{
\item{upsampled}{See Usage.}
\item{contracting}{See Usage.}
}
\description{
Part of the unetbk_native implementation; see the file header for the source it follows.
}
\value{
A list with \code{concatenated}, \code{crop_offset}, \code{channels}, \code{note}.
}
\references{
----------
Ronneberger, O., Fischer, P. & Brox, T. (2015) "U-Net: Convolutional
Networks for Biomedical Image Segmentation", *Medical Image Computing
and Computer-Assisted Intervention (MICCAI 2015)*, LNCS 9351,
234-241, doi:10.1007/978-3-319-24574-4_28, arXiv:1505.04597. The
abstract (training deep networks is thought to need many thousand
annotated samples; the strategy relies on strong data augmentation; a
contracting path to capture context and a symmetric expanding path
enabling precise localization; end-to-end training from very few
images; winning the ISBI cell tracking challenge 2015; segmentation of
a 512x512 image in under a second). Sec. 2 (pooling operators replaced
by upsampling; high-resolution features from the contracting path
combined with the upsampled output; a large number of feature channels
in the expansive path propagating context to higher resolution layers,
making it symmetric and yielding the u-shape; no fully connected
layers and only the valid part of each convolution, so the map
contains only pixels with full context). Figure 2 and Sec. 3 (the
overlap-tile strategy with missing input extrapolated by mirroring,
and the weight map for separating touching objects).

Long, J., Shelhamer, E. & Darrell, T. (2015) "Fully convolutional
networks for semantic segmentation", *CVPR 2015*, 3431-3440,
doi:10.1109/CVPR.2015.7298965. The fully convolutional predecessor.
}
\keyword{internal}
