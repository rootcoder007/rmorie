% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_sam2vd_memory_bank}
\alias{morie_sam2vd_memory_bank}
\title{morie_sam2vd_memory_bank}
\usage{
morie_sam2vd_memory_bank(n_recent = 7, m_prompted = 1)
}
\arguments{
\item{n_recent}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{7}.}
\item{m_prompted}{Coerced to integer by the body, with \code{as.integer}. Defaults to \code{1}.}
}
\description{
A step of the sam2vd_native implementation. Called by \code{morie_sam2vd_propagate}. See the file header for the source the module follows.
}
\value{
A list with \code{recent}, \code{prompted}, \code{pointers}, \code{n_recent}, \code{m_prompted}, \code{note}.
}
\references{
----------
Ravi, N., Gabeur, V., Hu, Y.-T., Hu, R., Ryali, C., Ma, T., Khedr, H.,
Radle, R., Rolland, C., Gustafson, L., Mintun, E., Pan, J., Alwala,
K. V., Carion, N., Wu, C.-Y., Girshick, R., Dollar, P. &
Feichtenhofer, C. (2024) "SAM 2: Segment Anything in Images and
Videos", arXiv:2408.00714. Sec. 4: the streaming architecture
processing video frames one at a time with a memory attention module
attending to previous memories of the target object, and that when
applied to images the memory is empty and the model behaves like SAM;
the memory attention stacking L blocks of self-attention followed by
cross-attention to memories of prompted and unprompted frames and to
OBJECT POINTERS, followed by an MLP; the memory bank as a FIFO queue
of up to N recent frames plus a separate FIFO queue of up to M
prompted frames, both stored as spatial feature maps; the object
pointers taken from mask decoder output tokens; and temporal
position information embedded into the N recent memories but NOT
into prompted frames, because their training signal is sparser and
generalising to unseen temporal ranges is harder. Also the reported
results: 3x fewer interactions in video and 6x faster than SAM on
images, with the SA-V dataset of 35.5M masks across 50.9K videos.

Kirillov, A. et al. (2023) "Segment Anything", ICCV 2023,
4015-4026, arXiv:2304.02643. The image model generalised here.
}
\keyword{internal}
\examples{
morie_sam2vd_memory_bank()
}
