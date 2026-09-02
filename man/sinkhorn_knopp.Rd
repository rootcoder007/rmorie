% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{sinkhorn_knopp}
\alias{sinkhorn_knopp}
\title{Sinkhorn-Knopp centering (SwAV style)}
\usage{
sinkhorn_knopp(scores, iterations = 3L, epsilon = 0.05) 
}
\arguments{
\item{scores}{Numeric matrix (n x K).}
\item{iterations}{Number of Sinkhorn-Knopp iterations.}
\item{epsilon}{Temperature on the exp transform.}
}
\description{
Sinkhorn-Knopp centering (SwAV style)
}
\value{
List with \code{Q} (matrix), \code{iterations},   \code{row_sums}, \code{note}.
}
\references{
Oquab, M., Darcet, T., Moutakanni, T., Vo, H., Szafraniec, M.,
Khalidov, V., Fernandez, P., Haziza, D., Massa, F., El-Nouby, A. et
al. (2024) "DINOv2: Learning Robust Visual Features without
Supervision", TMLR 01/2024, arXiv:2304.07193.
Caron, M., Touvron, H., Misra, I., Jegou, H., Mairal, J., Bojanowski,
P. & Joulin, A. (2021) "Emerging Properties in Self-Supervised Vision
Transformers", ICCV 2021, arXiv:2104.14294. DINO.
Zhou, J., Wei, C., Wang, H., Shen, W., Xie, C., Yuille, A. & Kong, T.
(2022) "iBOT: Image BERT Pre-Training with Online Tokenizer", ICLR
2022, arXiv:2111.07832.
Sablayrolles, A., Douze, M., Schmid, C. & Jegou, H. (2019)
"Spreading vectors for similarity search", ICLR 2019,
arXiv:1806.03198.
}
\keyword{internal}
\examples{
V <- c(1, 2, 3, 4, 5, 6, 7, 8)
sinkhorn_knopp(V)
}
