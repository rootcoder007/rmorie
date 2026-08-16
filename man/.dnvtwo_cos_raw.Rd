% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{.dnvtwo_cos_raw}
\alias{.dnvtwo_cos_raw}
\title{Cosine similarity between two numeric vectors}
\usage{
.dnvtwo_cos_raw(a, b)
}
\arguments{
\item{a}{Numeric; combined arithmetically in the body.}
\item{b}{Numeric; combined arithmetically in the body.}
}
\description{
A step of the dnvtwo_native implementation. Called by \code{deduplicate}, \code{retrieve_augment}. See the file header for the source the module follows.
}
\value{
A numeric value.
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
.dnvtwo_cos_raw(V, V)
}
