% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{self_distillation_loss}
\alias{self_distillation_loss}
\title{DINO self-distillation loss (image- or patch-level)}
\usage{
self_distillation_loss(student, teacher, temperature_s = 0.1, temperature_t = 0.04, patch_level = FALSE)
}
\arguments{
\item{student}{Numeric vector.}
\item{teacher}{Numeric vector.}
\item{temperature_s}{Student softmax temperature.}
\item{temperature_t}{Teacher softmax temperature.}
\item{patch_level}{If TRUE, label the level as "patch" (iBOT).}
}
\description{
DINO self-distillation loss (image- or patch-level)
}
\value{
Named list mirroring the RichResult payload: \code{estimate},   \code{loss}, \code{student}, \code{teacher}, \code{level},   \code{teacher_entropy}, \code{method}, \code{note}.
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
