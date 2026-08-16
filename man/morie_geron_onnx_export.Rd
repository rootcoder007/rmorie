% Generated from the package source by gen_rd_tree.R -- roxygen2
% hangs on this package. Usage comes from args(), so it tracks
% the formals.
\name{morie_geron_onnx_export}
\alias{morie_geron_onnx_export}
\title{ONNX export trace: shape-traced graph validated on a concrete input (Geron App B, hmonnx)}
\usage{
morie_geron_onnx_export(model, args, file = NULL)
}
\arguments{
\item{model}{List of layer specs (`op`, `in_features`, `out_features`, ...).}
\item{args}{Example input.}
\item{file}{Optional path for the traced graph (written as JSON).}
}
\description{
ONNX export trace: shape-traced graph validated on a concrete input (Geron App B, hmonnx)
}
\value{
A list with \code{nodes}, \code{graph}, \code{input_shape}, \code{output_shape}, \code{n_parameters}, \code{file}, \code{is_protobuf}, \code{estimate}, \code{n}, \code{method}.
}
\keyword{internal}
