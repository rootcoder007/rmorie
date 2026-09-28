.rs_indices <- c("NDVI", "EVI", "SAVI", "MSAVI", "GNDVI", "NDWI", "MNDWI", "NDBI", "NDMI", "NDSI", "NBR", "NBR2",
                 "BAI", "MSR", "CIre", "NDRE", "PRI")

#' Spectral indices of reflectance bands
#'
#' NDVI (Rouse et al. 1974), EVI (Huete et al. 2002), SAVI with L = 0.5
#' (Huete 1988), MSAVI (Qi et al. 1994), GNDVI (Gitelson et al. 1996), NDWI
#' (McFeeters 1996), MNDWI (Xu 2006) and NDSI (Hall et al. 1995)
#' (green, swir1), NDBI (Zha et al. 2003), NDMI (Gao 1996), NBR and NBR2
#' (Key and Benson 2006), BAI (Chuvieco et al. 2002), MSR (Chen 1996), CIre
#' (Gitelson et al. 2003), NDRE (Gitelson and Merzlyak 1994), PRI (Gamon et
#' al. 1992). Identical to the Python arm \code{morie.fn.rsindex}.
#'
#' @param index Index name.
#' @param ... Named bands: nir, red, green, blue, swir1, swir2, rededge,
#'   r531, r570, and L for SAVI.
#' @return Index values.
#' @references Chen, J. M. (1996). Evaluation of vegetation indices and a
#'   modified simple ratio for boreal applications. Canadian Journal of
#'   Remote Sensing 22, 229-242.
#'
#'   Huete, A. et al. (2002). Overview of the radiometric and biophysical
#'   performance of the MODIS vegetation indices. Remote Sensing of
#'   Environment 83, 195-213.
#' @examples
#' SpectralIndex("NDVI", nir = 0.5, red = 0.1)
#' SpectralIndex("SAVI", nir = c(0.5, 0.4), red = 0.1)
#' @export
SpectralIndex <- function(index, ...) {
  b <- list(...)
  if (!index %in% .rs_indices) stop("index must be one of ", paste(.rs_indices, collapse = ", "))
  nd <- function(a, c) (a - c) / (a + c)
  switch(index,
    NDVI = nd(b$nir, b$red),
    EVI = 2.5 * (b$nir - b$red) / (b$nir + 6 * b$red - 7.5 * b$blue + 1),
    SAVI = {
      L <- if (is.null(b$L)) 0.5 else b$L
      (1 + L) * (b$nir - b$red) / (b$nir + b$red + L)
    },
    MSAVI = (2 * b$nir + 1 - sqrt((2 * b$nir + 1)^2 - 8 * (b$nir - b$red))) / 2,
    GNDVI = nd(b$nir, b$green),
    NDWI = nd(b$green, b$nir),
    MNDWI = nd(b$green, b$swir1),
    NDSI = nd(b$green, b$swir1),
    NDBI = nd(b$swir1, b$nir),
    NDMI = nd(b$nir, b$swir1),
    NBR = nd(b$nir, b$swir2),
    NBR2 = nd(b$swir1, b$swir2),
    BAI = 1 / ((0.1 - b$red)^2 + (0.06 - b$nir)^2),
    MSR = (b$nir / b$red - 1) / sqrt(b$nir / b$red + 1),
    CIre = b$nir / b$rededge - 1,
    NDRE = nd(b$nir, b$rededge),
    PRI = nd(b$r531, b$r570)
  )
}

#' Red edge, reflectance, temperature and albedo conversions
#'
#' \code{RedEdgePosition}: \eqn{700 + 40((r_{670} + r_{780})/2 - r_{700})/(r_{740} - r_{700})}
#' (Guyot and Baret 1988). \code{ToaReflectance}: Landsat 8/9
#' \eqn{(M Q + A)/\sin\theta_{SE}} (USGS Landsat 8 Data Users Handbook).
#' \code{FractionalVegetationCover}: squared (Carlson and Ripley 1997) or
#' linear (Gutman and Ignatov 1998) scaled NDVI, clipped to the unit interval.
#' \code{NdviEmissivity}: NDVI thresholds method (Sobrino et al. 2004).
#' \code{LandSurfaceTemperature}: \eqn{BT/(1 + (\lambda BT/14388)\ln\epsilon)}
#' (Artis and Carnahan 1982). \code{ShortwaveAlbedo}: Liang (2001) Landsat
#' coefficients.
#'
#' @param r670,r700,r740,r780 Reflectances (nm bands).
#' @param dn Quantised values.
#' @param mult,add REFLECTANCE_MULT and ADD metadata.
#' @param sun_elevation Sun elevation (degrees).
#' @param ndvi NDVI.
#' @param ndvi_soil,ndvi_veg NDVI of bare soil and full vegetation.
#' @param squared Squared scaling.
#' @param red Red reflectance.
#' @param veg_emissivity Emissivity of full vegetation.
#' @param bt Brightness temperature (K).
#' @param emissivity Emissivity.
#' @param wavelength Effective wavelength (micrometres).
#' @param b1,b3,b4,b5,b7 Landsat TM/ETM+ surface reflectances.
#' @return Numeric values.
#' @references Sobrino, J. A., Jimenez-Munoz, J. C. and Paolini, L. (2004).
#'   Land surface temperature retrieval from LANDSAT TM 5. Remote Sensing of
#'   Environment 90, 434-440.
#'
#'   Liang, S. (2001). Narrowband to broadband conversions of land surface
#'   albedo I: algorithms. Remote Sensing of Environment 76, 213-238.
#' @examples
#' RedEdgePosition(0.05, 0.1, 0.35, 0.45)
#' ToaReflectance(10000, 2e-5, -0.1, 30)
#' FractionalVegetationCover(0.35)
#' NdviEmissivity(0.35, 0.08)
#' LandSurfaceTemperature(300, 0.98)
#' ShortwaveAlbedo(0.1, 0.1, 0.3, 0.2, 0.1)
#' @export
RedEdgePosition <- function(r670, r700, r740, r780) 700 + 40 * ((r670 + r780) / 2 - r700) / (r740 - r700)

#' @rdname RedEdgePosition
#' @export
ToaReflectance <- function(dn, mult, add, sun_elevation) (mult * dn + add) / sin(sun_elevation * pi / 180)

#' @rdname RedEdgePosition
#' @export
FractionalVegetationCover <- function(ndvi, ndvi_soil = 0.2, ndvi_veg = 0.5, squared = TRUE) {
  f <- pmin(1, pmax(0, (ndvi - ndvi_soil) / (ndvi_veg - ndvi_soil)))
  if (squared) f^2 else f
}

#' @rdname RedEdgePosition
#' @export
NdviEmissivity <- function(ndvi, red, ndvi_soil = 0.2, ndvi_veg = 0.5, veg_emissivity = 0.99) {
  ifelse(ndvi < ndvi_soil, 0.979 - 0.035 * red,
         ifelse(ndvi > ndvi_veg, veg_emissivity,
                0.004 * FractionalVegetationCover(ndvi, ndvi_soil, ndvi_veg) + 0.986))
}

#' @rdname RedEdgePosition
#' @export
LandSurfaceTemperature <- function(bt, emissivity, wavelength = 10.895) {
  bt / (1 + (wavelength * bt / 14388) * log(emissivity))
}

#' @rdname RedEdgePosition
#' @export
ShortwaveAlbedo <- function(b1, b3, b4, b5, b7) 0.356 * b1 + 0.130 * b3 + 0.373 * b4 + 0.085 * b5 + 0.072 * b7 - 0.0018

#' Change vector analysis and thematic map accuracy
#'
#' \code{ChangeVector}: per pixel (row) magnitude
#' \eqn{\sqrt{\sum_k (a_k - b_k)^2}} and, for two bands, direction in
#' degrees (Malila 1980). \code{AccuracyAssessment}: confusion matrix (rows
#' predicted, columns reference), overall accuracy, Cohen's kappa,
#' producer's and user's accuracies (Congalton 1991).
#'
#' @param before,after Matrices (pixels x bands).
#' @param reference,predicted Class labels.
#' @return List.
#' @references Congalton, R. G. (1991). A review of assessing the accuracy
#'   of classifications of remotely sensed data. Remote Sensing of
#'   Environment 37, 35-46.
#' @examples
#' ChangeVector(rbind(c(0.1, 0.3)), rbind(c(0.4, 0.7)))
#' AccuracyAssessment(c(1, 1, 2, 2, 2, 1), c(1, 2, 2, 2, 1, 1))$kappa
#' @export
ChangeVector <- function(before, after) {
  B <- as.matrix(before)
  A <- as.matrix(after)
  if (!identical(dim(A), dim(B))) stop("before and after must have the same shape")
  D <- A - B
  list(magnitude = sqrt(rowSums(D^2)),
       direction = if (ncol(D) == 2) (atan2(D[, 2], D[, 1]) * 180 / pi) %% 360 else rep(NaN, nrow(D)))
}

#' @rdname ChangeVector
#' @export
AccuracyAssessment <- function(reference, predicted) {
  if (length(reference) != length(predicted) || !length(reference)) {
    stop("reference and predicted must have equal non-zero length")
  }
  labels <- sort(unique(c(reference, predicted)))
  C <- table(factor(predicted, labels), factor(reference, labels))
  C <- matrix(as.vector(C), length(labels))
  n <- length(reference)
  po <- sum(diag(C)) / n
  pe <- sum(rowSums(C) * colSums(C)) / n^2
  list(labels = labels, confusion = C, overall = po, kappa = if (pe < 1) (po - pe) / (1 - pe) else NaN,
       producers = diag(C) / colSums(C), users = diag(C) / rowSums(C))
}
