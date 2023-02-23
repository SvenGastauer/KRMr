#' Sardine Swimbladder Shape
#'
#' Example Shapes of the swimbladder (sb) of a few sticklebacks (stb)
#'
#' @docType data
#'
#' @usage data(pil_sb)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
#' @references NOAA Southwest Fisheries Science Center
#'
#' @source https://www.fisheries.noaa.gov/data-tools/krm-model
#'
#' @examples
#' data(pil_sb)
#' sb=pil_sb
#' par(mfrow=c(1,2))
#' KRMr::shplot(x_sb = sb$x_sb, w_sb = sb$w_sb,
#'             z_sbU = sb$z_sbU, z_sbL = sb$z_sbL)
"pil_sb"
