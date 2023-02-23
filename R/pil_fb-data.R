#' Sardine Fish body Shape
#'
#' Example Shapes of the fish body (fb) of a few sticklebacks (stb)
#'
#' @docType data
#'
#' @usage data(pil_fb)
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
#' data(pil_fb)
#' fb=pil_fb
#' par(mfrow=c(1,2))
#' KRMr::shplot(x_fb = fb$x_fb, w_fb = fb$w_fb,
#'             z_fbU = fb$z_fbU, z_fbL = fb$z_fbL)
"pil_fb"
