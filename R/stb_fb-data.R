#' Stickleback Fish Body Shape
#'
#' Example Shapes of the fishbody (fb) of a few sticklebacks (stb)
#'
#' @docType data
#'
#' @usage data(stb_fb)
#'
#' @format data.frame
#'
#' @keywords datasets
#'
#' @references TBA
#'
#' @source TBA
#'
#' @examples
#' data(stb_fb)
#' i = unique(stb_fb$ind)[1]
#' fb =stb_fb%>%filter(ind==i)
#' par(mfrow=c(1,2))
#' KRMr::shplot(x_fb = fb$x_fb, w_fb = fb$w_fb,
#'             z_fbU = fb$z_fbU, z_fbL = fb$z_fbL)
"stb_fb"
