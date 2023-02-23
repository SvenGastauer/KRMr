#' Stickleback Swimbladder Shape
#'
#' Example Shapes of the swimbladder (sb) of a few sticklebacks (stb)
#'
#' @docType data
#'
#' @usage data(stb_sb)
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
#' data(stb_sb)
#' i = unique(stb_sb$ind)[1]
#' sb =stb_sb%>%filter(ind==i)
#' par(mfrow=c(1,2))
#' KRMr::shplot(x_sb = sb$x_sb, w_sb = sb$w_sb,
#'             z_sbU = sb$z_sbU, z_sbL = sb$z_sbL)
"stb_sb"
