#' Calculate the analytical solution for a weakly scattering sphere
#' @description Scattered pressure from an incident plane wave upon a fluid sphere. Based on:
#' Jech, J. M., Horne, J. K., Chu, D., Demer, D. A., Francis, D. T., Gorska, N., ... & Reeder, D. B. (2015).
#' "Comparisons among ten models of acoustic backscattering used in aquatic ecosystem research."
#' The Journal of the Acoustical Society of America, 138(6), 3742-3764.
#' modified from:
#' Anderson, V. C. (1950). "Sound scattering from a fluid sphere."
#' The Journal of the Acoustical Society of America, 22(4), 426-431.
#' @param f Frequency in Hz (s^-1)
#' @param r Range in m from center of sphere
#' @param a Radius of sphere
#' @param c Soundspeed in surrounding fluid m/s
#' @param h Soundspeed contrast inside/surrounding fluid
#' @param g Density contrast inside/surrounding fluid
#' @param rho Density of surrounding fluid
#' @examples
#' fs <- as.list(seq(1,300, by=1)*1000) #Frequencies
#' r <- 10 #range
#' a <- 0.01 # radius
#' c <- 1500 #soundspeed surrounding fluid
#' rho <- 1026 #density surrounding fluid
#' g <- 1.0025 #density contrast
#' h <- 1.0025 #soundspeed contrast
#' TS <- sapply(fs,TS.sphere,r=r,a=a,c=c,h=h,g=g,rho=rho)
#' plot(fs,TS, type="l", xlab="Frequency [Hz]",ylab="TS [dB re m2]")
#' @export
fsphere <- function(f,a,c,h,g){

  #External fluid
  k0 <- kcalc(c,f) #Wavenumber surrounding fluid
  ka <- k0 * a #Wavenumber surrounding fluid * radius

  #Internal fluid
  k1 <- kcalc(h*c,f) #Wavenumber internal fluid
  k1a <- k1 * a  #Wavenumber internal fluid * radius
  #rho1 <- g *rho #density internal fluid

  Cn_fun <- function(n){
    ((Sbessj_p(k1a,n)*Sneum(ka,n))/(Sbessj(k1a,n)*Sbessj_p(ka,n)) -
       g*h*(Sneum_p(ka,n)/Sbessj_p(ka,n))) /

      ((Sbessj_p(k1a,n)*Sbessj(ka,n))/(Sbessj(k1a,n)*Sbessj_p(ka,n)) - g*h)
  }
  n <- seq(0,round(ka+20,0))
  Cn <- apply(matrix(n),1,Cn_fun)

  A <- -1/(1+1i*Cn)
  fbs <- -1i/k0*sum((-1)^n * (2*n +1) * A)
  TS <- 10*log10(abs(fbs)^2)
  return(TS)
}
