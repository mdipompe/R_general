#Cosmology calculator - get various distances, etc for a given redshift
#Returns data frame with several bits of information
cosmocalc <- function(z = 1.0, h = 0.71, om = 0.27, lambda = 0.73) {
  H0 <- h * 100.
  ok <- 1. - om - lambda
  
  #Set speed of light
  c <- 2.99792458e5
  
  #Convert H0 to s^-1, get hubble distance
  H0_conv <- H0 / 3.0859e19
  d_h <- (c/H0_conv) / (3.0859e19)
  t_h <- (1./H0_conv) * (1./3600.) * (1./24.) * (1./365.)
  
  #Define integrands, integrate
  E <- function(x) {1./sqrt(om * ((1 + x)^3) + ok * ((1 + z)^2) + lambda)}
  E2 <- function(x) {1./((1 + x) * sqrt(om * ((1 + x)^3) + 
                                          ok * ((1 + z)^2) + lambda))}
  y <- integrate(E, lower=0, upper=z)
  y2 <- integrate(E2, lower=0, upper=z)

  #Comoving distance
  d_c <- d_h * y$value
  
  #Get d_m depending on curvature
  if (ok > 0) d_m <- d_h * (1./sqrt(ok)) * sinh(sqrt(ok)*(d_c/d_h))
  if (ok == 0) d_m <- d_c
  if (ok < 0) d_m <- d_h * (1./sqrt(abs(ok))) * sin(sqrt(abs(ok))*(d_c/d_h))
  
  #Angular size distance
  d_a <- d_m / (1+z)
  #Luminosity distance
  d_l <- d_m * (1+z)
  #Comoving volume within z
  if (ok > 0) v_c=((4*pi*d_h^3)/(2*ok))*(((d_m/d_h)*sqrt(1+ok*(d_m^2/d_h^2)))
                                         -((1/sqrt(ok))*asinh(sqrt(ok)*(d_m/d_h))))
  if (ok == 0) v_c=((4*pi)/3)*d_m^3
  if (ok < 0) v_c((4*pi*d_h^3)/(2*ok))*(((d_m/d_h)*sqrt(1+ok*(d_m^2/d_h^2)))
                                        -((1/sqrt(abs(omega_k)))*
                                            asin(sqrt(abs(ok))*(d_m/d_h))))
  #lookback time
  t_l=t_h*y2$value*1e-9
  
  #make output dataframe
  return(data.frame(d_h=d_h, d_m=d_m, d_l=d_l, d_a=d_a, d_c=d_c,
                    v_c=v_c, t_l=t_l, t_h=t_h*1e-9))
}