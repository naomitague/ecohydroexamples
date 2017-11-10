#' aero_cond
#'
#' Calculate aerodynamic conductance from windspeed and vegetation height
#'
#' @param     v= windspeed  (m/s) air temperature
#' @param    height (m)   vegetation height
#' @param     zomult    multiplier to estimate roughness from veg height
#' @param     zdmult    multiplier to estimate zero plane displacement from veg height

#' @author Naomi
#' @return Aerodynamic Conductance  (m/s)
aero_cond = function(v, vegheight, zomult=0.1, zdmult=0.7, zwind=10) {

  zd = zdmult*vegheight
  zo = zomult*vegheight
  a = 6.25*log( (zwind-zd)/zo)**2
  # convert to mm/s
  Cat = v/a*10
  return(Cat)
}
