#' compute_budyko2
#'
#' THis evaporative index (ratio of AET/P) using Budyko
#' @param      PET potential evapotranspiration
#' @param    P precipitation
#' @param     w adjustment parameter (default 2.63)
#'
#' @author Naomi
#' @return Evaporative index (ratio), Q (streamflow), AET (actual evapotranspration)


compute_budyko2 =
function(P, PET, w=2.63)
{
   evapindex = 1+ PET/P-(1+(PET/P)**w)**(1/w)

   Q = P*(1-evapindex)
  # return from your function
  return(list(EI = evapindex, AET=P*evapindex, Q=Q))

}
