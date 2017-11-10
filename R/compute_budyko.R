#' compute_budyko
#'
#' THis evaporative index (ratio of AET/P) using Budyko
#' @param      PET potential evapotranspiration
#' @param    P precipitation
#' @param     w adjustment parameter (default 2.63)
#'
#' @author Naomi
#' @return Evaporative index (ratio)


compute_budyko =
function(P, PET, w=2.63)
{
   evapindex = 1+ PET/P-(1+(PET/P)**w)**(1/w)

  # return from your function
  return(evapindex)

}
