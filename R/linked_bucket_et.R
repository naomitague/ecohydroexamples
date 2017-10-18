#' linked_bucket_et'
#' 
#' Function to compute profits   
#' @param S initial water storage, default 500mm
#' @param Scap maximum storage, default 1000 mm
#' @param Smin minimum storage, default 200mm
#' @param Recharge water added
#' @param gs stomatal conductance default 20mm/s
#' @param ga aerodyamic conductance default 100mm/s
#' @param K drainage rate mm/day default 1mm/day
#' @return data structure with the following
#' \describe{
#' \item{Q}{runoff mm/day}
#' \item{S}{storage mm}
#'  \item{AET}{Actual Evapotranspiration mm/day}
#'  \item{PET}{PET from Penman monteith mm/day}
#'  \item{recharge}{water input to system mm/day}
#'}


linked_bucket_et = function(Sini=500, Scap=1000, Smin=200, recharge, K=1, metdata, gs=20,ga=100, CP=1010, Pair=101325) 
{
  
  ntime = min(nrow(metdata), length(recharge))
  
  res = as.data.frame(matrix(nrow=ntime, recharge[1:ntime]))
  colnames(res)="recharge"
  res$Q=0
  res$AET=0
  res$S = 0
  res$S[1]=Sini
  res$PET=0
  
  
  for (t in 2:ntime) {
    PET = penman_montieth(metdata$tavg[t], metdata$vpd[t], metdata$rnet[t], gs=20, ga=100)
    tmp = bucket_model(res$S[t-1], Scap, Smin, res$recharge[t], PET, K)
    res$S[t]=tmp$S
    res$AET[t]=tmp$AET
    res$Q[t]=tmp$Q
    res$PET[t]=PET
  }
  return(res)
}




