#' bucket'
#' 
#' Function to compute profits   
#' @param S storage
#' @param Scap maximum storage
#' @param Smin minimum storage  
#' @param Recharge water added
#' @param ET ET loss
#' @param K drainage rate 
#' @return both storage and Q
#' \describe{
#' \item{Q}{runoff}
#' \item{S}{storage}
#'  \item{AET}{Actual Evapotranspiration}
#'}


bucket_model = function(S, Scap, Smin, Recharge, PET, K) 
{
  AET=PET
  Q=K
  Snew = S + Recharge - PET - K
  if (Snew > Scap) {
      Q = Q+Snew-Scap
      Snew = Scap
  }
  else if (Snew < Smin ) {
    Sloss = S+Recharge-Smin
    if (Sloss > 0) {
      AET= Sloss/2
      Q = Sloss/2
    }
    else {
      AET=0
      Q=0
    }
    Snew = Smin
  }
  

return(list(S=Snew,Q=Q,AET=AET))
}




