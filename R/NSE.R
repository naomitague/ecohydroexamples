#' NSE
#' 
#' Nash-Sutcliffe Efficiency Performance Metric
#' @param m modelled value (any unit)
#' @param o observed value (any unit) 
#' @return nse  


NSE = function (m, o)        
{
    err = m - o
    meanobs = mean(o)
    mse = sum(err*err)
    ovar = sum((o-meanobs)*(o-meanobs))
    nse = 1.0 - mse/ovar
    nse
}
