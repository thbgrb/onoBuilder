#' finding all observations to create a vector
#'
#' @param data a data table normalized by the build_start_stop_table function
#'
#' @return a vector which contain all the observations observed 
#' @export
getObservationsLabels <- function(data){
  
  observations <- c()
  
  for(o in data$observation){
    if((o %in% observations)==0){
      observations <- c(observations, o)
    }
  }

  return(sort(observations))
}
