#' finding all subjects to create a vector
#'
#' @param data a data table normalized by the build_start_stop_table function
#' @param observation_column name of the observation column
#'
#' @return a vector which contain all the subjects observed 
#' @export
getSubjectsLabels <- function(data, observation_column){
 
   data <- data %>%
    filter(data$observation %in% observation_column)
  
  subjects <- c()
  for(s in data$subject){
    if((s %in% subjects)==0){
      subjects <- c(subjects, s)
    }
  }
                   
  return(sort(subjects))
}