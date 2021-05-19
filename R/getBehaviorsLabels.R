#' finding all behaviors to create a vector
#'
#' @param data a data table normalized by the build_start_stop_table function
#' @param observation_column name of the observation column
#' @param subject_column name of the subject column
#'
#' @return a vector which contain all the behaviors observed 
#' @export
getBehaviorsLabels <- function(data, observation_column, subject_column){
  
  data <- data %>%
    filter(data$observation %in% observation_column) %>%
    filter(data$subject %in% subject_column)
  
  behaviors <- c()
  for(b in data$behavior){
    if((b %in% behaviors)==0){
      behaviors <- c(behaviors, b)
    }
  }

  return(sort(behaviors))
}
