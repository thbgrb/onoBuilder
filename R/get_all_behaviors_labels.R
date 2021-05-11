#' finding all behaviors to create a vector
#'
#' @param data a data table normalized by the build_start_stop_table function
#'
#' @return a vector which contain all the behaviors observed 
#' @export
get_all_behaviors_labels <- function(data, observation_column, subject_column){
  
  data <- data %>%
    filter(observation %in% observation_column) %>%
    filter(subject %in% subject_column)
  
  behaviors <- c()
  for(b in data$behavior){
    if((b %in% behaviors)==0){
      behaviors <- c(behaviors, b)
    }
  }
  return(behaviors)
}