#' Transforming data into start - stop data
#'
#' @param data the data file to transform
#' @param Event_Type the event type column
#' @param Time_Relative_sf the time relative column
#' @param Observation the observation column
#' @param Subject the subject column
#' @param Behavior the behavior column
#'
#' @return the table transform into a start stop table
#' @export
#'
#' @import dplyr
#' @import tidyr
build_start_stop_table <- function(data,
                                   Event_Type,
                                   Time_Relative_sf,
                                   Observation,
                                   Subject,
                                   Behavior) {
  #get data file
  result <- data %>%
    
    #select required columns
    select(Event_Type,
           Time_Relative_sf,
           Observation,
           Subject,
           Behavior,)  %>%
    
    #cleaning empty columns
    filter(Event_Type != "") %>%
    filter(Observation != "") %>%
    filter(Behavior != "") %>%
    filter(Subject != "") %>%
    
    #arranging data in a good order
    arrange(Observation, Subject, Behavior, Time_Relative_sf) %>%
    
    #merging rows
    group_by(row = ceiling(row_number() / 2)) %>%
    pivot_wider(names_from = Event_Type,
                values_from = c(Time_Relative_sf, Event_Type)) %>%
    ungroup() %>%
    select(starts_with(Time_Relative_sf),
           Observation,
           Subject,
           Behavior)
  
  #transforming columns name
  names(result)[1] <- 'start'
  names(result)[2] <- 'stop'
  names(result)[3] <- 'observation'
  names(result)[4] <- 'subject'
  names(result)[5] <- 'behavior'
  
  #transforming columns type
  result$start <- as.integer(result$start)
  result$stop <- as.integer(result$stop)
  result$behavior <- as.character(result$behavior)
  
  return(result)
}