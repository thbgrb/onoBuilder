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
buildStartStopTable <- function(data,
                                   Event_Type,
                                   Time_Relative_sf,
                                   Observation,
                                   Subject,
                                   Behavior) {
  
  #cleaning empty columns
  data <- data %>%
    filter(Event_Type != "") %>%
    filter(Observation != "") %>%
    filter(Behavior != "") %>%
    filter(Subject != "") 
  
  #FIRST TABLE: create the start stop table for the state point events
  res_point <- data %>%
    
    #taking only state point rows
    filter(Event_Type == "State point") %>%
    
    #select required columns
    select(Time_Relative_sf,
           Observation,
           Subject,
           Behavior)
  
  #transforming columns name
  names(res_point)[1] <- 'start' 
  names(res_point)[2] <- 'observation'
  names(res_point)[3] <- 'subject'
  names(res_point)[4] <- 'behavior' 
  
  #transforming columns type
  res_point$start <- as.integer(res_point$start)
  res_point$observation <- as.character(res_point$observation)
  res_point$subject <- as.character(res_point$subject)
  res_point$behavior <- as.character(res_point$behavior)
    
  #duplicate the time column
  res_point <- cbind(res_point, end=res_point$start)
    
  #arrange order of columns
  res_point <- res_point[,c(1, 5, 2, 3, 4)]
  
  
  #SECOND TABLE: create the start stop table for the start/stop events
  res_ss <- data %>%
    
    #not taking state point rows
    filter(Event_Type != "State point") %>%
    
    #select required columns
    select(Event_Type,
           Time_Relative_sf,
           Observation,
           Subject,
           Behavior)  %>%
    
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
  names(res_ss)[1] <- 'start'
  names(res_ss)[2] <- 'end'
  names(res_ss)[3] <- 'observation'
  names(res_ss)[4] <- 'subject'
  names(res_ss)[5] <- 'behavior'
  
  #transforming columns type
  res_ss$start <- as.integer(res_ss$start)
  res_ss$end <- as.integer(res_ss$end)
  res_ss$observation <- as.character(res_ss$observation)
  res_ss$subject <- as.character(res_ss$subject)
  res_ss$behavior <- as.character(res_ss$behavior)
  
  #FINAL TABLE: join the 2 tables created before
  res <- bind_rows(res_point, res_ss) 
  res <- arrange(res, res$start)
  
  return(res)
}