#' Transforming data into start - stop data
#'
#' @param data 
#' @param Event_Type 
#' @param Time_Relative_sf 
#' @param Observation 
#' @param Subject 
#' @param Behavior 
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
                                   Behavior
) {
  result <- data %>%
    select(Event_Type,
           Time_Relative_sf,
           Observation,
           Subject,
           Behavior,
    )  %>%
    filter(Event_Type!="") %>%
    filter(Observation!="") %>%
    filter(Behavior!="") %>%
    filter(Subject!="") %>%
    arrange(Observation, Subject, Behavior, Time_Relative_sf) %>%
    group_by(row = ceiling(row_number()/2)) %>%
    pivot_wider(names_from = Event_Type,
                values_from = c(Time_Relative_sf,
                                Event_Type
                ) )%>%
    ungroup() %>%
    select(
      starts_with(Time_Relative_sf),
      Observation,
      Subject,
      Behavior
    )
  View(result)
  

  ## Transforming column type
  result$`Time_Relative_sf_State start` <-
    as.integer(result$`Time_Relative_sf_State start`)
  result$`Time_Relative_sf_State stop` <-
    as.integer(result$`Time_Relative_sf_State stop`)
  result$Behavior <-
    as.character(result$Behavior)
  
  ## Transforming column name
  names(result)[1] <- 'start'
  names(result)[2] <- 'end'
  names(result)[3] <- 'observation'
  names(result)[4] <- 'subject'
  names(result)[5] <- 'behavior'
  
  return(result)
}