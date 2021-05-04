#' Transform data into a table of occurrences - non occurrences
#' @param df a \code{data.param} with a column of timestamps and a column of behaviors.
#' @param timestamp_column_name indicates the name of the timestamp column.
#' @param behavior_column_name specifies the name of the behaviour column.
#' @param select_behavior allows you to choose all the behaviors to be taken into account or specify the behaviors to be taken into account using a vector.
#' @return returns a \code{data.frame} of occurences - non occurences.
#' @export

## FIXIT: Régler le problème de select behavior où le tableau ono garde quand même les comportements qui n'ont pas été choisis...


build_ono_data <- function(df,
                           start = "start",
                           end = "end",
                           behavior_column_name = "behaviors",
                           select_behavior = "all",
                           ...) {
  
  # Building the table that will contain the user-specified behaviors, occurrences and 
  # non-occurrences of the behaviors. The timeline is also considered.
  
  # Creation of the header
  if (select_behavior[1]  == "all" && length(select_behavior) == 1) {
    header <- sort(unique(df[, behavior_column_name])) 
  } else {
    header <- select_behavior
  }
  
  # Creation of the table
  ono_df <- data.frame(matrix(data = 0,
                              # +1 is required to not exceed the table. 
                              # See line 41
                              nrow = max(df[, end] + 1), 
                              ncol = length(header)))
  names(ono_df) <- header
  
  for (line in 1:nrow(df)){
    
    x_start <- df[line, start]
    x_end <- df[line, end]
    #x_length <- x_start - x_end
    y <- df[line, behavior_column_name]
    
    for(x in x_start:x_end) {
      ono_df[x + 1, y] <- 1
    }
  }
  return(ono_df)
}

