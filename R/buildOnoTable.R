#' Transform data into a table of occurrences - non occurrences (ono)
#'
#' @param df a start/stop table created with the function buildStartStopTable 
#'           (with columns : start, end, observation, subject, behavior
#'
#' @return returns a table of occurences - non occurences.
#'
#' @export
#'
buildOnoTable <- function(df) {
  # Header of the ono file (behaviors)
  header <- sort(unique(df[, "behavior"]))
  
  # Creation of the table
  ono_df <- data.frame(matrix(
    data = 0,
    # +1 is required to not exceed the table.
    # See line 41
    nrow = max(df[, "end"] + 1),
    ncol = length(header)
  ))
  names(ono_df) <- header
  
  # For each second
  for (line in 1:nrow(df)) {
    # Finding start and end of each record in the start/stop table
    x_start <- as.numeric(df[line, "start"])
    x_end <- as.numeric(df[line, "end"])
    
    # Finding the behavior of the record
    y <- as.character(df[line, "behavior"])
    
    # Applying a 1 if the behavior is an occurrence at the second x
    for (x in x_start:x_end) {
      ono_df[x + 1, y] <- 1
    }
  }
  
  return(ono_df)
}
