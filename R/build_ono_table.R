#' Transform data into a table of occurrences - non occurrences
#'
#' @param df a \code{data.param} with a column of timestamps and a column of behaviors.
#' @param start indicates the name of the state start column.
#' @param end indicates the name of the state end column.
#' @param behavior_column_name specifies the name of the behaviour column.
#' @param select_behavior allows you to choose all the behaviors to be taken into account or specify the behaviors to be taken into account using a vector.
#'
#' @return returns a \code{data.frame} of occurences - non occurences.
#' @export
build_ono_data <- function(df){
  
  # Building the table that will contain the user-specified behaviors, occurrences and 
  # non-occurrences of the behaviors. The timeline is also considered.
  
  # Creation of the header
#if (select_behavior[1]  == "all" && length(select_behavior) == 1) {
    header <- sort(unique(df[, "behavior"]))
 # } else {
  #  header <- select_behavior
 # }
  
  # Creation of the table
  ono_df <- data.frame(matrix(data = 0,
                              # +1 is required to not exceed the table. 
                              # See line 41
                              nrow = max(df[, "end"] + 1), 
                              ncol = length(header)))
  names(ono_df) <- header

  for (line in 1:nrow(df)){
    
    x_start <- as.numeric(df[line, "start"])
    x_end <- as.numeric(df[line, "end"]) 

    y <- as.character(df[line, "behavior"])

    for(x in x_start:x_end) {
      ono_df[x + 1, y] <- 1
    }
  }

  return(ono_df)
}

