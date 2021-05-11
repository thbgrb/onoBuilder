#' Get the data table from a file
#'
#' @param inFile 
#' @param header 
#' @param sep 
#' @param quote 
#'
#' @return the table table imported
#' @export
#' @importFrom utils read.csv
get_data_table <- function(inFile, header, sep, quote){
  if (is.null(inFile))
    return(NULL)
  
  tryCatch({
    df <- read.csv(inFile$datapath, header, sep, quote)
  },
  error = function(e) {
    # return a safeError if a parsing error occurs
    stop(safeError(e))
  })
  
  return(df)
}

