#' Get the data table from a file
#'
#' @param inFile the file to import
#' @param header if the first line of the file is the header of the table
#' @param sep the separator used in the file
#' @param quote the quote used in the file
#'
#' @return the table table imported
#' 
#' @export
#' 
#' @importFrom utils read.csv
#' @importFrom readxl read_excel
#'
importDataTable <- function(inFile, header, sep, quote) {
  
  # Check if the file exist
  if (is.null(inFile))
    return(NULL)
  
  # Get file extension
  fileExtension = tolower(substr(inFile$name,
                                 nchar(inFile$name) - 3,
                                 nchar(inFile$name)))
  
  # Trying to open the csv file
  tryCatch({
    
    # Case of csv file
    if (fileExtension == ".csv") {
      df <- read.csv(inFile$datapath, header, sep, quote)
    } 
    
    # Case of excel file
    else if (fileExtension == "xlsx" | fileExtension == ".xls") {
      df <- read_excel(inFile$datapath)
    }
  },
  error = function(e) {
    print(e)
  })
  
  return(df)
}
