#' Run the converter in an interactive shiny ui
#'
#' @export
#' 
runOno <- function() {
  # Finding the shiny files in the package
  appDir <- system.file("shinyapp", package = "onoBuilder")
  
  if(appDir == "") {
    stop("Could not find example directory. Try reinstalling `onoBuilder`.",
         call. = FALSE)
  }
  
  # Run the shiny app
  shiny::runApp(appDir, display.mode = "normal")
}
