#' Run the converter in an interactive shiny ui
#'
#' @export
#'
#' @examples
#' if(interactive()) {
#'   runEx()
#' }
runEx <- function() {
  appDir <- system.file("shinyapp", package = "theObsConverter")
  if(appDir == "") {
    stop("Could not find example directory. Try reinstalling `theObsConverter`.",
         call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
