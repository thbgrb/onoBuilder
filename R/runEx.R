#' Run interactive FreqProf example (Shiny App)
#'
#' @export
#' @examples
#' # Only run this example in interactive R sessions
#' if(interactive()) {
#'   runEx()
#' }
runEx <- function() {
  #appDir <- system.file("shinyapp", package = "FreqProf")
  #appDir <- system.file("../inst/shinyapp/ui.R", package = "base")
  # if(appDir == "") {
  #   stop("Could not find example directory. Try reinstalling `FreqProf`.",
  #        call. = FALSE)
  # }
  shiny::runApp("R/ui.R", display.mode = "normal")
}
