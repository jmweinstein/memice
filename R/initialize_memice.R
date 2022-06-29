

#' initialize_memice(): initialize workspace for proper formatting of memice output
#'
#' @return returns formatting functions in system.frame()
#' @export
#'
#' @examples
#' memice::initialize_memice()

initialize_memice <- function() {

  assign(
    x = "tidy.comparisons",
    value = tidy.comparisons <- utils::getFromNamespace(x = "tidy.comparisons", ns = "marginaleffects"),
    envir = sys.frame())

  assign(
    x = "tidy.marginaleffects",
    value = utils::getFromNamespace(x = "tidy.marginaleffects", ns = "marginaleffects"),
    envir = sys.frame())


  assign(
    x = "tidy.custom",
    value = tidy.custom <- function(x, ...) {
      out <- tidy.marginaleffects(x, ...)
      out$term <- paste0(out$term, " [", out$contrast, "]")
      return(out)
    },
    pos = sys.frame())

}




