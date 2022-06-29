




#' mem_mice: estimate marginal effects at the means and modes using data objects created by mice::mice() (.mids)
#'
#' @param model a model object, either lm() or glm(), created using the original data with missing values
#' @param mice_object an object of class .mids, created using the mice::mice() function, containing imputed data
#' @param ... additional arguments (not intended to be used)
#'
#' @import utils
#' @import stats
#' @import dplyr
#' @import tibble
#' @import marginaleffects
#' @import mice
#' @import missMethods
#' @return a dataframe of the pooled marginal effect estimates (pooled using Rubin's rules)
#' @export
#'
#' @examples
#' ## Create dataset with missingness
#' data_with_missings <- mtcars |>
#'  missMethods::delete_MCAR(0.3, "cyl") |>
#'  dplyr::mutate(cyl = as.factor(cyl))
#'
#' ## Create model object
#' lm_mod <- lm(formula = mpg ~ wt + hp + cyl + cyl*wt + disp,
#'              data = data_with_missings)
#'
#' ## Impute data
#' imputation <- mice::mice(data = data_with_missings, m = 5, seed = 01701)
#'
#' ## Estimate and pool marginal effects at the means and modes
#' pooled_mfx <- memice::mem_mice(model = lm_mod, mice_object = imputation)
#' pooled_mfx



mem_mice <- function(model, mice_object, ...) {

  stopifnot("Model object is not lm() or glm()" = inherits(model, "glm") | inherits(model, "lm"))
  stopifnot("mice_object is not class mids (mids is the output of the mice::mice() function)" = inherits(mice_object, "mids"))

  part1 <- function(model, data) {

    mod <- stats::update(object = model, data = data)

    out <- marginaleffects::marginaleffects(mod, newdata = "mean")

    class(out) <- c("custom", class(out))

    return(out)

  }

  memice::initialize_memice()

  input <- mice::complete(mice_object, "all") ## MICE complete object

  ## WORKS
  model_imputation <-
    lapply(seq_along(input),
           FUN = function(x)
             part1(model = model,
                   data = input[[x]]))


  model_imputation <- suppressWarnings(mice::pool(model_imputation))
  output <- summary(model_imputation)
  output2 <- output |>
    dplyr::mutate(contrast = as.character(qdapRegex::ex_between(text.var = term, left = "[", right = "]"))) |>
    dplyr::relocate(contrast, .before = estimate) |>
    dplyr::select(-df) |>
    dplyr::mutate(term = qdapRegex::rm_square(term))

  return(output2)
}

