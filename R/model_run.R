.s2_vars <- c("age", "sex", "smoking", "sbp", "total_chol", "hdl", "diabetes",
              "region", "chol_units")
.s2_required <- c("age", "sex", "smoking", "sbp", "total_chol", "hdl", "region")
.s2_alias <- c(smoker = "smoking", current_smoker = "smoking",
               systolic_bp = "sbp", systolic = "sbp",
               cholesterol = "total_chol", tc = "total_chol",
               total_cholesterol = "total_chol", hdl_chol = "hdl",
               hdl_cholesterol = "hdl", risk_region = "region",
               gender = "sex")

.s2_normalize <- function(model_input, dots) {
  if (is.null(model_input)) {
    if (length(dots) == 0) return(NULL)
    model_input <- dots
  }
  if (is.list(model_input) && !is.data.frame(model_input)) {
    model_input <- model_input[!vapply(model_input, is.null, logical(1))]
  }
  out <- as.list(model_input)
  if (length(out) == 0) return(NULL)
  names(out) <- tolower(names(out))
  for (a in intersect(names(out), names(.s2_alias))) {
    canon <- .s2_alias[[a]]
    if (!canon %in% names(out)) names(out)[match(a, names(out))] <- canon
  }
  out <- out[intersect(names(out), .s2_vars)]
  out[!vapply(out, function(v) is.character(v) && length(v) == 1L &&
                !nzchar(trimws(v)), logical(1))]
}

#' SCORE2 (ModelsCloud entry point)
#'
#' Accepts inputs wrapped in `model_input` or as named arguments. See [score2()].
#'
#' @param model_input Named list. Required: `age`, `sex`, `smoking`, `sbp`,
#'   `total_chol`, `hdl`, `region`. Optional: `diabetes`, `chol_units`.
#' @param ... The same fields as named arguments.
#' @return A data frame with `model` and `risk`.
#' @examples
#' model_run(get_default_input())
#' @export
model_run <- function(model_input = NULL, ...) {
  args <- .s2_normalize(model_input, list(...))
  if (is.null(args) || length(args) == 0) args <- get_default_input()
  missing <- setdiff(.s2_required, names(args))
  if (length(missing) > 0) {
    stop("Missing required variable(s): ", paste(missing, collapse = ", "),
         ".", call. = FALSE)
  }
  do.call(score2, args)
}

#' Example SCORE2 inputs
#' @param n Optional number of rows to return.
#' @param ... Ignored.
#' @return A data frame of example patients.
#' @export
get_sample_input <- function(n = NULL, ...) {
  df <- data.frame(
    age = c(50, 65, 73), sex = c("male", "male", "female"),
    smoking = c("no", "yes", "no"), sbp = c(140, 160, 150),
    total_chol = c(6.3, 7.0, 6.0), hdl = c(1.4, 1.0, 1.4),
    diabetes = c("no", "no", "no"),
    region = c("moderate", "very_high", "moderate"),
    stringsAsFactors = FALSE
  )
  if (!is.null(n)) {
    if (!is.numeric(n) || length(n) != 1L || n < 1L) {
      stop("`n` must be a single positive integer.", call. = FALSE)
    }
    df <- utils::head(df, n)
  }
  df
}

#' Default SCORE2 input
#'
#' A 50-year-old non-smoking man in a moderate-risk region.
#' @param ... Ignored.
#' @return A named list.
#' @export
get_default_input <- function(...) {
  list(age = 50, sex = "male", smoking = "no", sbp = 140, total_chol = 6.3,
       hdl = 1.4, diabetes = "no", region = "moderate")
}
