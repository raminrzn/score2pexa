# ---------------------------------------------------------------------------
# SCORE2 (ages 40-69) and SCORE2-OP (ages 70-89).
#
# SCORE2 working group and ESC Cardiovascular Risk Collaboration.
# Eur Heart J. 2021;42(25):2439-2454 (SCORE2) and 2455-2467 (SCORE2-OP).
#
# 1. Uncalibrated risk = 1 - S0 ^ exp(LP), sex-specific Cox model with age
#    interactions.
# 2. Region recalibration: risk = 1 - exp(-exp(a + b * log(-log(1 - risk)))).
#
# SCORE2 centres age at 60 (per 5 y), SBP at 120 (per 20), total cholesterol
# at 6 and HDL at 1.3 (per 0.5), all mmol/L. SCORE2-OP centres age at 73, SBP
# at 150, total cholesterol at 6 and HDL at 1.4, and subtracts a mean LP.
# ---------------------------------------------------------------------------

.s2_coef <- list(
  male = c(age = 0.3742, smoke = 0.6012, sbp = 0.2777, dm = 0.6457,
           tc = 0.1458, hdl = -0.2698, age_smoke = -0.0755,
           age_sbp = -0.0255, age_tc = -0.0281, age_hdl = 0.0426,
           age_dm = -0.0983, s0 = 0.9605),
  female = c(age = 0.4648, smoke = 0.7744, sbp = 0.3131, dm = 0.8096,
             tc = 0.1002, hdl = -0.2606, age_smoke = -0.1088,
             age_sbp = -0.0277, age_tc = -0.0226, age_hdl = 0.0613,
             age_dm = -0.1272, s0 = 0.9776)
)

.op_coef <- list(
  male = c(age = 0.0634, dm = 0.4245, smoke = 0.3524, sbp = 0.0094,
           tc = 0.0850, hdl = -0.3564, age_dm = -0.0174, age_smoke = -0.0247,
           age_sbp = -0.0005, age_tc = 0.0073, age_hdl = 0.0091,
           s0 = 0.7576, mean_lp = 0.0929),
  female = c(age = 0.0789, dm = 0.6010, smoke = 0.4921, sbp = 0.0102,
             tc = 0.0605, hdl = -0.3040, age_dm = -0.0107, age_smoke = -0.0255,
             age_sbp = -0.0004, age_tc = -0.0009, age_hdl = 0.0154,
             s0 = 0.8082, mean_lp = 0.2290)
)

# Region recalibration scales (intercept, slope) by model, region and sex.
.s2_scale <- list(
  SCORE2 = list(
    low       = list(male = c(-0.5699, 0.7476), female = c(-0.7380, 0.7019)),
    moderate  = list(male = c(-0.1565, 0.8009), female = c(-0.3143, 0.7701)),
    high      = list(male = c( 0.3207, 0.9360), female = c( 0.5710, 0.9369)),
    very_high = list(male = c( 0.5836, 0.8294), female = c( 0.9412, 0.8329))),
  `SCORE2-OP` = list(
    low       = list(male = c(-0.34, 1.19), female = c(-0.52, 1.01)),
    moderate  = list(male = c( 0.01, 1.25), female = c(-0.10, 1.10)),
    high      = list(male = c( 0.08, 1.15), female = c( 0.38, 1.09)),
    very_high = list(male = c( 0.05, 0.70), female = c( 0.38, 0.69)))
)

.s2_yesno <- function(x, field) {
  if (is.logical(x) && length(x) == 1L && !is.na(x)) return(as.integer(x))
  if (is.numeric(x) && length(x) == 1L && x %in% c(0, 1)) return(as.integer(x))
  key <- tolower(trimws(as.character(x)))
  if (key %in% c("yes", "y", "true", "1", "current")) return(1L)
  if (key %in% c("no", "n", "false", "0", "former", "never", "other")) return(0L)
  stop('Unrecognised `', field, '` value: "', x, '". Use yes/no or 1/0.',
       call. = FALSE)
}

.s2_sex <- function(x) {
  key <- tolower(trimws(as.character(x)))
  if (key %in% c("male", "m")) return("male")
  if (key %in% c("female", "f")) return("female")
  stop('Unrecognised `sex` value: "', x, '". Use "male" or "female".', call. = FALSE)
}

.s2_region <- function(x) {
  key <- gsub("[^a-z]+", "_", tolower(trimws(as.character(x))))
  key <- gsub("^_|_$", "", key)
  if (key %in% c("low", "moderate", "high", "very_high")) return(key)
  stop('Unrecognised `region`: "', x, '". Use "low", "moderate", "high" or ',
       '"very_high".', call. = FALSE)
}

#' SCORE2 / SCORE2-OP 10-year cardiovascular risk
#'
#' Uses SCORE2 for ages 40-69 and SCORE2-OP for ages 70-89, then recalibrates
#' to the chosen European risk region.
#'
#' @param age Age in years, 40-89.
#' @param sex `"male"` or `"female"`.
#' @param smoking Current smoker, yes/no.
#' @param sbp Systolic blood pressure, mmHg.
#' @param total_chol,hdl Total and HDL cholesterol.
#' @param diabetes Diabetes, yes/no. Default no. (The ESC recommends
#'   SCORE2-Diabetes for people with diabetes under 70.)
#' @param region `"low"`, `"moderate"`, `"high"` or `"very_high"` risk region.
#' @param chol_units `"mmol/L"` (default) or `"mg/dL"`.
#' @return A data frame with `model` and `risk` (0-1).
#' @references SCORE2 working group. *Eur Heart J.* 2021;42(25):2439-2454.
#'   \doi{10.1093/eurheartj/ehab309}; SCORE2-OP working group. *Eur Heart J.*
#'   2021;42(25):2455-2467. \doi{10.1093/eurheartj/ehab312}
#' @examples
#' score2(age = 50, sex = "male", smoking = "no", sbp = 140, total_chol = 6.3,
#'        hdl = 1.4, region = "moderate")
#' @export
score2 <- function(age, sex, smoking, sbp, total_chol, hdl, diabetes = "no",
                   region, chol_units = "mmol/L") {
  num_ok <- function(v) is.numeric(v) && length(v) == 1L && !is.na(v)
  if (!num_ok(age) || age < 40 || age > 89) {
    stop("`age` must be 40-89 (SCORE2 covers 40-69, SCORE2-OP 70-89).", call. = FALSE)
  }
  if (!num_ok(sbp) || sbp < 60 || sbp > 260) stop("`sbp` must be in mmHg.", call. = FALSE)
  if (!num_ok(total_chol) || !num_ok(hdl) || total_chol <= 0 || hdl <= 0) {
    stop("`total_chol` and `hdl` must be positive numbers.", call. = FALSE)
  }
  units <- tolower(gsub("\\s", "", chol_units))
  if (units %in% c("mg/dl", "mgdl")) {
    total_chol <- total_chol / 38.67
    hdl <- hdl / 38.67
  } else if (!units %in% c("mmol/l", "mmol")) {
    stop('`chol_units` must be "mmol/L" or "mg/dL".', call. = FALSE)
  }
  if (total_chol > 20) {
    stop("`total_chol` looks like mg/dL; set `chol_units = \"mg/dL\"`.", call. = FALSE)
  }
  sex <- .s2_sex(sex)
  smk <- .s2_yesno(smoking, "smoking")
  dm  <- .s2_yesno(diabetes, "diabetes")
  reg <- .s2_region(region)

  if (age < 70) {
    model <- "SCORE2"
    b <- .s2_coef[[sex]]
    ca <- (age - 60) / 5; cs <- (sbp - 120) / 20
    ct <- total_chol - 6; ch <- (hdl - 1.3) / 0.5
    lp <- b[["age"]] * ca + b[["smoke"]] * smk + b[["sbp"]] * cs +
      b[["dm"]] * dm + b[["tc"]] * ct + b[["hdl"]] * ch +
      b[["age_smoke"]] * ca * smk + b[["age_sbp"]] * ca * cs +
      b[["age_tc"]] * ca * ct + b[["age_hdl"]] * ca * ch +
      b[["age_dm"]] * ca * dm
    raw <- 1 - b[["s0"]]^exp(lp)
  } else {
    model <- "SCORE2-OP"
    b <- .op_coef[[sex]]
    ca <- age - 73; cs <- sbp - 150; ct <- total_chol - 6; ch <- hdl - 1.4
    lp <- b[["age"]] * ca + b[["dm"]] * dm + b[["smoke"]] * smk +
      b[["sbp"]] * cs + b[["tc"]] * ct + b[["hdl"]] * ch +
      b[["age_dm"]] * ca * dm + b[["age_smoke"]] * ca * smk +
      b[["age_sbp"]] * ca * cs + b[["age_tc"]] * ca * ct +
      b[["age_hdl"]] * ca * ch
    raw <- 1 - b[["s0"]]^exp(lp - b[["mean_lp"]])
  }
  sc <- .s2_scale[[model]][[reg]][[sex]]
  risk <- 1 - exp(-exp(sc[1] + sc[2] * log(-log(1 - raw))))
  data.frame(model = model, risk = risk, stringsAsFactors = FALSE)
}
