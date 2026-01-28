#' Calculate arithmetic mean
#'
#' Calculates the arithmetic mean of a numeric vector.
#' Missing values can be removed via `na_rm`.
#'
#' @param x A numeric vector.
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#'
#' @return A single numeric value. Returns NA_real_ if `x` is empty after NA handling.
#' @examples
#' calc_mean(c(1, 2, 3))
#' calc_mean(c(1, NA, 3), na_rm = TRUE)
#' @export
calc_mean <- function(x, na_rm = FALSE) {
  if (length(x) == 0) return(NA_real_)
  
  if (is.logical(x)) x <- as.numeric(x)
  
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  mean(x, na.rm = FALSE)
}

#' Calculate median
#'
#' Calculates the median of a numeric vector.
#'
#' @param x A numeric vector.
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#'
#' @return A single numeric value. Returns NA_real_ if `x` is empty after NA handling.
#' @examples
#' calc_median(c(1, 2, 2, 10))
#' calc_median(c(1, NA, 3), na_rm = TRUE)
#' @export
calc_median <- function(x, na_rm = FALSE) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (length(x) == 0) return(NA_real_)
  
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  stats::median(x, na.rm = FALSE)
}

#' Calculate mode
#'
#' Calculates the statistical mode (most frequent value) of a vector.
#' Handles ties and "no mode" cases.
#'
#' **Tie handling:** if multiple values share the maximum frequency, returns a vector
#' of all modes (sorted).
#'
#' **No mode handling:** if all values occur with equal frequency (e.g., all unique),
#' returns NA.
#'
#' @param x A vector (numeric, character, factor).
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#'
#' @return A vector of modes, or NA if there is no mode, or NA if empty after NA handling.
#' @examples
#' calc_mode(c(1, 2, 2, 3))
#' calc_mode(c(1, 2, 3))  # no mode -> NA
#' calc_mode(c("a", "b", "b", "c"))
#' @export
calc_mode <- function(x, na_rm = FALSE) {
  if (length(x) == 0) return(NA)
  
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA)
  
  tbl <- table(x, useNA = "no")
  max_n <- max(tbl)
  
  # No mode if every value occurs once
  if (max_n == 1) return(NA)
  
  modes <- names(tbl)[tbl == max_n]
  modes <- sort(modes)
  
  if (is.numeric(x)) return(as.numeric(modes))
  modes
}

#' Calculate first quartile (Q1)
#'
#' Calculates the first quartile (25th percentile) of a numeric vector.
#'
#' @param x A numeric vector.
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#' @param type Quantile algorithm type passed to [stats::quantile()]. Defaults to 7.
#'
#' @return A single numeric value. Returns NA_real_ if `x` is empty after NA handling.
#' @examples
#' calc_q1(c(1, 2, 3, 4))
#' @export
calc_q1 <- function(x, na_rm = FALSE, type = 7) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (length(x) == 0) return(NA_real_)
  
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  as.numeric(stats::quantile(x, probs = 0.25, type = type, na.rm = FALSE, names = FALSE))
}

#' Calculate third quartile (Q3)
#'
#' Calculates the third quartile (75th percentile) of a numeric vector.
#'
#' @param x A numeric vector.
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#' @param type Quantile algorithm type passed to [stats::quantile()]. Defaults to 7.
#'
#' @return A single numeric value. Returns NA_real_ if `x` is empty after NA handling.
#' @examples
#' calc_q3(c(1, 2, 3, 4))
#' @export
calc_q3 <- function(x, na_rm = FALSE, type = 7) {
  if (!is.numeric(x)) stop("`x` must be a numeric vector.")
  if (length(x) == 0) return(NA_real_)
  
  if (na_rm) x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  
  as.numeric(stats::quantile(x, probs = 0.75, type = type, na.rm = FALSE, names = FALSE))
}

#' Calculate interquartile range (IQR)
#'
#' Calculates the interquartile range (IQR = Q3 - Q1) of a numeric vector.
#'
#' @param x A numeric vector.
#' @param na_rm Logical; if TRUE, remove NA values before computation.
#' @param type Quantile algorithm type passed to [stats::quantile()]. Defaults to 7.
#'
#' @return A single numeric value. Returns NA_real_ if `x` is empty after NA handling.
#' @examples
#' calc_iqr(c(1, 2, 3, 4))
#' @export
calc_iqr <- function(x, na_rm = FALSE, type = 7) {
  q1 <- calc_q1(x, na_rm = na_rm, type = type)
  q3 <- calc_q3(x, na_rm = na_rm, type = type)
  if (is.na(q1) || is.na(q3)) return(NA_real_)
  q3 - q1
}
