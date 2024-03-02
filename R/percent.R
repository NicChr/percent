#' Percent
#'
#' @description
#' A lightweight class for printing proportions as percentages.
#'
#' @param x Percentages.
#'
#' @returns
#' A class of object `percent`.
#'
#' @examples
#' library(percent)
#'
#' percent(0:10)
#'
#' # Convert proportions to percentages
#' percent(seq(0, 1, 0.1) * 100)
#'
#' # You can use round() as usual
#' p <- percent(15.56)
#' round(p)
#' round(p, digits = 1)
#'
#' p2 <- percent(0.0005)
#' signif(p2, 2)
#' floor(p2)
#' ceiling(p2)
#'
#' # We can do basic math operations as usual
#' 10 * percent(c(0, 50, 200))
#' percent(10) + percent(20)
#'
#' # Formatting options
#' format(percent(2.674), digits = 2, symbol = " (%)")
#' # Prints nicely in data frames (and tibbles)
#' x <- seq(0, 1, 0.1)
#' df <- data.frame(row.names = seq_along(x))
#' df$perc <- percent(x)
#' df
#'
#' @export
#' @rdname percent
percent <- function(x = numeric()){
  if (!is.numeric(x)){
    stop("x must be a numeric vector")
  }
  new_percent(x / 100)
}

as.character.percent <- function(x, ...){
  if (length(x) == 0){
    character()
  } else {
    paste0(unclass(x) * 100, "%")
  }
}

format.percent <- function(x, symbol = "%",
                           # dec_digits = 2,
                           ...){
  if (length(x) == 0){
    character()
  } else {
    paste0(format(unclass(x) * 100, ...), symbol)
    # paste0(format(round(unclass(x) * 100, dec_digits), ...), symbol)
  }
}

print.percent <- function(x, max = NULL,
                          # dec_digits = 2,
                          ...){
  out <- x
  N <- length(out)
  if (N == 0){
    print("percent(numeric())")
    return(invisible(x))
  }
  if (is.null(max)) {
    max <- getOption("max.print", 9999L)
  }
  suffix <- character()
  max <- min(max, N)
  if (max < N) {
    out <- out[seq_len(max)]
    suffix <- paste(" [ reached 'max' / getOption(\"max.print\") -- omitted",
                    N - max, "entries ]\n")
  }
  print(as.character(out), ...)
  # print(as.character(round(out, dec_digits)), ...)
  cat(suffix)
  invisible(x)
}

`[.percent` <- function(x, ..., drop = TRUE){
  cl <- oldClass(x)
  class(x) <- NULL
  out <- NextMethod("[")
  class(out) <- cl
  out
}

unique.percent <- function(x, incomparables = FALSE,
                           fromLast = FALSE, nmax = NA, ...){
  cl <- oldClass(x)
  class(x) <- NULL
  out <- NextMethod("unique")
  class(out) <- cl
  out
}

rep.percent <- function(x, ...){
  x[rep(seq_along(x), ...)]
}

rep_len.percent <- function(x, length.out){
  x[rep_len(seq_along(x), length.out)]
}

Ops.percent <- function(e1, e2){
  math <- switch(.Generic,
                 `+` =,
                 `-` =,
                 `*` =,
                 `/` =,
                 `^` =,
                 `%%` =,
                 `%/%` = TRUE, FALSE)
  both_percent <- inherits(e1, "percent") && inherits(e2, "percent")
  if (!both_percent){
    e1 <- unclass(e1)
    e2 <- unclass(e2)
  }
  NextMethod(.Generic)
}
Math.percent <- function(x, ...){
  rounding_math <- switch(.Generic,
                          `floor` =,
                          `ceiling` =,
                          `trunc` =,
                          `round` =,
                          `signif` = TRUE, FALSE)
  x <- unclass(x)
  if (rounding_math){
   x <- x * 100
   out <- NextMethod(.Generic)
   percent(out)
  } else {
    out <- NextMethod(.Generic)
    new_percent(out)
  }
}
new_percent <- function(x){
  class(x) <- "percent"
  x
}
