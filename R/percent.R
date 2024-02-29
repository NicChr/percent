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
#' percent(0:100)
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

format.percent <- function(x, symbol = "%", ...){
  if (length(x) == 0){
    character()
  } else {
    paste0(format(unclass(x) * 100, ...), symbol)
  }
}

print.percent <- function(x, max = NULL, ...){
  out <- x
  N <- length(out)
  if (N == 0){
    print("percent(numeric(0))")
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
  out <- NextMethod("[")
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
new_percent <- function(x){
  class(x) <- "percent"
  x
}
