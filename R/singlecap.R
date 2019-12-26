#' single Caps
#'
#' @param x a character string
#'
#' @return a character string with the first letter as a caps
#' @export

simpleCap <- function(x) {
  s <- strsplit(base::tolower(x), " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
