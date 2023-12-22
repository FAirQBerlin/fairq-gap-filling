#' Run the ETL
#'
#' Starts the ETL.
#'
#' @param tables (list) see result of \code{data_sources()}
#'
#' @export
main <- function(tables = data_sources()) {
  res <- lapply(tables, function(x) try(etl(x)))
  invisible(
    if (any(unlist(lapply(res, inherits, what = "try-error")))) 1
    else 0
  )
}
