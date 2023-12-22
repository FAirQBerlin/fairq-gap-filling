logging <- function(msg, ...) {
  futile.logger::flog.info(msg, ...)
}

log_debug <- function(msg, ...) {
  futile.logger::flog.debug(msg, ...)
}

log_warn <- function(msg, ...) {
  futile.logger::flog.warn(msg, ...)
}
