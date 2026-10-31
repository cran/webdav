# Null-coalescing operator. Base R only ships `%||%` from 4.4.0 on, and the
# package declares R >= 4.1, so it needs its own definition.
`%||%` <- function(x, y) if (is.null(x)) y else x

#' Handle HTTP response from Server
#'
#' This function processes the response from the WebDAV server, checking for errors.
#'
#' @param response The response object from an `httr2` request.
#'
#' @return The processed response object if successful, or an error if the request failed.
#'
#' @export
handle_response <- function(response) {
  status <- httr2::resp_status(response)
  if (status >= 400) {
    cli::cli_abort("HTTP request failed with status: {status}")
  }
  response
}
