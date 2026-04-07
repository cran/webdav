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
