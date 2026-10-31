#' Create a request for the WebDAV server
#'
#' This function creates a base request for the WebDAV server with proper authentication.
#' It validates the provided parameters and handles errors during the connection setup.
#'
#' @param base_url The base URL of the WebDAV server (e.g., "https://example.com/remote.php/dav/files/").
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param verbose Logical. If TRUE, prints detailed messages during the request creation process.
#'
#' @return An `httr2_request` object with authentication and base URL configured, or an error message if the connection fails.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Create a request
#' if (!inherits(test_server, "try-error"))
#'   req <- webdav_create_request(base_url = test_server$url, verbose = TRUE)
#' @export
webdav_create_request <- function(base_url,
                                  username = Sys.getenv("WEBDAV_USERNAME"),
                                  password = Sys.getenv("WEBDAV_PASSWORD"),
                                  verbose = FALSE) {

  # Validate base_url
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Base URL: {.url {base_url}}"))
    cli::cli_inform(c("i" = "Username: {.val {ifelse(username != '', username, 'Not provided')}}"))
  }

  # Create the request object
  tryCatch({
    req <- httr2::request(httpuv::encodeURI(base_url))

    if (verbose) {
      cli::cli_inform(c("v" = "Request object created successfully."))
    }

    # Add basic authentication if username and password are provided
    if (username != "" && password != "") {
      req <- req |>
        httr2::req_auth_basic(username, password)

      if (verbose) {
        cli::cli_inform(c("v" = "Basic authentication added."))
      }
    } else if (verbose) {
      cli::cli_inform(c("!" = "No authentication added."))
    }

    return(req)

  }, error = function(e) {
    cli::cli_abort("Failed to create WebDAV request: {e$message}")
  })
}

#' Copy a resource on the WebDAV server
#'
#' This function copies a resource from one URI to another on the WebDAV server using the COPY method.
#' It validates the provided parameters and handles errors during the copy process.
#'
#' @param base_url The base URL of the WebDAV server.
#' @param from_path The source path of the resource to copy.
#' @param to_path The destination path where the resource will be copied.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param overwrite Logical. If TRUE (the default, matching the WebDAV protocol default), an existing resource at `to_path` is overwritten.
#' @param verbose Logical. If TRUE, prints detailed messages during the copy process.
#'
#' @return `TRUE` if the resource was copied successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Copy a file on the WebDAV server
#' if (!inherits(test_server, "try-error"))
#'   webdav_copy_file(base_url = test_server$url,
#'     from_path = "Project.pdf",
#'     to_path = "Project-copy.pdf",
#'     verbose = TRUE)
#' @export
webdav_copy_file <- function(base_url, from_path, to_path,
                             username = Sys.getenv("WEBDAV_USERNAME"),
                             password = Sys.getenv("WEBDAV_PASSWORD"),
                             overwrite = TRUE,
                             verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(from_path) || !is.character(from_path) || nchar(from_path) == 0) {
    cli::cli_abort("The {.arg from_path} parameter is required and must be a non-empty string.")
  }

  if (missing(to_path) || !is.character(to_path) || nchar(to_path) == 0) {
    cli::cli_abort("The {.arg to_path} parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Base URL: {.url {base_url}}"))
    cli::cli_inform(c("i" = "Copying from: {.path {from_path}} to: {.path {to_path}}"))
  }

  # Create the source and destination paths
  tryCatch({
    s_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ from_path }")
    d_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ to_path }")

    if (verbose) {
      cli::cli_inform(c("i" = "Source Path: {.url {s_path}}"))
      cli::cli_inform(c("i" = "Destination Path: {.url {d_path}}"))
    }

    # Create the base request using the previously created function
    req <- webdav_create_request(s_path, username, password, verbose)

    # Set the Destination header and use the COPY method. The request URL is
    # percent-encoded inside webdav_create_request(), so the Destination
    # header must be encoded the same way.
    req <- req |>
      httr2::req_headers(
        Destination = httpuv::encodeURI(d_path),
        Overwrite = if (isTRUE(overwrite)) "T" else "F"
      ) |>
      httr2::req_method("COPY")

    # Perform the request; handle HTTP status explicitly instead of letting
    # httr2 abort, so failures produce a precise message.
    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      cli::cli_alert_success("Resource successfully copied from {.path {from_path}} to {.path {to_path}}")
      TRUE
    } else {
      cli::cli_warn("Failed to copy resource. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error during the copy process: {conditionMessage(e)}")
    FALSE
  })
}

#' Move or rename a resource on the WebDAV server
#'
#' This function moves (or renames) a resource from one URI to another on the
#' WebDAV server using the MOVE method. It validates the provided parameters
#' and handles errors during the move process.
#'
#' @param base_url The base URL of the WebDAV server.
#' @param from_path The source path of the resource to move.
#' @param to_path The destination path of the resource. Moving within the same folder renames the resource.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param overwrite Logical. If TRUE (the default, matching the WebDAV protocol default), an existing resource at `to_path` is overwritten.
#' @param verbose Logical. If TRUE, prints detailed messages during the move process.
#'
#' @return `TRUE` if the resource was moved successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Rename a file on the WebDAV server
#' if (!inherits(test_server, "try-error"))
#'   webdav_move_file(base_url = test_server$url,
#'     from_path = "Notes.txt",
#'     to_path = "Notes-renamed.txt",
#'     verbose = TRUE)
#' @export
webdav_move_file <- function(base_url, from_path, to_path,
                             username = Sys.getenv("WEBDAV_USERNAME"),
                             password = Sys.getenv("WEBDAV_PASSWORD"),
                             overwrite = TRUE,
                             verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(from_path) || !is.character(from_path) || nchar(from_path) == 0) {
    cli::cli_abort("The {.arg from_path} parameter is required and must be a non-empty string.")
  }

  if (missing(to_path) || !is.character(to_path) || nchar(to_path) == 0) {
    cli::cli_abort("The {.arg to_path} parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Base URL: {.url {base_url}}"))
    cli::cli_inform(c("i" = "Moving from: {.path {from_path}} to: {.path {to_path}}"))
  }

  tryCatch({
    s_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ from_path }")
    d_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ to_path }")

    if (verbose) {
      cli::cli_inform(c("i" = "Source Path: {.url {s_path}}"))
      cli::cli_inform(c("i" = "Destination Path: {.url {d_path}}"))
    }

    req <- webdav_create_request(s_path, username, password, verbose)

    # The request URL is percent-encoded inside webdav_create_request(), so
    # the Destination header must be encoded the same way.
    req <- req |>
      httr2::req_headers(
        Destination = httpuv::encodeURI(d_path),
        Overwrite = if (isTRUE(overwrite)) "T" else "F"
      ) |>
      httr2::req_method("MOVE")

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      cli::cli_alert_success("Resource successfully moved from {.path {from_path}} to {.path {to_path}}")
      TRUE
    } else {
      cli::cli_warn("Failed to move resource. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error during the move process: {conditionMessage(e)}")
    FALSE
  })
}

#' Download a file from the WebDAV server
#'
#' This function downloads a file from the WebDAV server and saves it to a local directory.
#' It validates the provided parameters, handles errors, and optionally prints detailed logs if requested.
#'
#' @param base_url The base URL of the WebDAV server (e.g., "https://example.com/remote.php/dav/files/").
#' @param file_path The path of the file on the WebDAV server to download (relative to the `base_url`).
#' @param destination_path The local directory where the downloaded file will be saved. Defaults to the current directory.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param verbose Logical. If TRUE, prints detailed messages during the download process.
#'
#' @return `TRUE` if the file was downloaded successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @examples
#' # Example usage with a public WebDAV server.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Download a file from the WebDAV server
#' if (!inherits(test_server, "try-error"))
#'   webdav_download_file(base_url = test_server$url,
#'     file_path = "Project.pdf",
#'     destination_path = tempdir(),
#'     verbose = TRUE)
#' # Visit test_server$url to view the results of the operation.
#' @export
webdav_download_file <- function(base_url, file_path, destination_path = ".",
                                 username = Sys.getenv("WEBDAV_USERNAME"),
                                 password = Sys.getenv("WEBDAV_PASSWORD"),
                                 verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
    cli::cli_abort("The {.arg file_path} parameter is required and must be a non-empty string.")
  }

  # Build the full local path
  file_name <- basename(file_path)
  dest_dir <- if (is.null(destination_path) || nchar(destination_path) == 0) {
    "."
  } else {
    stringr::str_remove(destination_path, "/$")
  }

  if (!dir.exists(dest_dir)) {
    cli::cli_abort("The destination directory {.path {dest_dir}} does not exist.")
  }

  local_path <- file.path(dest_dir, file_name)

  if (verbose) {
    cli::cli_inform(c("i" = "Base URL: {.url {base_url}}"))
    cli::cli_inform(c("i" = "Downloading from: {.path {file_path}} to: {.path {local_path}}"))
  }

  # Build the full URL of the file on the WebDAV server
  tryCatch({
    server_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ stringr::str_remove(file_path, '^/') }")

    if (verbose) {
      cli::cli_inform(c("i" = "Server Path: {.url {server_path}}"))
      cli::cli_inform(c("i" = "Local Destination Path: {.path {local_path}}"))
    }

    req <- webdav_create_request(server_path, username, password, verbose)

    # Stream the body straight to disk instead of holding it in memory.
    # Download to a temporary file first so a failed request never leaves a
    # partial or error-page file at the destination.
    tmp_path <- tempfile(pattern = paste0(file_name, "-"), tmpdir = dest_dir)
    on.exit(unlink(tmp_path), add = TRUE)

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform(path = tmp_path)

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      file.rename(tmp_path, local_path)
      if (verbose) {
        cli::cli_alert_success("Resource successfully downloaded from {.url {server_path}}")
        cli::cli_alert_success("Resource successfully written to {.path {local_path}}")
      }
      TRUE
    } else {
      cli::cli_warn("Failed to download resource. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error during the download process: {conditionMessage(e)}")
    FALSE
  })
}

#' Create a collection (directory) on a WebDAV server
#'
#' This function creates a collection (directory/folder) on the WebDAV server
#' using the MKCOL method. It validates parameters and handles errors during the process.
#'
#' @param base_url The base URL of the WebDAV server (e.g., "https://example.com/remote.php/dav/files/").
#' @param folder_path The path of the directory to create.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param verbose Logical. If TRUE, prints detailed messages during the directory creation process.
#'
#' @return `TRUE` if the collection was created successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom httpuv encodeURI
#' @importFrom stringr str_remove
#' @examples
#'
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Create a directory on the WebDAV server
#' if (!inherits(test_server, "try-error"))
#'   webdav_create_directory(base_url = test_server$url, folder_path = "Test_Folder", verbose = TRUE)
#' @export
webdav_create_directory <- function(base_url, folder_path,
                                    username = Sys.getenv("WEBDAV_USERNAME"),
                                    password = Sys.getenv("WEBDAV_PASSWORD"),
                                    verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(folder_path) || !is.character(folder_path) || nchar(folder_path) == 0) {
    cli::cli_abort("The {.arg folder_path} parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Folder path to create: {.path {folder_path}}"))
  }

  # Create the directory path
  tryCatch({
    path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ folder_path }")

    # Create the base request using the previously created function
    req <- webdav_create_request(path, username, password, verbose)

    # Set the MKCOL method to create the directory
    req <- req |>
      httr2::req_method("MKCOL")

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      cli::cli_alert_success("Collection successfully created at: {.path {folder_path}}")
      TRUE
    } else {
      cli::cli_warn("Failed to create collection. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error during directory creation: {conditionMessage(e)}")
    FALSE
  })
}

#' Upload a file to the WebDAV server
#'
#' This function uploads a file to a specific folder on the WebDAV server. It validates the provided parameters and handles errors during the process.
#'
#' @param base_url The base URL of the WebDAV server.
#' @param local_path The local path of the file to be uploaded.
#' @param server_path The folder path on the WebDAV server where the file will be uploaded.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param timeout The timeout for the upload request in seconds (default is 300 seconds).
#' @param verbose Logical value indicating whether to print detailed debug messages. When TRUE, the function outputs additional information about its progress and actions.
#'
#' @return `TRUE` if the file was uploaded successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_file req_timeout req_error
#' @importFrom glue glue
#' @importFrom stringr str_remove str_trim
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Upload a file
#' file_test <- tempfile(pattern = "teste_", fileext = ".txt")
#' cat("Text file content", file = file_test)
#' if (!inherits(test_server, "try-error"))
#'   webdav_upload_file(base_url = test_server$url, local_path = file_test, verbose = TRUE)
#' @export
webdav_upload_file <- function(base_url,
                               local_path,
                               server_path = "",
                               username = Sys.getenv("WEBDAV_USERNAME"),
                               password = Sys.getenv("WEBDAV_PASSWORD"),
                               timeout = 300,
                               verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(local_path) || !file.exists(local_path)) {
    cli::cli_abort("The {.arg local_path} parameter is required and the file must exist.")
  }

  # Expand and clean local path
  local_path <- path.expand(stringr::str_trim(local_path))
  file_name <- basename(local_path)

  # Handle folder_path and construct URL
  path <- if (is.null(server_path) || nchar(server_path) == 0) {
    glue::glue("{stringr::str_remove(base_url, '/$')}/{ file_name }") # Root folder
  } else {
    glue::glue("{stringr::str_remove(base_url, '/$')}/{stringr::str_remove(server_path, '/$')}/{ file_name }")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Uploading file: {.path {local_path}}"))
    cli::cli_inform(c("i" = "Target URL: {.url {path}}"))
  }

  # Try to upload the file
  tryCatch({
    # req_body_file() streams the file from disk instead of loading it
    # entirely into memory.
    req <- webdav_create_request(path, username, password, verbose) |>
      httr2::req_body_file(local_path) |>
      httr2::req_method("PUT") |>
      httr2::req_timeout(timeout)

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      cli::cli_alert_success("File successfully uploaded to: {.path {if (is.null(server_path) || nchar(server_path) == 0) 'root' else server_path}}")
      TRUE
    } else {
      cli::cli_warn("Failed to upload file. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error uploading the file: {conditionMessage(e)}")
    FALSE
  })
}

#' List files from a specific folder on WebDAV server
#'
#' This function lists the files in a specific folder on the WebDAV server. If no folder path is provided, it lists files from the root directory. The function validates the provided parameters and handles errors during the process.
#'
#' @param base_url The base URL of the WebDAV server.
#' @param folder_path The path inside WebDAV where the files are located. If not provided or empty, the root folder will be listed.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param depth The depth of the PROPFIND request (default is 1).
#' @param verbose Logical value indicating whether to print detailed debug messages. When TRUE, the function outputs additional information about its progress and actions.
#'
#' @return A tibble containing:
#' \describe{
#'   \item{display_name}{The name of the file or directory.}
#'   \item{full_path}{The full URL (href) of the resource.}
#'   \item{creation_date}{The date the resource was created.}
#'   \item{last_modified}{The date the resource was last modified.}
#'   \item{content_length}{The size of the resource in bytes (NA for directories).}
#'   \item{is_folder}{Logical indicating whether the resource is a directory.}
#' }
#' Returns `NULL` if an error occurs during the execution of the function.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom dplyr mutate slice_tail first
#' @importFrom tidyr replace_na
#' @importFrom purrr map_dfr
#' @importFrom stringr str_remove
#' @importFrom xml2 read_xml xml_find_all
#' @importFrom httpuv encodeURI decodeURI
#' @importFrom tibble tibble
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # List files in a directory
#' if (!inherits(test_server, "try-error"))
#'   webdav_list_files(base_url = test_server$url, folder_path = "Sales/", verbose = TRUE)
#'
#' @export
webdav_list_files <- function(
    base_url,
    folder_path = NULL,
    username = Sys.getenv("WEBDAV_USERNAME"),
    password = Sys.getenv("WEBDAV_PASSWORD"),
    depth = 1,
    verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }


  # Validate base_url
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  # Handle folder_path and construct URL
  path <- if (is.null(folder_path) || nchar(folder_path) == 0) {
    glue::glue("{stringr::str_remove(base_url, '/$')}/") # Root folder
  } else {
    glue::glue("{stringr::str_remove(base_url, '/$')}/{stringr::str_remove(folder_path, '/$')}")
  }

  if (verbose) {
    cli::cli_inform(c("i" = "Listing files in folder: {.path {ifelse(is.null(folder_path) || nchar(folder_path) == 0, 'root', folder_path)}}"))
    cli::cli_inform(c("i" = "Full URL: {.url {path}}"))
  }

  # Try to list files in the folder
  tryCatch({
    req <- webdav_create_request(path, username, password, verbose) |>
      httr2::req_method("PROPFIND") |>
      httr2::req_headers("Depth" = as.character(depth))

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    # Handle response and parse XML
    status <- httr2::resp_status(response)
    if (status < 400) {
      contents <- parse_propfind_response(httr2::resp_body_string(response))

      if (verbose) {
        cli::cli_alert_success("Files listed successfully.")
      }
      contents
    } else {
      cli::cli_warn("Failed to list files. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      NULL
    }

  }, error = function(e) {
    cli::cli_warn("Error listing files in the folder: {conditionMessage(e)}")
    NULL
  })
}

# Parse a PROPFIND multistatus response body into a tibble, skipping the
# first entry (the folder itself).
##### https://github.com/StrategicProjects/webdav/issues/1
##### Code from Benjamin Buchwitz (bchwtz) patch
parse_propfind_response <- function(content) {
  xml_content <- xml2::read_xml(content)

  ns <- xml2::xml_ns(xml_content)
  xml_prefix <- names(ns[ns == "DAV:"])

  webdav_list <- xml_content |>
    xml2::xml_find_all(glue::glue(".//{ xml_prefix }:response"), ns, flatten = FALSE) |>
    xml2::as_list()

  purrr::map_dfr(webdav_list, function(x) {
    props <- x$propstat$prop
    tibble(
      full_path = httpuv::decodeURI(x$href[[1]]) %||% NA_character_,
      creation_date = props$creationdate[[1]] %||% NA_character_,
      last_modified = props$getlastmodified[[1]] %||% NA_character_,
      content_length = as.numeric(props$getcontentlength[[1]] %||% NA),
      # isFolder is a Microsoft extension; absent on most servers, so the
      # subscript yields NULL and %in% a zero-length logical — isTRUE()
      # keeps the result a scalar FALSE instead of NA.
      is_folder = !is.null(props$resourcetype$collection) ||
        isTRUE(props$isFolder[[1]] %in% c("1", "t"))
    )
  }) |>
    mutate(is_folder = tidyr::replace_na(is_folder, FALSE)) |>
    # fixed(): the folder's own path is a literal prefix, not a regex —
    # names with "(", "+", etc. must not be interpreted as metacharacters.
    mutate(display_name = stringr::str_remove(full_path, stringr::fixed(dplyr::first(full_path))), .before = full_path) |>
    slice_tail(n = -1)
}

#' Delete a file or directory from the WebDAV server
#'
#' This function deletes a file or directory on the WebDAV server using the DELETE method. It validates the provided parameters and handles errors during the process.
#'
#' @param base_url The base URL of the WebDAV server.
#' @param resource_path The path of the file or directory to delete on the WebDAV server.
#' @param username The username for WebDAV authentication. Defaults to the "WEBDAV_USERNAME" environment variable.
#' @param password The password for WebDAV authentication. Defaults to the "WEBDAV_PASSWORD" environment variable.
#' @param verbose Logical value indicating whether to print detailed debug messages. When TRUE, the function outputs additional information about its progress and actions.
#'
#' @return `TRUE` if the file or directory was deleted successfully, `FALSE` (with a warning) otherwise. Invalid arguments raise an error.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_error
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(httr2)
#' test_server <- "http://webdavserver.net/" |>
#'   request() |>
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) |>
#'   req_perform() |>
#'   try(silent = TRUE)
#'
#' # Delete a file or directory
#' if (!inherits(test_server, "try-error"))
#'   webdav_delete_resource(base_url = test_server$url, resource_path = "Notes.txt", verbose = TRUE)
#' @export
webdav_delete_resource <- function(base_url, resource_path,
                                   username = Sys.getenv("WEBDAV_USERNAME"),
                                   password = Sys.getenv("WEBDAV_PASSWORD"),
                                   verbose = FALSE) {

  if (!curl::has_internet()) {
    cli::cli_warn("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(FALSE))
  }

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    cli::cli_abort("The {.arg base_url} parameter is required and must be a non-empty string.")
  }

  if (missing(resource_path) || !is.character(resource_path) || nchar(resource_path) == 0) {
    cli::cli_abort("The {.arg resource_path} parameter is required and must be a non-empty string.")
  }

  # Construct the full path for the resource
  path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ stringr::str_remove(resource_path, '/$') }")

  if (verbose) {
    cli::cli_inform(c("i" = "Attempting to delete resource at: {.url {path}}"))
  }

  # Create and perform the DELETE request
  tryCatch({
    req <- webdav_create_request(path, username, password, verbose) |>
      httr2::req_method("DELETE")

    response <- req |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()

    status <- httr2::resp_status(response)
    if (status %in% 200:299) {
      cli::cli_alert_success("Resource successfully deleted at: {.path {resource_path}}")
      TRUE
    } else {
      cli::cli_warn("Failed to delete resource. Server responded with status {status} ({httr2::resp_status_desc(response)}).")
      FALSE
    }

  }, error = function(e) {
    cli::cli_warn("Error deleting the resource: {conditionMessage(e)}")
    FALSE
  })
}

