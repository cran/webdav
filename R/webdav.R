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
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom magrittr %>%
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # Create a request
#' if (class(test_server) != "try-error")
#'   req <- webdav_create_request(base_url = test_server$url, verbose = TRUE)
#' @export
webdav_create_request <- function(base_url,
                                  username = Sys.getenv("WEBDAV_USERNAME"),
                                  password = Sys.getenv("WEBDAV_PASSWORD"),
                                  verbose = FALSE) {

  check_and_load_package("httr2")

  # Validate base_url
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    message("Base URL: ", base_url)
    message("Username: ", ifelse(username != "", username, "Not provided"))
  }

  # Create the request object
  tryCatch({
    req <- httr2::request(httpuv::encodeURI(base_url))

    if (verbose) {
      message("Request object created successfully.")
    }

    # Add basic authentication if username and password are provided
    if (username != "" && password != "") {
      req <- req %>%
        httr2::req_auth_basic(username, password)

      if (verbose) {
        message("Basic authentication added.")
      }
    } else if (verbose) {
      message("No authentication added.")
    }

    return(req)

  }, error = function(e) {
    message("Error creating request: ", e$message)
    stop("Failed to create WebDAV request.")
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
#' @param verbose Logical. If TRUE, prints detailed messages during the copy process.
#'
#' @return Logical value indicating whether the resource was copied successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @export
webdav_copy_file <- function(base_url, from_path, to_path,
                             username = Sys.getenv("WEBDAV_USERNAME"),
                             password = Sys.getenv("WEBDAV_PASSWORD"),
                             verbose = FALSE) {

  if (!curl::has_internet()) {
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }

  check_and_load_package("httr2")
  check_and_load_package("glue")
  check_and_load_package("stringr")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(from_path) || !is.character(from_path) || nchar(from_path) == 0) {
    stop("The 'from_path' parameter is required and must be a non-empty string.")
  }

  if (missing(to_path) || !is.character(to_path) || nchar(to_path) == 0) {
    stop("The 'to_path' parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    message("Base URL: ", base_url)
    message("Copying from: ", from_path, " to: ", to_path)
  }

  # Create the source and destination paths
  tryCatch({
    s_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ from_path }")
    d_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ to_path }")

    if (verbose) {
      message("Source Path: ", s_path)
      message("Destination Path: ", d_path)
    }

    # Create the base request using the previously created function
    req <- webdav_create_request(s_path, username, password, verbose)

    # Set the Destination header and use the COPY method
    req <- req %>%
      httr2::req_headers(Destination = d_path) %>%
      httr2::req_method("COPY")

    # Perform the request and handle the response
    response <- req %>%
      httr2::req_perform()

    if (httr2::resp_status(response) %in% c(200, 201, 204)) {
      message("Resource successfully copied from ", from_path, " to ", to_path)
      return(TRUE)
    } else {
      stop("Failed to copy resource. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error during the copy process: ", e$message)
    stop("Failed to copy resource.")
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
#' @return Logical value indicating whether the file was downloaded successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @examples
#' # Example usage with a public WebDAV server.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # Download a file from the WebDAV server
#' if (class(test_server) != "try-error")
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
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }


  check_and_load_package("httr2")
  check_and_load_package("glue")
  check_and_load_package("stringr")

  # Validar parâmetros
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(file_path) || !is.character(file_path) || nchar(file_path) == 0) {
    stop("The 'file_path' parameter is required and must be a non-empty string.")
  }

  # Definir caminho local completo
  file_name <- basename(file_path)
  local_path <- if (is.null(destination_path) || nchar(destination_path) == 0) {
    glue::glue("./{ file_name }") # Diretório atual
  } else {
    glue::glue("{stringr::str_remove(destination_path, '/$')}/{ file_name }")
  }

  if (verbose) {
    message("Base URL: ", base_url)
    message("Downloading from: ", file_path, " to: ", local_path)
  }

  # Construir URL completo do arquivo no servidor WebDAV
  tryCatch({
    server_path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ stringr::str_remove(file_path, '^/') }")

    if (verbose) {
      message("Server Path: ", server_path)
      message("Local Destination Path: ", local_path)
    }

    # Criar requisição
    req <- webdav_create_request(server_path, username, password, verbose)

    # Executar requisição
    response <- httr2::req_perform(req)

    if (httr2::resp_status(response) %in% c(200, 201, 204)) {
      if (verbose)
        message("Resource successfully downloaded from ", server_path)
      writeBin(object = httr2::resp_body_raw(response), con = local_path)
      if (verbose)
        message("Resource successfully written to ", local_path)
      return(TRUE)
    } else {
      stop("Failed to download resource. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error during the download process: ", e$message)
    stop("Failed to download resource.")
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
#' @return Logical value indicating whether the collection was created successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom httpuv encodeURI
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @examples
#'
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # Create a directory on the WebDAV server
#' if (class(test_server) != "try-error")
#'   webdav_create_directory(base_url = test_server$url, folder_path = "Test_Folder", verbose = TRUE)
#' @export
webdav_create_directory <- function(base_url, folder_path,
                                    username = Sys.getenv("WEBDAV_USERNAME"),
                                    password = Sys.getenv("WEBDAV_PASSWORD"),
                                    verbose = FALSE) {

  if (!curl::has_internet()) {
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }

  check_and_load_package("httr2")
  check_and_load_package("glue")
  check_and_load_package("stringr")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(folder_path) || !is.character(folder_path) || nchar(folder_path) == 0) {
    stop("The 'folder_path' parameter is required and must be a non-empty string.")
  }

  if (verbose) {
    message("Folder path to create: ", folder_path)
  }

  # Create the directory path
  tryCatch({
    path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ folder_path }")

    # Create the base request using the previously created function
    req <- webdav_create_request(path, username, password, verbose)

    # Set the MKCOL method to create the directory
    req <- req %>%
      httr2::req_method("MKCOL")

    # Perform the request and handle the response
    response <- req %>%
      httr2::req_perform()

    # Check if the response indicates success
    if (httr2::resp_status(response) %in% c(201, 200)) {
      message("Collection successfully created at: ", folder_path)
      return(TRUE)
    } else {
      stop("Failed to create collection. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error during directory creation: ", e$message)
    stop("Failed to create directory.")
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
#' @return Logical value indicating whether the file was uploaded successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom stringr str_remove str_trim
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # Upload a file
#' file_test <- tempfile(pattern = "teste_", fileext = ".txt")
#' cat("Text file content", file = file_test)
#' if (class(test_server) != "try-error")
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
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }

  check_and_load_package("httr2")
  check_and_load_package("glue")
  check_and_load_package("stringr")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(local_path) || !file.exists(local_path)) {
    stop("The 'local_path' parameter is required and the file must exist.")
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
    message("Uploading file: ", local_path)
    message("Target URL: ", path)
  }

  # Try to upload the file
  tryCatch({
    raw_file <- readBin(local_path, what = "raw", n = file.info(local_path)$size)

    req <- webdav_create_request(path, username, password, verbose) %>%
      httr2::req_body_raw(raw_file) %>%
      httr2::req_method("PUT") %>%
      httr2::req_options(timeout = timeout)

    # Perform the upload request
    response <- req %>%
      httr2::req_perform()

    # Check the response for success
    if (httr2::resp_status(response) %in% c(200, 201, 204)) {
      message("File successfully uploaded to: ", ifelse(is.null(server_path), "root", server_path), "\n")
      return(TRUE)
    } else {
      stop("Failed to upload file. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error uploading the file: ", conditionMessage(e), "\n")
    return(FALSE)
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
#'   \item{file_name}{The name of the file.}
#'   \item{relative_path}{The path of the file relative to the specified folder.}
#'   \item{lastmodified}{The date and time when the file was last modified.}
#'   \item{content_length}{The size of the file in bytes.}
#' }
#' Returns `NULL` if an error occurs during the execution of the function.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom dplyr filter mutate select arrange slice_tail
#' @importFrom stringr str_detect str_remove
#' @importFrom xml2 read_xml xml_find_all xml_text
#' @importFrom httpuv encodeURI decodeURI
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # List files in a directory
#' if (class(test_server) != "try-error")
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
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }


  check_and_load_package("httr2")
  check_and_load_package("xml2")
  check_and_load_package("stringr")
  check_and_load_package("dplyr")
  check_and_load_package("tibble")
  check_and_load_package("httpuv")


  # Validate base_url
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  # Handle folder_path and construct URL
  path <- if (is.null(folder_path) || nchar(folder_path) == 0) {
    glue::glue("{stringr::str_remove(base_url, '/$')}/") # Root folder
  } else {
    glue::glue("{stringr::str_remove(base_url, '/$')}/{stringr::str_remove(folder_path, '/$')}")
  }

  if (verbose) {
    message("Listing files in folder: ", ifelse(is.null(folder_path) || nchar(folder_path) == 0, "root", folder_path))
    message("Full URL: ", path)
  }

  # Try to list files in the folder
  tryCatch({
    req <- webdav_create_request(path, username, password, verbose) %>%
      httr2::req_method("PROPFIND") %>%
      httr2::req_headers("Depth" = as.character(depth))

    # Perform the request
    response <- req %>%
      httr2::req_perform()

    # Handle response and parse XML
    if (httr2::resp_status(response) < 400) {
      ##### https://github.com/StrategicProjects/webdav/issues/1
      ##### Code from Benjamin Buchwitz (bchwtz) patch
      content <- response %>% httr2::resp_body_string()
      xml_content <- xml2::read_xml(content)

      raw <- xml_content %>%
        xml2::xml_find_all(".//d:href") %>%
        xml2::xml_text() %>%
        httpuv::decodeURI()

      lastmodified <- xml_content %>%
        xml2::xml_find_all(".//d:getlastmodified") %>%
        xml2::xml_text() %>%
        httpuv::decodeURI()

      contentlength <- xml_content %>%
        xml2::xml_find_all(".//d:getcontentlength") %>%
        xml2::xml_text() %>%
        httpuv::decodeURI() %>%
        as.numeric()

      contentlength <- c(NA, contentlength) # Add NA as contentlength value for the folder

      # Process and filter files, skipping the first entry (root or folder itself)
      contents <- tibble::tibble(
        contents = stringr::str_remove(raw, raw[1]), # Remove the first entry, which is the folder itself
        full_path = raw,
        last_modified = lastmodified,
        content_length = contentlength
      ) %>%
        slice_tail(n = -1)
      #######

      if (verbose) {
        message("Files listed successfully.")
      }
      return(contents)
    } else {
      stop("Failed to list files. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error listing files in the folder: ", conditionMessage(e), "\n")
    return(NULL)
  })
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
#' @return Logical value indicating whether the file or directory was deleted successfully.
#' @importFrom httr2 req_perform req_auth_basic req_headers req_method req_body_raw req_options
#' @importFrom glue glue
#' @importFrom stringr str_remove
#' @importFrom magrittr %>%
#' @examples
#' # Example usage with a public WebDAV server.
#' # Visit test_server$url link to view the results of the operation.
#' library(magrittr)
#' library(httr2)
#' test_server <- "https://www.webdavserver.com/" %>%
#'   request() %>%
#'   req_retry(max_tries = 1, max_seconds = 2, backoff =  ~ 1) %>%
#'   req_perform() %>%
#'   try(silent = TRUE)
#'
#' # Delete a file or directory
#' if (class(test_server) != "try-error")
#'   webdav_delete_resource(base_url = test_server$url, resource_path = "Notes.txt", verbose = TRUE)
#' @export
webdav_delete_resource <- function(base_url, resource_path,
                                   username = Sys.getenv("WEBDAV_USERNAME"),
                                   password = Sys.getenv("WEBDAV_PASSWORD"),
                                   verbose = FALSE) {

  if (!curl::has_internet()) {
    message("No internet connection detected. You need an internet connection to use this function.")
    return(invisible(NULL))  # R
  }

  # Load necessary packages
  check_and_load_package("httr2")
  check_and_load_package("glue")
  check_and_load_package("stringr")

  # Validate parameters
  if (missing(base_url) || !is.character(base_url) || nchar(base_url) == 0) {
    stop("The 'base_url' parameter is required and must be a non-empty string.")
  }

  if (missing(resource_path) || !is.character(resource_path) || nchar(resource_path) == 0) {
    stop("The 'resource_path' parameter is required and must be a non-empty string.")
  }

  # Construct the full path for the resource
  path <- glue::glue("{ stringr::str_remove(base_url, '/$') }/{ stringr::str_remove(resource_path, '/$') }")

  if (verbose) {
    message("Attempting to delete resource at: ", path)
  }

  # Create and perform the DELETE request
  tryCatch({
    req <- webdav_create_request(path, username, password, verbose) %>%
      httr2::req_method("DELETE")

    # Perform the request
    response <- req %>%
      httr2::req_perform()

    # Check response and determine success
    if (httr2::resp_status(response) %in% c(200, 204)) {
      message("Resource successfully deleted at: ", resource_path)
      return(TRUE)
    } else {
      stop("Failed to delete resource. Server responded with status: ", httr2::resp_status(response))
    }

  }, error = function(e) {
    message("Error deleting the resource: ", conditionMessage(e))
    return(FALSE)
  })
}


