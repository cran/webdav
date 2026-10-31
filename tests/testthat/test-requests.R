# Offline tests for the request-building functions, using httr2 mocks.
# No network access is required: local_mocked_responses() intercepts
# req_perform().

base_url <- "https://example.com/remote.php/dav/files/user"

test_that("webdav_create_request validates base_url and adds auth", {
  expect_error(webdav_create_request(""), "base_url")

  req <- webdav_create_request(base_url, username = "u", password = "p")
  expect_s3_class(req, "httr2_request")
  expect_true(!is.null(req$options$userpwd) || !is.null(req$headers$Authorization))

  # Without credentials no auth is configured
  req_noauth <- webdav_create_request(base_url, username = "", password = "")
  expect_null(req_noauth$options$userpwd)
})

test_that("webdav_copy_file succeeds on 2xx and percent-encodes Destination", {
  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- req
    httr2::response(status = 201)
  })

  expect_true(
    webdav_copy_file(base_url, "pasta um/a.txt", "pasta dois/a.txt",
                     username = "u", password = "p")
  )
  expect_match(captured$headers$Destination, "pasta%20dois/a.txt", fixed = TRUE)
  expect_identical(captured$method, "COPY")
  expect_identical(captured$headers$Overwrite, "T")
})

test_that("webdav_copy_file warns and returns FALSE on an HTTP error", {
  httr2::local_mocked_responses(function(req) httr2::response(status = 423))

  expect_warning(
    res <- webdav_copy_file(base_url, "a.txt", "b.txt",
                            username = "u", password = "p"),
    "423"
  )
  expect_false(res)
})

test_that("webdav_move_file uses MOVE and honors overwrite = FALSE", {
  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- req
    httr2::response(status = 204)
  })

  expect_true(
    webdav_move_file(base_url, "a.txt", "b.txt",
                     username = "u", password = "p", overwrite = FALSE)
  )
  expect_identical(captured$method, "MOVE")
  expect_identical(captured$headers$Overwrite, "F")
  expect_match(captured$headers$Destination, "/b.txt", fixed = TRUE)
})

test_that("webdav_move_file validates its arguments", {
  expect_error(webdav_move_file("", "a", "b"), "base_url")
  expect_error(webdav_move_file(base_url, "", "b"), "from_path")
  expect_error(webdav_move_file(base_url, "a", ""), "to_path")
})

test_that("webdav_delete_resource returns FALSE with a warning on failure", {
  httr2::local_mocked_responses(function(req) httr2::response(status = 404))

  expect_warning(
    res <- webdav_delete_resource(base_url, "missing.txt",
                                  username = "u", password = "p"),
    "404"
  )
  expect_false(res)
})

test_that("webdav_download_file aborts early when the destination dir is missing", {
  expect_error(
    webdav_download_file(base_url, "a.txt",
                         destination_path = file.path(tempdir(), "nope-no-dir"),
                         username = "u", password = "p"),
    "does not exist"
  )
})

test_that("webdav_download_file leaves no file behind on an HTTP error", {
  httr2::local_mocked_responses(function(req) httr2::response(status = 404))

  dest <- withr::local_tempdir()
  expect_warning(
    res <- webdav_download_file(base_url, "missing.txt", destination_path = dest,
                                username = "u", password = "p"),
    "404"
  )
  expect_false(res)
  expect_length(list.files(dest), 0)
})

test_that("webdav_upload_file streams the file and reports success", {
  captured <- NULL
  httr2::local_mocked_responses(function(req) {
    captured <<- req
    httr2::response(status = 201)
  })

  src <- withr::local_tempfile(fileext = ".txt")
  writeLines("content", src)

  expect_true(
    webdav_upload_file(base_url, src, server_path = "docs",
                       username = "u", password = "p")
  )
  expect_identical(captured$method, "PUT")
  # The body must reference the file on disk, not an in-memory raw vector
  expect_identical(captured$body$type, "file")
})
