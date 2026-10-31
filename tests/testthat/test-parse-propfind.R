# Offline tests for the PROPFIND response parser used by webdav_list_files().

owncloud_response <- function(folder = "docs") {
  glue::glue('<?xml version="1.0"?>
<d:multistatus xmlns:d="DAV:" xmlns:oc="http://owncloud.org/ns">
 <d:response>
  <d:href>/remote.php/dav/files/user/{folder}/</d:href>
  <d:propstat><d:prop>
    <d:getlastmodified>Tue, 28 Oct 2026 10:00:00 GMT</d:getlastmodified>
    <d:resourcetype><d:collection/></d:resourcetype>
  </d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>
 </d:response>
 <d:response>
  <d:href>/remote.php/dav/files/user/{folder}/sub%20folder/</d:href>
  <d:propstat><d:prop>
    <d:getlastmodified>Tue, 28 Oct 2026 09:00:00 GMT</d:getlastmodified>
    <d:resourcetype><d:collection/></d:resourcetype>
  </d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>
 </d:response>
 <d:response>
  <d:href>/remote.php/dav/files/user/{folder}/report.pdf</d:href>
  <d:propstat><d:prop>
    <d:getlastmodified>Mon, 27 Oct 2026 09:00:00 GMT</d:getlastmodified>
    <d:getcontentlength>12345</d:getcontentlength>
    <d:resourcetype/>
  </d:prop><d:status>HTTP/1.1 200 OK</d:status></d:propstat>
 </d:response>
</d:multistatus>')
}

test_that("parses an ownCloud-style response (no isFolder, missing props)", {
  res <- webdav:::parse_propfind_response(owncloud_response())

  expect_s3_class(res, "tbl_df")
  # The folder itself is dropped
  expect_equal(nrow(res), 2)
  expect_equal(res$display_name, c("sub folder/", "report.pdf"))
  expect_equal(res$is_folder, c(TRUE, FALSE))
  expect_equal(res$content_length, c(NA_real_, 12345))
  # creationdate is absent in the response: must come back NA, not fail
  expect_true(all(is.na(res$creation_date)))
})

test_that("folder names with regex metacharacters don't break display_name", {
  res <- webdav:::parse_propfind_response(owncloud_response(folder = "pasta (1)"))

  expect_equal(res$display_name, c("sub folder/", "report.pdf"))
})

test_that("recognizes folders via the IIS/SharePoint isFolder property", {
  content <- '<?xml version="1.0"?>
<D:multistatus xmlns:D="DAV:">
 <D:response>
  <D:href>/Sales/</D:href>
  <D:propstat><D:prop>
    <D:isFolder>t</D:isFolder>
    <D:resourcetype><D:collection/></D:resourcetype>
  </D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat>
 </D:response>
 <D:response>
  <D:href>/Sales/Q1/</D:href>
  <D:propstat><D:prop>
    <D:isFolder>1</D:isFolder>
  </D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat>
 </D:response>
 <D:response>
  <D:href>/Sales/plan.xlsx</D:href>
  <D:propstat><D:prop>
    <D:isFolder>f</D:isFolder>
    <D:getcontentlength>10</D:getcontentlength>
  </D:prop><D:status>HTTP/1.1 200 OK</D:status></D:propstat>
 </D:response>
</D:multistatus>'

  res <- webdav:::parse_propfind_response(content)

  expect_equal(res$display_name, c("Q1/", "plan.xlsx"))
  expect_equal(res$is_folder, c(TRUE, FALSE))
})

test_that("`%||%` is available regardless of the R version", {
  # Base R only ships %||% from 4.4.0; the package must carry its own.
  expect_true(is.function(webdav:::`%||%`))
  expect_identical(webdav:::`%||%`(NULL, "fallback"), "fallback")
  expect_identical(webdav:::`%||%`("value", "fallback"), "value")
})
