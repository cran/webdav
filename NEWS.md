# webdav 0.3.0

## New Features

- New `webdav_move_file()` moves or renames a file or directory on the
  server using the WebDAV MOVE method, with an `overwrite` argument.
- `webdav_copy_file()` gained an `overwrite` argument (default `TRUE`,
  matching the WebDAV protocol default).

## Breaking Changes

- Error handling is now consistent across all functions: invalid arguments
  raise an error, while network or server failures issue a warning and
  return `FALSE` (`NULL` for `webdav_list_files()`). Previously,
  `webdav_copy_file()`, `webdav_download_file()` and
  `webdav_create_directory()` aborted on server failures.
- HTTP failures now produce precise messages with the status code and its
  description (e.g. "423 (Locked)") instead of a generic transport error.

## Bug Fixes

- `webdav_list_files()` no longer fails on R < 4.4.0: the `%||%` operator it
  relied on only exists in base R from 4.4.0 onward, and the package now
  carries its own definition.
- `webdav_list_files()` now returns correct `display_name` values when the
  listed folder's name contains regex metacharacters (e.g. `"pasta (1)"`);
  the folder prefix is removed as a fixed string instead of a regex.
- `webdav_copy_file()` now percent-encodes the `Destination` header, so
  copying to paths with spaces or non-ASCII characters works. The request
  URL was already encoded; the header was being sent raw.
- `webdav_upload_file()` now prints "root" (instead of an empty path) in the
  success message when uploading to the server root.

## Enhancements

- `webdav_download_file()` streams the response straight to disk (via a
  temporary file, so a failed request never leaves a partial file behind)
  instead of loading the whole body into memory, and validates that the
  destination directory exists.
- `webdav_upload_file()` streams the file from disk with
  `httr2::req_body_file()` instead of reading it entirely into memory, and
  uses `httr2::req_timeout()` for the timeout.
- The PROPFIND response parser was extracted into an internal function and
  the package is now covered by offline unit tests (`tests/testthat/`),
  including ownCloud-style and IIS-style (`isFolder`) fixtures and
  mocked-request tests for every HTTP verb.
- Removed `LazyData` from DESCRIPTION (the package has no data directory).

# webdav 0.2.0

## Breaking Changes

- `check_and_load_package()` is now deprecated. All required packages are
  declared in `DESCRIPTION` and loaded automatically by R. Calling this
  function will issue a deprecation warning.

## Enhancements

- Migrated all user-facing messages from `message()` / `stop()` to the `cli`
  package (`cli::cli_inform()`, `cli::cli_alert_success()`, `cli::cli_warn()`,
  `cli::cli_abort()`), providing richer formatted output with semantic markup
  for URLs, file paths, and argument names.
- Added `cli` to `Imports` in DESCRIPTION.
- Added `httr2::resp_status()` import to NAMESPACE.
- Fixed `handle_response()` to use `httr2::resp_status()` instead of the
  legacy `response$status_code` accessor (httr v1 syntax).
- Removed redundant runtime calls to `check_and_load_package()` inside all
  exported functions — dependencies declared in `Imports` are guaranteed to be
  available and do not need manual loading.

## Authors

- Added Marcos Wasiliew, Carlos Amorim as authors.
- Fixed Hugo Vasconcelos surname spelling.

# webdav 0.1.6

## Enhancements

- Major improvements to the `webdav_list_files()` function for better compatibility and robustness:
  - Now dynamically detects and uses the correct XML namespace prefix from the server (`d`, `D`, or other), preventing XPath errors like `Undefined namespace prefix`.
  - Uses `xml2::as_list()` to reliably parse the XML response structure.
  - Ensures `content_length` is safely parsed and padded with `NA` where missing (e.g., for directories), preventing errors when creating the resulting `tibble`.
  - Adds a new field `is_folder`, correctly identifying whether each resource is a directory.
  - Improved documentation and output consistency with more informative column names (e.g., `display_name`, `full_path`, `creation_date`, etc.).

## Acknowledgements

Special thanks to **Adrian Jusepeitis** (University of Jena, Germany) for reporting two key issues related to XML namespace parsing and missing metadata in `webdav_list_files()`, and for proposing initial fixes that inspired the improvements in this release.


# webdav 0.1.4

*	Changed the license to MIT
*	Adjusted the time limits in the examples
* Added an internet connection test to ensure network availability

# webdav 0.1.3

* Expose lastmodified and contentlength via webdav_list_files. Idea from Benjamin Buchwitz (bchwtz) issue: https://github.com/StrategicProjects/webdav/issues/1 

# webdav 0.1.2

* [New Feature] Added a new function `webdav_download_file()` to download files from a WebDAV server to a local directory.
  - This function allows users to specify a file on the server and download it to a specified local path.
  - Verbose mode (`verbose = TRUE`) provides detailed output of the download process, including the file path on the server and local destination.
  
* Added examples using demo WebDav server.  

# webdav 0.1.1

* Added a `NEWS.md` file to track changes to the package.
