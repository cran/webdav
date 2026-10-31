
# WebDAV

<!-- (https://cloud.r-project.org/package=webdav) -->

<!-- badges: start -->

![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/webdav)  
![CRAN Downloads](https://cranlogs.r-pkg.org/badges/grand-total/webdav)
  ![License](https://img.shields.io/badge/license-MIT-darkviolet.svg) 
![Devel
Badge](https://img.shields.io/badge/devel%20version-0.3.0-blue.svg)
<!-- badges: end -->

<!-- badger::badge_devel("StrategicProjects/webdav", "blue") -->

WebDAV (*Web Distributed Authoring and Versioning*) is an extension of
the HTTP protocol that allows users to collaboratively edit and manage
files stored on remote web servers. It provides a framework for users to
create, delete, copy, move, and download files.

### Examples of WebDAV Servers

Some platforms that implement WebDAV are:

1.  **OwnCloud**: A popular open-source platform for storing and sharing
    files, commonly used by organizations.
2.  **NextCloud**: A fork of OwnCloud that focuses on user-friendly
    interfaces and collaborative file-sharing features.
3.  **mod_dav** for Apache HTTP servers: Adds WebDAV support to the
    Apache web server.
4.  **Microsoft SharePoint**: Uses WebDAV for file management over the
    web.

### Installation

Install the released version from CRAN:

``` r
install.packages("webdav")
```

Or the development version from GitHub:

``` r
# install.packages("remotes")
remotes::install_github("StrategicProjects/webdav")
```

### Functions

| Function | WebDAV method | Description |
|----|----|----|
| `webdav_create_request()` | — | Build an authenticated `httr2` request for the server |
| `webdav_list_files()` | PROPFIND | List files and folders as a tibble |
| `webdav_upload_file()` | PUT | Upload a local file (streamed from disk) |
| `webdav_download_file()` | GET | Download a file (streamed to disk) |
| `webdav_create_directory()` | MKCOL | Create a collection (directory) |
| `webdav_copy_file()` | COPY | Copy a resource |
| `webdav_move_file()` | MOVE | Move or rename a resource |
| `webdav_delete_resource()` | DELETE | Delete a file or directory |

### Quick start

Credentials default to the `WEBDAV_USERNAME` and `WEBDAV_PASSWORD`
environment variables (see `vignette("env_variables")`):

``` r
library(webdav)

base_url <- "https://cloud.example.com/remote.php/dav/files/user"

# List what's there
webdav_list_files(base_url, folder_path = "Documents")

# Upload, rename, download, clean up
webdav_upload_file(base_url, local_path = "report.pdf", server_path = "Documents")
webdav_move_file(base_url,
                 from_path = "Documents/report.pdf",
                 to_path = "Documents/report-2026.pdf")
webdav_download_file(base_url,
                     file_path = "Documents/report-2026.pdf",
                     destination_path = tempdir())
webdav_delete_resource(base_url, resource_path = "Documents/report-2026.pdf")
```

Invalid arguments raise an error; network or server failures issue a
warning and return `FALSE` (`NULL` for `webdav_list_files()`), so
scripts can test the return value instead of wrapping calls in
`tryCatch()`.
