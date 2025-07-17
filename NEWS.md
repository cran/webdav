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
