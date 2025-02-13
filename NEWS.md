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
