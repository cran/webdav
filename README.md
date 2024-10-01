
# Using the WebDAV R Package for File Management

## Introduction to WebDAV

WebDAV (Web Distributed Authoring and Versioning) is an extension of the HTTP protocol that allows users to collaboratively edit and manage files stored on remote web servers. It provides a framework for users to create, delete, copy, move, and lock files and directories over the web.

### Examples of WebDAV Servers

Some platforms that implement WebDAV are:
- **OwnCloud**: A popular open-source platform for storing and sharing files, commonly used by organizations.
- **NextCloud**: A fork of OwnCloud that focuses on user-friendly interfaces and collaborative file-sharing features.
- **mod_dav** for Apache HTTP servers: Adds WebDAV support to the Apache web server.
- **Microsoft SharePoint**: Uses WebDAV for file management over the web.

This README introduces the **`webdav`** R package, which provides an interface for managing files on WebDAV-compliant servers, enabling file operations such as uploading, downloading, copying, moving, and deleting resources.

## Function Overview

### 1. Creating a WebDAV Request

```r
webdav_create_request(base_url, path, dav, username, password, verbose = FALSE)
```

This function creates a basic authenticated WebDAV request for the given resource path on a WebDAV server. It sets up authentication using the provided username and password and returns a request object.

**Parameters**:
- `base_url`: Base URL of the WebDAV server (e.g., `https://drive.expresso.pe.gov.br`).
- `path`: The relative path to the resource on the server.
- `dav`: The WebDAV base path (specific to the server's configuration).
- `username`: Username for authentication.
- `password`: Password for authentication.
- `verbose`: Whether to print debug information (default is `FALSE`).

**Example**:

```r
webdav_create_request("https://drive.expresso.pe.gov.br", "/Shared/der/app_painel/data/", "0c75a584-017d-103a-9c84-d34d2e44200b")
```

### 2. Copying Files

```r
webdav_copy_file(base_url, source_path, destination_path, dav, username, password)
```

This function copies a file or directory from one path to another on a WebDAV server.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `source_path`: The source file path to copy.
- `destination_path`: The destination path where the file will be copied.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.

**Example**:

```r
webdav_copy_file(
  "https://drive.expresso.pe.gov.br", 
  "/data/file.txt", 
  "/data/copied_file.txt", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

### 3. Creating a Directory

```r
webdav_create_directory(base_url, folder_path, dav, username, password)
```

This function creates a new directory (or collection) on the WebDAV server using the MKCOL method.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `folder_path`: Path to the directory to be created.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.

**Example**:

```r
webdav_create_directory(
  "https://drive.expresso.pe.gov.br", 
  "/data/new_folder/", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

### 4. Deleting a File or Directory

```r
webdav_delete_resource(
  base_url, 
  resource_path, 
  dav, 
  username, 
  password, 
  verbose = FALSE
  )
```

This function deletes a file or directory from the WebDAV server using the DELETE method.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `resource_path`: The path to the file or directory to delete.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.

**Example**:

```r
webdav_delete_resource(
  "https://drive.expresso.pe.gov.br", 
  "/data/file.txt", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

### 5. Listing Files in a Directory

```r
webdav_list_files(
  base_url, 
  folder_path, 
  dav, 
  username, 
  password, 
  depth = 1, 
  verbose = FALSE
  )
```

This function lists the files within a directory on the WebDAV server using the PROPFIND method.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `folder_path`: Path to the folder to list files.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.
- `depth`: Depth of the PROPFIND request (default is `1`).

**Example**:

```r
webdav_list_files(
  "https://drive.expresso.pe.gov.br", 
  "/data/", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

### 6. Uploading a File

```r
webdav_upload_file(
  base_url, 
  file_path, 
  upload_path, 
  dav, 
  username, 
  password, 
  timeout = 300
  )
```

This function uploads a local file to the WebDAV server.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `file_path`: Local path to the file.
- `upload_path`: Path on the WebDAV server where the file will be uploaded.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.
- `timeout`: Time limit for the upload operation (default is 300 seconds).

**Example**:

```r
webdav_upload_file(
  "https://drive.expresso.pe.gov.br", 
  "local_file.txt", 
  "/data/", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

### 7. Locking a Resource

```r
webdav_lock_resource(
  base_url, 
  resource_path, 
  dav, 
  lock_type = "exclusive", 
  username, 
  password
  )
```

This function locks a resource on the WebDAV server to prevent concurrent modifications using the LOCK method.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `resource_path`: Path to the resource to lock.
- `dav`: The WebDAV base path.
- `lock_type`: Type of lock ('exclusive' or 'shared').
- `username`: Username for authentication.
- `password`: Password for authentication.

**Example**:

```r
webdav_lock_resource(
  "https://drive.expresso.pe.gov.br", 
  "/data/file.txt", 
  "0c75a584-017d-103a-9c84-d34d2e44200b", 
  "exclusive"
  )
```

### 8. Checking for Lock Support

```r
check_lock_support_webdav(base_url, resource_path, dav, username, password)
```

This function checks if a resource on the WebDAV server supports the LOCK method.

**Parameters**:
- `base_url`: Base URL of the WebDAV server.
- `resource_path`: Path to the resource to check.
- `dav`: The WebDAV base path.
- `username`: Username for authentication.
- `password`: Password for authentication.

**Example**:

```r
check_lock_support_webdav(
  "https://drive.expresso.pe.gov.br", 
  "/data/file.txt", 
  "0c75a584-017d-103a-9c84-d34d2e44200b"
  )
```

## Conclusion

The **`webdav`** R package provides a interface for managing files and directories on WebDAV-enabled servers. With basic file management (uploading, downloading, deleting, copying), directory management, and resource locking, the package simplifies interactions with platforms like **OwnCloud**, **NextCloud**, and other WebDAV-compliant systems.
