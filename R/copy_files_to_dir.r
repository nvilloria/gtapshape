copy_files_to_dir<- function(dest_folder = "gempack", extension = "tab") {
  # Get the path to the extdata directory
  extdata_path <- system.file("tablo", package = "gtapshape")

  # Get all files with the specified extension
  files <- list.files(extdata_path, pattern = paste0("\\.", extension, "$"), full.names = FALSE)

  # Create the destination folder if it doesn't exist
  dest_path <- file.path(getwd(), dest_folder)
  dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)

  # Copy each file to the destination folder
  copied_files <- vapply(files, function(file) {
    from <- file.path(extdata_path, file)
    to <- file.path(dest_path, file)
    file.copy(from, to, overwrite = TRUE)
  }, logical(1))

  # Get the full paths of successfully copied files
  successful_copies <- file.path(dest_path, files[copied_files])

  message("Copied ", sum(copied_files), " out of ", length(files), " ", extension, " files to ", dest_path)
  invisible(successful_copies)
}
