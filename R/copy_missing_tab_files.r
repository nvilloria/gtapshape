copy_missing_tab_files <- function(dest_folder="gempack", missing_exe_files) {
  # Get the path to the extdata directory
  extdata_path <- system.file("tablo", package = "gtapshape")

  # Create the destination folder if it doesn't exist
  dest_path <- file.path(getwd(), dest_folder)
  dir.create(dest_path, showWarnings = FALSE, recursive = TRUE)

  # Convert .exe filenames to .tab filenames
  missing_tab_files <- gsub("\\.exe$", ".tab", missing_exe_files)

  # Copy each missing .tab file to the destination folder
  copied_files <- vapply(missing_tab_files, function(file) {
    from <- file.path(extdata_path, file)
    to <- file.path(dest_path, file)
    if (file.exists(from)) {
      file.copy(from, to, overwrite = TRUE)
    } else {
      FALSE
    }
  }, logical(1))

  # Get the full paths of successfully copied files
  successful_copies <- file.path(dest_path, missing_tab_files[copied_files])

  message("Copied ", sum(copied_files), " out of ", length(missing_tab_files), " missing .tab files to ", dest_path)

  # Suggest command to produce .exe files
  if (sum(copied_files) > 0) {
    message("\nTo produce .exe files, run the following command for each .tab file:")
    message('system("tablo -wfp filename.tab")')
  }

  invisible(successful_copies)
}
