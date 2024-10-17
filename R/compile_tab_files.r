compile_tab_files <- function(dest_folder="gempack") {
  # Store the current working directory
  original_dir <- getwd()

  # Change to the dest_folder
  setwd(file.path(original_dir, dest_folder))

  # Get a list of all .tab files in the dest_folder
  tab_files <- list.files(pattern = "\\.tab$")

  # Apply the system command to each .tab file
  for (file in tab_files) {
      system(paste("tablo -wfp", file))
      # Strip the .tab extension from the file name
    file_without_extension <- tools::file_path_sans_ext(file)
      system(
          paste0("cmd /c call ltg ",file_without_extension)
      )
  }
  # Return to the original directory
  setwd(original_dir)
}
