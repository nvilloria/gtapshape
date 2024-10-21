#'@export
write_cmf <- function(cmf_filename,
                      input_files,
                      output_files,
                      out_dir,
                      workdir=getwd()){
 # Set default output directory if not provided
  if (is.null(out_dir)) {
      out_dir <- getwd()
  }

  # Ensure output directory exists
  dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

  # Create CMF file path
  cmffile.location <- file.path(out_dir, cmf_filename)

  # Prepare CMF content
  cmf_content <- c("!Input files:!")

  # Add input files
  for (name in names(input_files)) {
    cmf_content <- c(cmf_content,
                     paste0("FILE ", name, "=", input_files[[name]], ";"))
  }

      # Add separator for output files
  cmf_content <- c(cmf_content, "!Output files:!")

  # Add output files
  for (name in names(output_files)) {
    cmf_content <- c(cmf_content,
                     paste0("FILE ", name, "=", output_files[[name]], ";"))
  }

  # Write CMF file
  writeLines(cmf_content, cmffile.location)

  # Return the path of the created CMF file
  return(cmffile.location)
    }
