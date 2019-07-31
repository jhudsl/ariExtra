#' Make an ari documnt
#'
#' @param images a vector of paths to images.
#' @param script a file or vector strings that will be spoken
#' @param output a path to the Rmd file which will be created.
#' @param open should the Rmd be opened after creating?
#' @param ... additional arguments to pass to [ari::ari_spin]
#' @param verbose print diagnostic messages and also passed to
#' [ari::ari_stitch]
#' @importFrom tools file_ext file_path_sans_ext
#'
#' @return A path to the document
#' @export
#' @importFrom ari ari_spin
#' @importFrom yaml as.yaml
#' @importFrom utils file.edit
#'
#' @examples
#' images = system.file("extdata", c("example_1.png", "example_2.png"),
#' package = "ariExtra")
make_ari_document = function(
  images, script,
  output = NULL,
  open = interactive(),
  ...,
  verbose = TRUE) {

  if (is.null(output)) {
    output = tempfile(fileext = ".Rmd")
    if (verbose > 1) {
      message(paste0("output is: ", output))
    }
  }
  stopifnot(length(output) == 1)
  ext = tools::file_ext(output)
  stopifnot(tolower(ext) %in% "rmd")
  stub = basename(tools::file_path_sans_ext(output))

  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  if (length(script) == 1 &
      is.character(script) &
      file.exists(script)) {
    script = readLines(script, warn = FALSE)
    script = trimws(script)
    script = script[ !script %in% ""]
  }

  stopifnot(
    length(script) > 0,
    identical(length(images), length(script)),
    all(file.exists(images))
  )

  if (verbose) {
    message("Making output directories")
  }
  output_dir <- normalizePath(dirname(output))
  if (verbose > 1) {
    message(paste0("output_dir is at: ", output_dir))
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

  files_dir = file.path(output_dir, paste0(stub, "_files"))
  dir.create(files_dir, recursive = TRUE, showWarnings = FALSE)
  if (verbose > 1) {
    message(paste0("files_dir is at: ", files_dir))
  }
  images = normalizePath(images)
  ext = tools::file_ext(images)
  new_names = paste0("slide_", seq_along(images), ext)
  new_names = file.path(files_dir, new_names)
  file.copy(images, new_names, overwrite = TRUE)

  script = paste0("<!--", script, "-->")
  images = paste0("![](", images, ")")

  rmd = paste(script, images, sep = "\n")
  args = list(..., verbose = verbose)
  L = list(
    output = list(
      ari_video = args)
  )
  if (verbose > 1) {
    message("Creating YAML header")
  }
  yml = yaml::as.yaml(L)
  yml = paste0("---\n", yml, "---\n")
  rmd = c(yml, rmd)
  rmd = paste(rmd, collapse = "\n")
  writeLines(rmd, output)
  if (open) {
    utils::file.edit(output)
  }

  return(output)
}
