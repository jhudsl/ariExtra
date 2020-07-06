#' Make an ari document
#'
#' @param images a vector of paths to images.
#' @param script a file or vector strings that will be spoken
#' @param output_file a path to the Rmd file which will be created.
#' @param open should the Rmd be opened after creating?
#' @param ... additional arguments to pass to [ari::ari_spin]
#' @param verbose print diagnostic messages and also passed to
#' [ari::ari_stitch]
#' @param use_knitr use an Rmarkdown type syntax for including the images
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
#' res = make_ari_document(images, script = c("hello", "how are you"))
#' res$output_file
#' res = make_ari_document(images, script = c("hello", "how are you"),
#' use_knitr = TRUE)
#' res$output_file
#'
#' res = to_ari(images, script = c("hello", "how are you"), open = FALSE)
make_ari_document = function(
  images, script,
  output_file = NULL,
  open = interactive(),
  use_knitr = FALSE,
  ...,
  verbose = TRUE) {

  if (is.null(output_file)) {
    fext = ".md"
    if (use_knitr) {
      fext = ".Rmd"
    }
    output_file = tempfile(fileext = fext)
    if (verbose > 1) {
      message(paste0("output_file is: ", output_file))
    }
  }
  stopifnot(length(output_file) == 1)
  ext = tools::file_ext(output_file)
  if (use_knitr) {
    stopifnot(tolower(ext) %in% c("md", "rmd"))
  } else {
    stopifnot(tolower(ext) %in% "md")
  }
  stub = basename(tools::file_path_sans_ext(output_file))

  stopifnot(length(images) > 0)
  images <- normalizePath(images)
  if (length(script) == 1) {
    if (is.character(script) & file.exists(script)) {
      script = readLines(script, warn = FALSE)
      script = trimws(script)
      script = script[ !script %in% ""]
    }
  }

  semi_colon = trimws(script) == ";"
  if (any(semi_colon)) {
    script[semi_colon] = ""
  }

  stopifnot(
    length(script) > 0,
    identical(length(images), length(script)),
    all(file.exists(images))
  )

  if (verbose) {
    message("Making output_file directories")
  }
  output_dir <- normalizePath(dirname(output_file))
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
  new_names = sprintf("slide_%05.0f.%s", seq_along(images), ext)
  new_names = file.path(files_dir, new_names)
  file.copy(images, new_names, overwrite = TRUE)

  ximages = images
  xscript = script
  # for ioslides
  seps = rep("\n----------\n", length(images))
  if (use_knitr) {
    script = paste0('```{speak speak_', seq_along(script),
                    ', echo = FALSE, results = "hide", cache = TRUE}\n',
                    script,
                    "\n```\n")
  } else {
    script = paste0("<!--", script, "-->")
  }
  # consider ```{r slide\#1, echo = FALSE, out.width="100%"}
  # paste0('knitr::include_graphics("', images, '")')
  # ```\n
  knitr_images = paste0('```{r slide_', seq_along(images),
                        ', echo = FALSE, out.width="100%"}\n',
                        paste0('knitr::include_graphics("', images, '")\n'),
                        "```\n")
  images = paste0("![](", images, ")\n")
  if (use_knitr) {
    images = knitr_images
  }
  rmd = paste(seps, script, images, sep = "\n")
  args = list(..., verbose = verbose)
  L = list(
    output = list(
      "ariExtra::ari_document" = args)
  )
  if (verbose > 1) {
    message("Creating YAML header")
  }
  yml = yaml::as.yaml(L)
  yml = paste0("---\n", yml, "---\n")
  if (use_knitr) {
    speak = text2speech::tts_speak_engine
    add_speak = paste0(
      '```{r speak_setup, echo = FALSE, results = "hide"}\n',
      'knitr::knit_engines$set(speak = text2speech::tts_speak_engine)',
      "\n```\n\n")
    yml = c(yml, add_speak)
  }
  rmd = c(yml, rmd)
  rmd = paste(rmd, collapse = "\n")
  writeLines(rmd, output_file)

  if (open) {
    if (requireNamespace("rstudioapi", quietly = TRUE)) {
      if (rstudioapi::isAvailable() && rstudioapi::hasFun("navigateToFile")) {
        rstudioapi::navigateToFile(output_file)
      } else {
        utils::file.edit(output_file)
      }
    } else {
      utils::file.edit(output_file)

    }
  }

  L = list(output_file = output_file,
           original_images = ximages,
           images = new_names,
           script = xscript,
           use_knitr = use_knitr)
  return(L)
}

#' @export
#' @rdname make_ari_document
pngs_to_ari = make_ari_document
