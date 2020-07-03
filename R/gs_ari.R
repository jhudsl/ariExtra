download_gs_file = function(id, out_type = "pptx") {
  id = as.character(id)
  id = get_slide_id(id)
  url = type_url(id = id, page_id = NULL, type = out_type)

  tfile = tempfile(fileext = paste0(".", out_type))
  result = httr::GET(url, httr::write_disk(tfile))
  warn_them = FALSE
  fr_header = result$headers$`x-frame-options`
  if (is.null(fr_header)) {
    if (all(fr_header == "DENY")) {
      warn_them = TRUE
    }
  }
  if (httr::status_code(result) >= 300) {
    warn_them = TRUE
  }
  if (grepl("ServiceLogin", result$url)) {
    warn_them = TRUE
  }
  if (result$times["redirect"] > 0) {
    warn_them = TRUE
  }
  if (warn_them) {
    warning(
      paste0(
        "This presentation may not be available, ",
        "did you turn link sharing on?")
    )
  }
  tfile
}

get_pptx_script = function(path, script = NULL, verbose = TRUE) {
  if (is.null(script)) {
    if (verbose) {
      message("Getting Notes from PPTX")
    }
    res = pptx_notes(path)
    script = tempfile(fileext = ".txt")
    if (verbose > 1) {
      message(paste0("script is at: ", script))
    }
    writeLines(res, script)
  }
  return(script)
}

#' Convert Google Slides and notes to video with ari
#'
#' @param path Identifier of google slides presentation, or PPTX filename
#' @param ... Arguments passed to [make_ari_document]
#' @param verbose print diagnostic messages
#'
#' @return The output from [make_ari_document]
#' @importFrom httr GET write_disk
#' @export
#' @examples
#' \donttest{
#' # takes > 5 seconds to run
#'   id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
#'   res = gs_to_ari(id, verbose = 2, open = FALSE)
#'   if (interactive()) {
#'     file.edit(res$output_file)
#'   }
#'   # replicates same thing as above without verbosity
#'   res2 = to_ari(id, open = FALSE)
#' }
#'
gs_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE) {

  if (verbose) {
    message("Downloading PPTX")
  }
  pptx_file = download_gs_file(id = path, out_type = "pptx")
  if (verbose > 1) {
    message(paste0("pptx is at: ", pptx_file))
  }

  script = get_pptx_script(
    path = pptx_file,
    script = script, verbose = verbose)

  if (verbose) {
    message("Downloading PDF")
  }
  pdf_file = download_gs_file(id = path, out_type = "pdf")
  if (verbose > 1) {
    message(paste0("PDF is at: ", pdf_file))
  }

  # should we download PDF too?
  pdf_to_ari(pdf_file, script = script, ..., verbose = verbose)
  # pptx_to_ari(pptx_file, script = script, ..., verbose = verbose)
}


#' @rdname gs_to_ari
#' @export
pptx_to_pdf = function(path, verbose = TRUE) {
  pdf_file = tempfile(fileext = ".pdf")
  if (verbose) {
    message("Converting PPTX to PDF")
  }
  out = try({
    docxtractr::convert_to_pdf(path, pdf_file = pdf_file)
  })
  if (inherits(out, "try-error")) {
    fix_soffice_library_path()
    docxtractr::convert_to_pdf(path, pdf_file = pdf_file)
  }
  if (verbose > 1) {
    message(paste0("PDF is at: ", pdf_file))
  }
  return(pdf_file)
}

#' @rdname gs_to_ari
#' @export
pptx_to_pngs = function(path, verbose = TRUE, dpi = 600) {
  pdf_file = pptx_to_pdf(path = path, verbose = verbose)
  fmts = pdftools::poppler_config()$supported_image_formats
  if (!"png" %in% fmts) {
    stop(paste0("PNG is not in ",
                "pdftools::poppler_config()$supported_image_formats",
                ", see pdftools::pdf_convert for options")
    )
  }
  pdf_to_pngs(
    path = pdf_file,
    verbose = verbose,
    dpi = dpi)
}

#' @export
#' @rdname gs_to_ari
#' @param script passed to [make_ari_document()]
#' @importFrom docxtractr convert_to_pdf
#' @examples
#' ex_file = system.file("extdata", "example.pptx", package = "ariExtra")
#' have_soffice = try(docxtractr:::lo_assert())
#' if (!inherits(have_soffice, "try-error")) {
#'   pngs = try({
#'     pptx_to_pngs(ex_file)
#'   }, silent = TRUE)
#'
#'   soffice_config_issue = inherits(pngs, "try-error")
#'   if (soffice_config_issue) {
#'     warning(
#'       paste0("soffice does not seem configured properly, may need to ",
#'              "adapt LD_LIBRARY_PATH, ",
#'              "try ariExtra:::fix_soffice_library_path()")
#'     )
#'     # this can be due to a library issue:
#'     url = paste0("https://codeyarns.github.io/tech/2019-09-05",
#'                  "-libregloso-cannot-open-shared-object-file.html")
#'     if (interactive()) {
#'       utils::browseURL(url)
#'     }
#'   }
#'   if (!soffice_config_issue) {
#'     res = pptx_to_ari(ex_file, open = FALSE)
#'     if (interactive()) {
#'       file.edit(res$output_file)
#'     }
#'     res2 = to_ari(ex_file, open = FALSE)
#'   }
#' }
pptx_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE) {

  script = get_pptx_script(path, script = NULL, verbose = verbose)
  pdf_file = pptx_to_pdf(path, verbose = verbose)
  pdf_to_ari(pdf_file, script = script, ..., verbose = verbose)
}





#' @param dpi resolution (dots per inch) to render images
#' @rdname gs_to_ari
#' @importFrom pdftools poppler_config pdf_info pdf_convert
#' @export
#' @examples
#' ex_file = system.file("extdata", "example.pdf", package = "ariExtra")
#' res = pdf_to_ari(ex_file, script = c("hey", "ho"), open = FALSE)
#' if (interactive()) {
#' file.edit(res$output_file)
#' }
#' \donttest{
#' res2 = to_ari(ex_file,  script = c("hey", "ho"), open = FALSE)
#' }
pdf_to_ari = function(
  path,
  script = NULL,
  dpi = 300,
  ...,
  verbose = TRUE){
  stopifnot(!is.null(script))
  pngs = pdf_to_pngs(path = path, dpi = dpi, verbose = verbose)
  make_ari_document(pngs, script = script, ..., verbose = verbose)
}

#' @rdname gs_to_ari
#' @export
html_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE
) {

  if (!requireNamespace("pagedown", quietly = TRUE)) {
    stop(
      paste0("pagedown pacakge needed to use chrome_print",
             " for html_to_ari")
    )
  }
  pdf_file = tempfile(fileext = ".pdf")
  args$input = path
  args$output = pdf_file
  args$verbose = as.numeric(verbose)
  args$format = "pdf"
  pdf_file = do.call(pagedown::chrome_print, args = args)
  n_slides_guess = pdftools::pdf_info(pdf_file)$pages

  pdf_to_ari(
    path = pdf_file,
    script = script,
    ..., verbose = verbose)
}


#' @rdname gs_to_ari
#' @export
pdf_to_pngs = function(
  path, verbose = TRUE,
  dpi = 600) {
  fmts = pdftools::poppler_config()$supported_image_formats
  if ("png" %in% fmts) {
    format = "png"
  } else {
    format = fmts[1]
  }
  info = pdftools::pdf_info(pdf = path)
  filenames = vapply(seq.int(info$pages), function(x) {
    tempfile(fileext = paste0(".", format))
  }, FUN.VALUE = character(1))
  if (verbose) {
    message("Converting PDF to PNGs")
  }
  pngs = pdftools::pdf_convert(
    pdf = path, dpi = dpi,
    format = format, filenames = filenames,
    verbose = as.logical(verbose))
  pngs
}

#' @rdname gs_to_ari
#' @export
images_to_ari = function(
  path,
  script = NULL,
  dpi = 300,
  ...,
  verbose = TRUE){
  make_ari_document(path, script = script, ..., verbose = verbose)
}


guess_ari_function = function(path) {
  to_ari_function = NULL
  if (length(path) > 1) {
    to_ari_function = "images_to_ari"
  } else {
    # bn = tolower(basename(path))
    is_file = file.exists(path)
    is_url = grepl("^(http|www|google)", tolower(path))

    # can you allow for "*.mat" - shouldn't be gs
    if (is_url ||
        (!is_file && nchar(path) < 50) && !grepl("[*]", path)) {
      to_ari_function = "gs_to_ari"
    } else {
      btype = mime::guess_type(path)
      btype = tolower(basename(btype))
      if (btype %in% "pdf") {
        to_ari_function = "pdf_to_ari"
      }
      if (btype %in% "html") {
        to_ari_function = "html_to_ari"
      }
      if (btype %in% c("x-markdown", "markdown")) {
        to_ari_function = "rmd_to_ari"
      }
      # video stuff
      if (btype %in% c("mp4", "x-msvideo", "x-matroska",
                       "mpeg", "quicktime")) {
        to_ari_function = "images_to_ari"
      }
      if (any(grepl("officedocument.presentation", btype))) {
        to_ari_function = "pptx_to_ari"
      }
    }
  }
  if (is.null(to_ari_function)) {
    stop(
      paste0("ari function cannot be guess",
             " by input path object",
             ", please use function directly")
    )
  }
  to_ari_function
}

#' @rdname gs_to_ari
#' @export
to_ari = function(path,
                  script = NULL,
                  ...,
                  verbose = TRUE) {

  to_ari_function = guess_ari_function(path)
  args = list(path = path,
              script = script,
              ...,
              verbose = verbose)
  do.call(to_ari_function, args = args)
}


fix_soffice_library_path = function() {
  LD_LIBRARY_PATH = Sys.getenv("LD_LIBRARY_PATH")
  if (sys_type() %in% c("linux", "macos")) {
    warning(
      paste0(
        "Changing LD_LIBRARY_PATH as error in soffice ",
        "with PPTX conversion may be due to path issues!"
      )
    )
    Sys.setenv(
      LD_LIBRARY_PATH =
        paste0(
          "/usr/lib/libreoffice/program",
          if (nzchar(LD_LIBRARY_PATH)) paste0(":", LD_LIBRARY_PATH)
        )
    )
  }
}
