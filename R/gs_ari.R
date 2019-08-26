download_gs_file = function(id, out_type = "pptx") {
  id = as.character(id)
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
#' id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
#' res = gs_to_ari(id, verbose = 2, open = FALSE)
#' if (interactive()) {
#' file.edit(res$output_file)
#' }
#' }
#'
#'
#' bad_id = "1AJxokPkGVzNZMXjyF59mNe3EiQRGxylbBfYGGoQMQls"
#' testthat::expect_error(res = gs_to_ari(bad_id))
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


#' @export
#' @rdname gs_to_ari
#' @param script passed to [make_ari_document()]
#' @importFrom docxtractr convert_to_pdf
#' @examples
#' ex_file = system.file("extdata", "example.pptx", package = "ariExtra")
#' have_soffice = try(docxtractr:::lo_assert())
#' if (!inherits(have_soffice, "try-error")) {
#' res = pptx_to_ari(ex_file, open = FALSE)
#' if (interactive()) {
#' file.edit(res$output_file)
#' }
#' }
pptx_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE) {

  script = get_pptx_script(path, script = NULL, verbose = verbose)
  pdf_file = tempfile(fileext = ".pdf")
  if (verbose) {
    message("Converting PPTX to PDF")
  }
  docxtractr::convert_to_pdf(path, pdf_file = pdf_file)
  if (verbose > 1) {
    message(paste0("PDF is at: ", pdf_file))
  }
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
#' file.edit(res)
#' }
pdf_to_ari = function(
  path,
  script = NULL,
  dpi = 300,
  ...,
  verbose = TRUE){
  stopifnot(!is.null(script))
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
  make_ari_document(pngs, script = script, ..., verbose = verbose)
}
