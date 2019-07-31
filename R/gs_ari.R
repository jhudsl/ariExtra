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
#' id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
#' res = gs_to_ari(id, verbose = 2)
#'
#'
gs_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE) {
  pptx = pptx_url(path)
  download_pptx = function(url) {
    tfile = tempfile(fileext = ".pptx")
    httr::GET(url, httr::write_disk(tfile))
    tfile
  }
  if (verbose) {
    message("Downloading PPTX")
  }
  res = download_pptx(url = pptx)
  if (verbose > 1) {
    message(paste0("pptx is at: ", res))
  }
  pptx_to_ari(res, script = script, ..., verbose = verbose)
}


#' @export
#' @rdname gs_to_ari
#' @param script passed to [make_ari_document()]
#' @importFrom docxtractr convert_to_pdf
pptx_to_ari = function(
  path,
  script = NULL,
  ...,
  verbose = TRUE) {
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
    format = format, filenames = filenames)
  make_ari_document(pngs, script = script, ..., verbose = verbose)
}
