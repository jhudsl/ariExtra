#' Convert Google Slides and notes to video with ari
#'
#' @param path Identifier of google slides presentation, or PPTX filename
#' @param ... Arguments passed to [make_ari_document]
#'
#' @return The output from [make_ari_document]
#' @importFrom httr GET write_disk
#' @export
gs_to_ari = function(
  path,
  ...) {
  pptx = pptx_url(path)
  download_pptx = function(url) {
    tfile = tempfile(fileext = ".pptx")
    httr::GET(url, httr::write_disk(tfile))
    tfile
  }
  res = download_pptx(url = pptx)
  pptx_to_ari(res, ...)
}


#' @export
#' @rdname gs_to_ari
#' @param script passed to [make_ari_document()]
#' @importFrom docxtractr convert_to_pdf
pptx_to_ari = function(path, script = NULL, ...) {
  if (is.null(script)) {
    res = pptx_notes(path)
    script = tempfile(fileext = ".txt")
    writeLines(res, script)
  }
  pdf_file = tempfile(fileext = ".pdf")
  docxtractr::convert_to_pdf(path, pdf_file = pdf_file)
  pdf_to_ari(pdf_file, script = script, ...)
}


#' @param dpi resolution (dots per inch) to render images
#' @rdname gs_to_ari
#' @importFrom pdftools poppler_config pdf_info pdf_convert
#' @export
pdf_to_ari = function(
  path,
  dpi = 300,
  ...){
  stopifnot(!is.null(script))
  fmts = pdftools::poppler_config()$supported_image_formats
  if ("png" %in% fmts) {
    format = "png"
  } else {
    format = fmts[1]
  }
  info = pdftools::pdf_info(pdf = path)
  filenames = vapply(info$pages, function(x) {
    tempfile(fileext = format)
  })
  pngs = pdftools::pdf_convert(
    pdf = pdf, dpi = dpi,
    format = format, filenames = filenames)
  make_ari_document(pngs, ...)
}
