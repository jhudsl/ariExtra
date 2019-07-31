#' Get Notes from XML
#'
#' @param file XML file from a PPTX
#'
#' @return A character vector
#' @export
#'
#' @importFrom xml2 read_xml xml_text xml_find_all
xml_notes = function(file) {
  xdoc = xml2::read_xml(file)
  txt = xml2::xml_find_all(x = xdoc, xpath = "//a:t")
  txt = xml2::xml_text(txt)
  txt = paste(txt, collapse = " ")
  return(txt)
}



#' Get Notes from a PowerPoint (usually from Google Slides)
#'
#' @param file Character. Path for `PPTX` file
#'
#' @return Either a character vector or `NULL`
#' @export
#'
#' @importFrom utils unzip
#' @examples
#' ex_file = system.file("extdata", "example.pptx", package = "didactr")
#' pptx_notes(ex_file)
pptx_notes = function(file) {
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)
  note_dir = file.path(tdir, "ppt", "notesSlides")
  notes = list.files(path = note_dir, pattern = "[.]xml$",
                     full.names = TRUE)
  if (length(notes) > 0) {
    res = sapply(notes, xml_notes)
    bn = basename(notes)
    id = sub("[[:alpha:]]*(\\d.*)[.].*", "\\1", bn)
    ord = order(as.numeric(id))
    res = res[ord]
    bn = bn[ord]
    names(res) = bn
    return(res)
  } else {
    return(NULL)
  }
}
