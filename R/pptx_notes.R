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
#' file = ex_file = system.file("extdata", "example.pptx",
#' package = "ariExtra")
#' pptx_notes(ex_file)
pptx_notes = function(file) {
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)
  slide_dir = file.path(tdir, "ppt", "slides")
  slides = list.files(path = slide_dir, pattern = "[.]xml$",
                      full.names = TRUE)

  note_dir = file.path(tdir, "ppt", "notesSlides")
  notes = list.files(path = note_dir, pattern = "[.]xml$",
                     full.names = TRUE)
  if (length(notes) > 0) {
    # in case empty notes
    assoc_notes = sub("slide", "", basename(slides))
    assoc_notes = paste0("notesSlide", assoc_notes)
    assoc_notes = file.path(note_dir, assoc_notes)
    no_fe = !file.exists(assoc_notes)
    if (any(no_fe)) {
      file.create(assoc_notes[no_fe])
      notes = assoc_notes
    }
    res = sapply(notes, xml_notes)
    bn = basename(notes)
    id = sub("[[:alpha:]]*(\\d.*)[.].*", "\\1", bn)
    ord = order(as.numeric(id))
    res = res[ord]
    res[is.na(res)] = ""
    bn = bn[ord]
    names(res) = bn
    # in case empty notes
    res[ res == ""] = ";"
    return(res)
  } else {
    return(NULL)
  }
}
