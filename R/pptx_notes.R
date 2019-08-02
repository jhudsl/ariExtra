#' Get Notes from XML
#'
#' @param file XML file from a PPTX
#' @param collapse_text should text be collapsed by spaces?
#'
#' @return A character vector
#' @export
#'
#' @importFrom xml2 read_xml xml_text xml_find_all
xml_notes = function(file, collapse_text = TRUE) {
  xdoc = xml2::read_xml(file)
  txt = xml2::xml_find_all(x = xdoc, xpath = "//a:t")
  txt = xml2::xml_text(txt)
  if (collapse_text) {
    txt = paste(txt, collapse = " ")
  }
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
#' pptx_slide_note_df(ex_file)
#' pptx_slide_text_df(ex_file)
pptx_notes = function(file) {

  df = pptx_slide_note_df(file)
  if (is.null(df)) {
    return(NULL)
  }
  ss = split(df, basename(df$file))
  res = sapply(ss, function(x) {
    paste(x$text, collapse = " ")
  })
  res[ res == ""] = ";"
  return(res)
}

#' @export
#' @rdname pptx_notes
pptx_slide_text_df = function(file) {

  L = unzip_pptx(file)
  slides = L$slides

  if (length(slides) > 0) {
    # in case empty notes
    res = lapply(slides, function(x) {
      xx = xml_notes(x, collapse_text = FALSE)
      if (length(xx) == 0) {
        return(NULL)
      }
      snum = sub("[.]xml", "", sub("slide", "", basename(x)))
      snum = as.numeric(snum)
      data.frame(
        file = x,
        slide = snum,
        text = xx,
        index = 1:length(xx),
        stringsAsFactors = FALSE)
    })
    res = do.call(rbind, res)
    return(res)
  } else {
    return(NULL)
  }
}

#' @export
#' @rdname pptx_notes
pptx_slide_note_df = function(file) {

  L = unzip_pptx(file)
  notes = L$notes
  slides = L$slides
  note_dir = L$note_dir

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
    res = lapply(notes, function(x) {
      xx = xml_notes(x, collapse_text = FALSE)
      if (length(xx) == 0) {
        xx = ""
      }
      snum = sub("[.]xml", "", sub("notesSlide", "", basename(x)))
      snum = as.numeric(snum)
      data.frame(
        file = x,
        slide = snum,
        text = xx,
        index = 1:length(xx),
        stringsAsFactors = FALSE)
    })
    res = do.call(rbind, res)
    return(res)
  } else {
    return(NULL)
  }
}


#' @export
#' @rdname pptx_notes
unzip_pptx = function(file) {
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)
  rm(res)
  slide_dir = file.path(tdir, "ppt", "slides")
  slides = list.files(path = slide_dir, pattern = "[.]xml$",
                      full.names = TRUE)

  note_dir = file.path(tdir, "ppt", "notesSlides")
  notes = list.files(path = note_dir, pattern = "[.]xml$",
                     full.names = TRUE)

  tdir = normalizePath(tdir)
  props_dir = file.path(tdir, "docProps")
  props_file = file.path(props_dir, "core.xml")
  ari_core_file = system.file("extdata", "docProps",
                              "core.xml", package = "ariExtra")
  if (!dir.exists(props_file)) {
    dir.create(props_dir, recursive = TRUE)
    file.copy(ari_core_file, props_file,
              overwrite = TRUE)
  }

  L = list(slides = slides,
           notes = notes,
           slide_dir = slide_dir,
           note_dir = note_dir,
           props_dir = props_dir,
           props_file = props_file,
           root_dir = tdir)
  return(L)
}
