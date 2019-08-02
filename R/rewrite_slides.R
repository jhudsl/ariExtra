#' @example file = "~/Downloads/Leanpub_MOOCs_JHU.pptx"
rewrite_notes = function(file) {
  library(xml2)
  tdir = tempfile()
  dir.create(tdir)
  res = unzip(file, exdir = tdir)

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

  slide_dir = file.path(tdir, "ppt", "slides")
  slides = list.files(path = slide_dir, pattern = "[.]xml$",
                      full.names = TRUE)

  slide_file = slides[2]
  slide = xml2::read_xml(slide_file)
  txt_nodes = xml2::xml_find_all(x = slide, xpath = "//a:t")
  txt = xml2::xml_text(txt_nodes)
  rep_txt = sample(letters, size = length(txt))
  paths = xml2::xml_path(txt_nodes)
  xml_text(txt_nodes) = rep_txt
  xml2::write_xml(slide, slide_file)

  owd = getwd()
  setwd(tdir)
  outfile = tempfile(fileext = ".pptx")
  all_files = list.files(path = ".", recursive = TRUE,
                         all.files = TRUE, full.names = TRUE)
  utils::zip(zipfile = outfile, files = all_files)


  setwd(owd)
  return(outfile)
}
