#' @example file = "~/Downloads/Leanpub_MOOCs_JHU.pptx"
pptx_rewriter = function(
  file,
  replace_df,
  type = c("slides", "notes")
) {
  L = unzip_pptx(file)
  root_dir = L$root_dir
  runners = L[[type]]
  type = match.arg(type)
  func = switch(slides = pptx_slide_text_df,
                notes = pptx_slide_note_df)

  df = func(file)
  stopifnot(nrow(df) == nrow(replace_df))

  run_file = runners[2]
  slide = xml2::read_xml(run_file)
  txt_nodes = xml2::xml_find_all(x = slide, xpath = "//a:t")
  if (length(txt_nodes) > 0) {
    xml2::xml_text(txt_nodes) = replace_df$text
    xml2::write_xml(slide, run_file)
  }

  owd = getwd()
  on.exit({
    setwd(owd)
  })
  setwd(root_dir)
  outfile = tempfile(fileext = ".pptx")
  all_files = list.files(path = ".", recursive = TRUE,
                         all.files = TRUE, full.names = TRUE)
  utils::zip(zipfile = outfile, files = all_files)

  return(outfile)
}


# translate_pptx = function(
#   file,
#   target = "es",
#   detect = TRUE,
#   verbose = TRUE,
#   ...) {
#   # googleAuthR::gar_auto_auth()
#   # googleLanguageR::gl_auth()
#   if (!inherits(googleAuthR::Authentication$public_fields$token, "Token")) {
#     stop("Google Language is not Authorized, see gl_auth")
#   }
#   text = NULL
#   rm(list = "text")
#
#   if (verbose) {
#     message("Pulling notes from Google Slides")
#   }
#
#   make_bad_string = function() {
#     x = round(runif(1, min = 1e5, max = 1000000))
#     x = paste0(x, ";")
#   }
#
#
#   tb_df = pptx_slide_note_df(file)
#
#   # Actually translate
#   L = list(id = id,
#            table_of_changes = tb_df)
#   if (nrow(tb_df) > 0) {
#     bad_string =  make_bad_string()
#     for (i in 1:10) {
#       # just make another
#       if (any(grepl(bad_string, tb_df$text))) {
#         bad_string =  make_bad_string()
#       }
#     }
#     stopifnot(!any(grepl(bad_string, tb_df$text)))
#     tb_df$text = gsub("\n", bad_string,
#                       tb_df$text)
#
#     tb = tb_df$text
#     file = tempfile()
#     writeLines(tb, con = file)
#     if (verbose) {
#       message("Temporary File Created: ", file,
#               " with bad_string: ", bad_string)
#     }
#     if (detect) {
#       if (verbose) {
#         message("Detecting Language")
#       }
#       out = gl_detect_file(file)
#       if (out$language == target) {
#         message(page_id, " already in target language")
#         return(NULL);
#       }
#     }
#
#     df = chunk_google_translate(
#       file,
#       target = target,
#       chunk = TRUE,
#       fix_header = FALSE)
#     df$translatedText = gsub(
#       bad_string, "\n",
#       df$translatedText)
#     df$text = gsub(
#       bad_string, "\n",
#       df$text)
#     tb_df$text = gsub(
#       bad_string, "\n", tb_df$text)
#     tb = gsub(bad_string, "\n", tb)
#
#     tb_new = df$translatedText
#     stopifnot(length(tb) == length(tb_new))
#     stopifnot(!any(grepl(bad_string, tb_new)))
#     stopifnot(!any(grepl(bad_string, tb)))
#
#     tb_df$text_replacement = tb_new
#     # delete text
#     out = pptx_rewriter(file,
#                         tb_df)
#     L$table_of_changes = tb_df
#     L$outfile = out
#   }
#   return(L)
# }


