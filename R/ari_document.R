#' @export
ari_document = function(...) {

  pre_knit <- function(input) {
    # saved_files_dir <<- files_dir
    # get the output
    stub = tools::file_path_sans_ext(input)
    output = paste0(stub, ".mp4")
    abs_stub = tools::file_path_as_absolute(input)
    abs_stub = tools::file_path_sans_ext(abs_stub)

    intermediates_dir = paste0(abs_stub, "_files")
    dir.create(intermediates_dir, showWarnings = FALSE)


    x = readLines(input, warn = FALSE)
    paragraphs <- parse_html_comments(input)
    x = sub("^\\s*knitr::", "", x)
    img_ind = grepl("(^!\\s*\\[.*\\]\\s*\\(|include_graphics\\()", x)
    if (sum(img_ind) == 0) {
      stop("No images are found")
    }
    images = x[img_ind]
    images = trimws(images)
    images = sub("!\\s*\\[.*\\]\\s*\\(", "", images)
    images = sub("\\s*include_graphics\\(", "", images)
    images = sub("\\)$", "", images)
    images = sub("^'", "", images)
    images = sub('^"', "", images)
    images = sub("'$", "", images)
    images = sub('"$', "", images)

    # need to check if voice the same
    # here is where caching may come in handy
    out_txt = file.path(intermediates_dir,
                        paste0("slide_", seq_along(paragraphs), ".txt"))
    fe = file.exists(out_txt)
    if (any(fe)) {
      old_paragraphs = lapply(out_txt[fe], readLines)
      names(old_paragraphs) = out_txt[fe]
    }

    res = ari_spin(
      images = images,
      paragraphs = paragraphs,
      output = output,
      cleanup = FALSE,
      ...)
    wav_path = attr(res, "wav_path")
    if (!is.null(wav_path)) {
      unlink(wav_path, force = TRUE)
    }

    txt_path = attr(res, "txt_path")
    if (!is.null(txt_path)) {
      unlink(txt_path, force = TRUE)
    }

    wavs = attr(res, "wavs")

    # here is where caching may come in handy
    stopifnot(length(wavs) == length(paragraphs))
    tmp = mapply(function(text, file) {
      writeLines(text, con = file)
    }, paragraphs, out_txt)

    output_movie_file <<- output
    out_wavs = file.path(intermediates_dir,
                         paste0("slide_", seq_along(wavs), ".wav"))
    tmp = mapply(function(wav, file) {
      tuneR::writeWave(wav, file)
    }, wavs, out_wavs)
    return(output)
  }

  post_processor = function(yaml_front_matter,
                            utf8_input,
                            output_file,
                            clean,
                            verbose) {
    val = c(
      "<html>",
      '<head>',
      paste0('<link href="https://vjs.zencdn.net/7.6.0/video-js.css"',
             ' rel="stylesheet">'),
      paste0(
        '    ',
        '<script src="https://vjs.zencdn.net/ie8/1.1.2/videojs-ie8.min.js">',
        '</script>'),
      '    </head>',
      '    <body>',
      "      <video id='my-video' class='video-js' controls preload='auto' ",
      paste0("      <source src='", output_movie_file,
             "' type='video/mp4'>"),
      "      <p class='vjs-no-js'>",
      paste0("      To view this video please enable JavaScript, ",
             "and consider upgrading to a web browser that"),
      paste0("    <a href='https://videojs.com/html5-video-support/' ",
             "target='_blank'>supports HTML5 video</a>" ),
      "      </p>",
      "      </video>",
      "    <script src='https://vjs.zencdn.net/7.6.0/video.js'></script>",
      "      </body>",
      "</html>")
  writeLines(val, output_file)
  output_file
}

rmarkdown::output_format(
  knitr = NULL,
  pandoc = list(to = "html", ext = ".html", keep_tex = FALSE),
  clean_supporting = FALSE,
  post_processor = post_processor,
  pre_knit = pre_knit)
}
