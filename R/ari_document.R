#' Ari Document Class for Rendering
#' @param ... arguments to pass to [ari::ari_spin]
#'
#' @export
ari_document <- function(...) {
  output_movie_file <- NULL
  rm(list = "output_movie_file")
  pre_knit <- function(input) {
    # saved_files_dir <<- files_dir
    # get the output
    ext <- tolower(tools::file_ext(input))
    stub <- tools::file_path_sans_ext(input)
    # yaml = yaml::read_yaml(file = input)
    yaml <- partition_yaml_front_matter(readLines(input))
    yaml <- yaml::read_yaml(text = yaml$front_matter)
    L <- c(
      yaml$output$`ariExtra::ari_document`$output,
      yaml$output$ari_document$output
    )
    if (length(L) == 0) {
      output <- paste0(stub, ".mp4")
    } else {
      output <- unique(L)
      if (length(output) > 1) {
        output <- output[1]
      }
    }
    abs_stub <- tools::file_path_as_absolute(input)
    abs_stub <- tools::file_path_sans_ext(abs_stub)

    intermediates_dir <- paste0(abs_stub, "_files")
    dir.create(intermediates_dir, showWarnings = FALSE)


    x <- readLines(input, warn = FALSE)
    if (ext %in% "rmd") {
      paragraphs <- parse_speak_comments(input)
    } else {
      paragraphs <- parse_html_comments(input)
    }
    x <- sub("^\\s*knitr::", "", x)
    img_ind <- grepl("(^!\\s*\\[.*\\]\\s*\\(|include_graphics\\()", x)
    if (sum(img_ind) == 0) {
      stop("No images are found")
    }
    images <- x[img_ind]
    images <- trimws(images)
    images <- sub("!\\s*\\[.*\\]\\s*\\(", "", images)
    images <- sub("\\s*include_graphics\\(", "", images)
    images <- sub("\\)$", "", images)
    images <- sub("^'", "", images)
    images <- sub('^"', "", images)
    images <- sub("'$", "", images)
    images <- sub('"$', "", images)

    # need to check if voice the same
    # here is where caching may come in handy
    out_txt <- file.path(
      intermediates_dir,
      paste0("slide_", seq_along(paragraphs), ".txt")
    )
    fe <- file.exists(out_txt)
    if (any(fe)) {
      old_paragraphs <- lapply(out_txt[fe], readLines)
      names(old_paragraphs) <- out_txt[fe]
    }

    args <- list(...)
    cleanup <- args$cleanup
    if (is.null(cleanup)) {
      # use default from ari
      cleanup <- formals(ari::ari_stitch)$cleanup
    } else {
      cleanup <- as.logical(cleanup)
    }
    args$images <- images
    args$paragraphs <- paragraphs
    args$output <- output
    args$cleanup <- FALSE
    res <- do.call(ari::ari_spin, args = args)

    wav_path <- attr(res, "wav_path")
    if (!is.null(wav_path) && cleanup) {
      unlink(wav_path, force = TRUE)
    }

    txt_path <- attr(res, "txt_path")
    if (!is.null(txt_path) && cleanup) {
      unlink(txt_path, force = TRUE)
    }
    wavs <- attr(res, "wavs")

    # here is where caching may come in handy
    stopifnot(length(wavs) == length(paragraphs))
    tmp <- mapply(function(text, file) {
      writeLines(text, con = file)
    }, paragraphs, out_txt)

    output_movie_file <<- output
    out_wavs <- file.path(
      intermediates_dir,
      paste0("slide_", seq_along(wavs), ".wav")
    )
    tmp <- mapply(function(wav, file) {
      tuneR::writeWave(wav, file)
    }, wavs, out_wavs)
    return(output)
  }

  post_processor <- function(yaml_front_matter,
                             utf8_input,
                             output_file,
                             clean,
                             verbose) {
    width <- c(
      yaml_front_matter$output$`ariExtra::ari_document`$width,
      yaml_front_matter$output$ari_document$width
    )
    if (length(width) == 0) {
      width <- "100%"
    }

    height <- c(
      yaml_front_matter$output$`ariExtra::ari_document`$height,
      yaml_front_matter$output$ari_document$height
    )
    if (length(height) == 0) {
      height <- NULL
    }

    val <- c(
      "<html>",
      "<head>",
      paste0(
        '<link href="https://vjs.zencdn.net/7.6.0/video-js.css"',
        ' rel="stylesheet">'
      ),
      paste0(
        "    ",
        '<script src="https://vjs.zencdn.net/ie8/1.1.2/videojs-ie8.min.js">',
        "</script>"
      ),
      "    </head>",
      "    <body>",
      paste0(
        "      <video id='my-video' class='video-js vjs-default-skin' ",
        "data-setup='{\"fluid\": true}' ",
        "controls preload='auto' ",
        ifelse(is.null(width), "",
          paste0(" width='", width, "'")
        ),
        ifelse(is.null(height), "",
          paste0(" height='", height, "'")
        ),
        ">"
      ),
      paste0(
        "      <source src='", output_movie_file,
        "' type='video/mp4'>"
      ),
      "      <p class='vjs-no-js'>",
      paste0(
        "      To view this video please enable JavaScript, ",
        "and consider upgrading to a web browser that"
      ),
      paste0(
        "    <a href='https://videojs.com/html5-video-support/' ",
        "target='_blank'>supports HTML5 video</a>"
      ),
      "      </p>",
      "      </video>",
      "    <script src='https://vjs.zencdn.net/7.6.0/video.js'></script>",
      "      </body>",
      "</html>"
    )
    writeLines(val, output_file)
    output_file
  }

  rmarkdown::output_format(
    knitr = NULL,
    pandoc = list(to = "html", ext = ".html", keep_tex = FALSE),
    clean_supporting = FALSE,
    post_processor = post_processor,
    pre_knit = pre_knit
  )
}
