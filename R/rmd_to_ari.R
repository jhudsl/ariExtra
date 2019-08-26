# using ioslides
get_nslides_ioslides = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(xpath = "//slide") %>%
    rvest::html_attr("data-total-slides") %>%
    unique()
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}

get_nslides_xaringan = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(
      xpath = '//div[ @class="remark-slide-number"]') %>%
    rvest::html_text("data-total-slides") %>%
    unique()
  num_slides = sub(".*/", "", num_slides)
  num_slides = trimws(num_slides)
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}

get_nslides_slidy = function(rendered_file) {
  doc = xml2::read_html(rendered_file)
  num_slides = doc %>%
    rvest::html_nodes(
      xpath = '//div[ @class="toolbar"]')
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = num_slides %>%
    rvest::html_nodes("span")
  txt = num_slides %>%
    html_text() %>%
    grepl(pattern = "slide")
  num_slides = num_slides[txt]
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = num_slides %>%
    html_text() %>%
    unique()
  num_slides = sub(".*/", "", num_slides)
  num_slides = trimws(num_slides)
  num_slides = suppressWarnings(as.numeric(num_slides))
  num_slides = stats::na.omit(num_slides)
  if (length(num_slides) == 0) {
    return(NA_real_)
  }
  num_slides = max(num_slides)
}


get_nslides = function(slides) {
  ##### rendering
  rendered_file = tempfile(fileext = ".html")
  if (requireNamespace("rdom", quietly = TRUE)) {
    {rdom::rdom(slides, filename = rendered_file); TRUE}

    L = list(get_nslides_ioslides,
             get_nslides_xaringan,
             get_nslides_slidy)
    n_slides_guess = vapply(L, function(x) x(rendered_file),
                            FUN.VALUE = numeric(1))
    n_slides_guess = stats::na.omit(n_slides_guess)
    if (length(n_slides_guess) > 1) {
      n_slides_guess = max(n_slides_guess)
    }
    if (length(n_slides_guess) == 0) {
      n_slides_guess = NA_real_
    }
  } else {
    n_slides_guess = NA_real_
  }
  return(n_slides_guess)
}


#' Convert a Rmd to an Ari Document
#'
#' @param path path to Rmd file
#' @param script optional spoken script, otherwise taken from the
#' HTML comments
#' @param capture_method Either `"vectorized"` or `"iterative"`. The
#' vectorized mode is faster though it can cause screens to repeat.
#' If making a video from an [rmarkdown::ioslides_presentation]
#' you should use `"iterative"`.
#' @param capturer_args a list of arguments to pass to [webshot::webshot] or
#' [pagedown::chrome_print]
#' @param ... additional arguments to pass to [make_ari_document]
#' @param capturer Methods for capturing the HTML slides
#' @param verbose print diagnostic messages
#' @param rendered_file the HTML output already from [render]
#'
#' @return The output of [make_ari_document]
#' @export
#'
#' @importFrom rvest html_nodes html_attr html_text
#' @importFrom xml2 read_html
#' @importFrom rmarkdown render
#' @importFrom webshot webshot
#' @importFrom stats na.omit
#'
#' @examples \dontrun{
#' path = system.file("extdata", "example.Rmd", package = "ariExtra")
#' res = rmd_to_ari(path)
#' res$output_file
#' if (requireNamespace("xaringan", quietly = TRUE)) {
#'    path  = system.file("examples", "lucy-demo.Rmd", package = "xaringan")
#'    rendered_file = tempfile(fileext = ".html")
#'    rmarkdown::render(path, output_format = xaringan::moon_reader(),
#'    output_file = rendered_file)
#'    testthat::expect_error({
#'    res = rmd_to_ari(path, open = FALSE, rendered_file = rendered_file,
#'    capture_method = "vectorized")
#'    },
#'    regexp = "> 0")
#' }
#' }
rmd_to_ari = function(
  path,
  script = NULL,
  capture_method = c("iterative", "vectorized"),
  capturer = c("webshot", "chrome_print"),
  capturer_args = list(),
  ...,
  rendered_file = NULL,
  verbose = TRUE
) {

  experimental = FALSE
  if (is.null(script)) {
    if (verbose) {
      message("Parsing HTML comments for script")
    }
    # yml = partition_yaml_front_matter(readLines(path))
    paragraphs = parse_html_comments(path)
    if (length(paragraphs) == 0) {
      p2 = parse_xaringan_comments(path)
      if (length(p2) > 0) {
        paragraphs = p2
      }
    }
    script = tempfile(fileext = ".txt")
    if (verbose > 1) {
      message(paste0("script is at: ", script))
    }
    writeLines(paragraphs, script)
  } else {
    stopifnot(length(script) == 1)
    stopifnot(file.exists(script))
    paragraphs = readLines(script)
  }

  capturer = match.arg(capturer)
  capture_method = match.arg(capture_method)
  if (!(capture_method %in% c("vectorized", "iterative"))) {
    stop('capture_method must be either "vectorized" or "iterative"')
  }

  if (is.null(rendered_file)) {
    if (verbose) {
      message("Rendering Rmd to HTML")
    }
    # render the HTML output
    tfile = tempfile(fileext = ".html")

    # need to figure out number of damn slides
    # either /slide for ioslides or --- for xaringan
    slides = rmarkdown::render(input = path, output_file = tfile)
  } else {
    slides = rendered_file
  }

  # Get the links
  if (file.exists(slides)) {
    slides <- normalizePath(slides)
    if (.Platform$OS.type == "windows") {
      slides_url <- paste0("file://localhost/", slides)
    } else {
      slides_url <- paste0("file://localhost", slides)
    }
  } else {
    stop("rendering the Rmd failed!")
  }


  if (capturer == "chrome_print") {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      stop("pagedown pacakge needed to use chrome_print")
    }
    pdf_file = tempfile(fileext = ".pdf")
    args = as.list(capturer_args)
    args$input = slides
    args$output = pdf_file
    args$verbose = as.numeric(verbose)
    args$format = "pdf"
    pdf_file = do.call(pagedown::chrome_print, args = args)
    n_slides_guess = pdftools::pdf_info(pdf_file)$pages
  } else {
    if (verbose) {
      message("Getting the number of slides")
    }
    if (experimental) {
      pdf_file = tempfile(fileext = ".pdf")
      args = as.list(capturer_args)
      args$url = slides_url
      args$file = pdf_file
      args$delay = 2
      do.call(webshot::webshot, args = args)
      n_slides_guess = pdftools::pdf_info(pdf_file)$pages
    } else {
      n_slides_guess = get_nslides(slides)
    }
    if (is.na(n_slides_guess)) {
      n_slides_guess = length(paragraphs)
    }

    if (n_slides_guess != length(paragraphs)) {
      warning(
        paste0(
          "Number of slides don't seem to match the script",
          ", use <!-- ; --> for slides with no comment"
        )
      )
    }
  }

  # pngs = vapply()
  slide_nums <- seq.int(n_slides_guess)
  img_paths <- sapply(slide_nums, function(r) {
    tempfile(fileext = ".png")
  })

  if (capturer == "webshot") {
    if (experimental) {
      pdftools::pdf_convert(
        pdf_file, dpi = 300,
        format = "png", filenames = img_paths,
        verbose = verbose)
    } else {
      if (verbose) {
        message("Running webshot to make PNGs")
      }
      args = as.list(capturer_args)
      if (capture_method == "vectorized") {
        args$url = paste0(slides_url, "#", slide_nums)
        args$file = img_paths
        do.call(webshot::webshot, args = args)
      } else {
        for (i in slide_nums) {
          args$url = paste0(slides_url, "#", i)
          args$file = img_paths[i]
          do.call(webshot::webshot, args = args)
        }
      }
    }
  } else {
    pdftools::pdf_convert(
      pdf_file, dpi = 300,
      format = "png", filenames = img_paths,
      verbose = verbose)
  }

  make_ari_document(
    images = img_paths,
    script = script, ..., verbose = verbose)

}
