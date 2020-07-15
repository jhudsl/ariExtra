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
  rendered_file = tempfile(fileext = ".pdf")
  have_pagedown = requireNamespace("pagedown",
                                   quietly = TRUE)
  if (have_pagedown) {
    pagedown::chrome_print(input = slides,
                           output = rendered_file)
    n_slides = pdftools::pdf_info(rendered_file)$pages
    if (!is.na(n_slides) &&
        !is.null(n_slides) &&
        n_slides > 0) {
      return(n_slides)
    }
  }

  # rendered_file = tempfile(fileext = ".html")
  # have_rdom = requireNamespace("rdom", quietly = TRUE)
  have_rdom = TRUE
  if (have_rdom) {
    rendered_file = slides
    # args = list(url = slides, filename = rendered_file)
    # do.call("rdom::rdom", args = args)

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
#' @importFrom stats na.omit
#'
#' @examples
#' \donttest{
#' if (rmarkdown::pandoc_available("1.12.3")) {
#'   path = system.file("extdata", "example.Rmd", package = "ariExtra")
#'   tfile = tempfile(fileext = ".pdf")
#'   out = try({
#'     output_file = tempfile(fileext = ".html")
#'     rmarkdown::render(path, output_file = output_file)
#'     pagedown::chrome_print(output_file,
#'                            output = tfile)
#'   }, silent = TRUE)
#'   if (!inherits(out, "try-error")) {
#'     res = rmd_to_ari(path, open = FALSE)
#'     res$output_file
#'   }
#' }
#' }
#'
#' \donttest{
#'   # xaringan example
#'   if (requireNamespace("xaringan", quietly = TRUE)) {
#'     path  = system.file("examples", "lucy-demo.Rmd", package = "xaringan")
#'
#'     # get rid of ggplot2 dependency
#'     x = readLines(path)
#'     x = gsub("library\\(ggplot2\\)", "", x)
#'     x = gsub("^\\s*ggplot.*", "", x)
#'     x = gsub("^\\s*geom_bar.*", "barplot(table(mtcars$am))", x)
#'     path = tempfile(fileext = ".Rmd")
#'     writeLines(x, path)
#'     rendered_file = tempfile(fileext = ".html")
#'
#'     required_pandoc <- "1.12.3"
#'     have_pandoc_version = rmarkdown::pandoc_available(required_pandoc)
#'     if (have_pandoc_version) {
#'       rmarkdown::render(path,
#'                         output_format = xaringan::moon_reader(),
#'                         output_file = rendered_file)
#'     } else {
#'       rendered_file = system.file("extdata",
#'                                   "lucy-demo-noggplot2.html",
#'                                   package = "ariExtra")
#'     }
#'
#'
#'     script = c("this", "is", "one", "word", "per slide")
#'
#'     have_decktape = nzchar(Sys.which("decktape"))
#'     if (have_decktape) {
#'       pdf_file = tempfile(fileext = ".pdf")
#'       xaringan::decktape(rendered_file, pdf_file, docker = FALSE)
#'       res = pdf_to_ari(pdf_file, script = script, open = FALSE)
#'       result = rmd_to_ari(path = path,
#'                  script = script,
#'                  rendered_file = rendered_file,
#'                  capturer = "decktape")
#'     }
#'   }
#' }
rmd_to_ari = function(
  path,
  script = NULL,
  capture_method = c("iterative", "vectorized"),
  capturer = c("chrome_print",
               "webshot",
               "decktape"),
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
    if (length(paragraphs) == 0) {
      stop("Cannot parse comments from Rmd!")
    }
    script = tempfile(fileext = ".txt")
    if (verbose > 1) {
      message(paste0("script is at: ", script))
    }
    writeLines(paragraphs, script)
  } else {
    if (length(script) > 1 & all(!file.exists(script))) {
      tfile = tempfile(fileext = ".md")
      writeLines(script, tfile)
      script = tfile
    }
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


  #'

  if (capturer == "decktape") {
    if (!requireNamespace("xaringan", quietly = TRUE)) {
      stop("xaringan package needed to use decktape")
    }
    pdf_file = tempfile(fileext = ".pdf")
    args = as.list(capturer_args)
    args$file = slides
    args$output = pdf_file
    args$open = FALSE
    pdf_file = do.call(xaringan::decktape, args = args)
    n_slides_guess = pdftools::pdf_info(pdf_file)$pages
  } else if (capturer == "chrome_print") {
    if (!requireNamespace("pagedown", quietly = TRUE)) {
      stop("pagedown package needed to use chrome_print")
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
    if (!requireNamespace("webshot", quietly = TRUE)) {
      stop("webshot package needed to use webshot")
    }
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
  # at least we know they will be in order
  img_paths = sort(img_paths)

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
