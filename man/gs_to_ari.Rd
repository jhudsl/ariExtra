% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/gs_ari.R
\name{gs_to_ari}
\alias{gs_to_ari}
\alias{gs_pptx_notes}
\alias{pptx_to_pdf}
\alias{pptx_to_pngs}
\alias{pptx_to_ari}
\alias{pdf_to_ari}
\alias{html_to_ari}
\alias{pdf_to_pngs}
\alias{images_to_ari}
\alias{to_ari}
\title{Convert Google Slides and notes to video with ari}
\usage{
gs_to_ari(path, script = NULL, ..., verbose = TRUE)

gs_pptx_notes(path, verbose = TRUE, ...)

pptx_to_pdf(path, verbose = TRUE)

pptx_to_pngs(path, verbose = TRUE, dpi = 600)

pptx_to_ari(path, script = NULL, ..., verbose = TRUE)

pdf_to_ari(path, script = NULL, dpi = 300, ..., verbose = TRUE)

html_to_ari(path, script = NULL, ..., verbose = TRUE)

pdf_to_pngs(path, verbose = TRUE, dpi = 600)

images_to_ari(path, script = NULL, dpi = 300, ..., verbose = TRUE)

to_ari(path, script = NULL, ..., verbose = TRUE)
}
\arguments{
\item{path}{Identifier of google slides presentation, or PPTX filename}

\item{script}{passed to \code{\link[=make_ari_document]{make_ari_document()}}}

\item{...}{additional arguments to \code{\link{pptx_notes}}}

\item{verbose}{print diagnostic messages}

\item{dpi}{resolution (dots per inch) to render images}
}
\value{
The output from \link{make_ari_document}
}
\description{
Convert Google Slides and notes to video with ari
}
\examples{
\donttest{
# takes > 5 seconds to run
  id = "1Opt6lv7rRi7Kzb9bI0u3SWX1pSz1k7botaphTuFYgNs"
  res = gs_to_ari(id, verbose = 2, open = FALSE)
  if (interactive()) {
    file.edit(res$output_file)
  }
  # replicates same thing as above without verbosity
  res2 = to_ari(id, open = FALSE)
}

ex_file = system.file("extdata", "example.pptx", package = "ariExtra")
have_soffice = try(docxtractr:::lo_assert())
if (!inherits(have_soffice, "try-error")) {
  pngs = try({
    pptx_to_pngs(ex_file)
  }, silent = TRUE)

  soffice_config_issue = inherits(pngs, "try-error")
  if (soffice_config_issue) {
    warning(
      paste0("soffice does not seem configured properly, may need to ",
             "adapt LD_LIBRARY_PATH, ",
             "try ariExtra:::fix_soffice_library_path()")
    )
    # this can be due to a library issue:
    url = paste0("https://codeyarns.github.io/tech/2019-09-05",
                 "-libregloso-cannot-open-shared-object-file.html")
    if (interactive()) {
      utils::browseURL(url)
    }
  }
  if (!soffice_config_issue) {
    res = pptx_to_ari(ex_file, open = FALSE)
    if (interactive()) {
      file.edit(res$output_file)
    }
    res2 = to_ari(ex_file, open = FALSE)
  }
}
ex_file = system.file("extdata", "example.pdf", package = "ariExtra")
res = pdf_to_ari(ex_file, script = c("hey", "ho"), open = FALSE)
if (interactive()) {
file.edit(res$output_file)
}
\donttest{
res2 = to_ari(ex_file,  script = c("hey", "ho"), open = FALSE)
}
}
