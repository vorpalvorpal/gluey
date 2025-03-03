% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/hook.R
\name{setup_gluey_preprocessing}
\alias{setup_gluey_preprocessing}
\title{Set up gluey preprocessing for R Markdown and Quarto documents}
\usage{
setup_gluey_preprocessing(enabled = TRUE)
}
\arguments{
\item{enabled}{Whether to enable gluey preprocessing (default: TRUE)}
}
\description{
This function registers gluey_knit as the document processor.
It should be called in .onLoad to ensure gluey is properly initialized.
}
