% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pkg.R
\docType{package}
\name{cleanNLP-package}
\alias{cleanNLP}
\alias{cleanNLP-package}
\title{cleanNLP: A Tidy Data Model for Natural Language Processing}
\description{
Provides a set of fast tools for converting a textual corpus into a set
of normalized tables. The underlying natural language processing pipeline
utilizes either the Python module spaCy or the Java-based Stanford
CoreNLP library. The Python option is faster and generally easier to
install; the Java option has additional annotators that are not available
in spaCy.
}
\details{
Once the package is set up, run one of \code{\link{cnlp_init_tokenizers}},
\code{\link{cnlp_init_spacy}}, or \code{\link{cnlp_init_corenlp}} to load the
desired NLP backend. After this function is done running, use
\code{\link{cnlp_annotate}} to run the annotation engine over a corpus
of text. Functions are then available to extract data tables from the
annotation object: \code{\link{cnlp_get_token}}, \code{\link{cnlp_get_dependency}},
\code{\link{cnlp_get_document}}, \code{\link{cnlp_get_coreference}},
\code{\link{cnlp_get_entity}}, \code{\link{cnlp_get_sentence}},
and \code{\link{cnlp_get_vector}}. See their documentation for
further details. The package vignettes provide more detailed set-up
information.

If loading annotation that have previously been saved to disk, these
can be pulled back into R using \code{\link{cnlp_read_csv}}. This
does not require Java or Python nor does it require initializing the
annotation pipeline.
}
\examples{

\dontrun{
# load the annotation engine (can also use spaCy and coreNLP backends)
setup_tokenizers_backend()
init_backend(type = "tokenizers")

# annotate your text
annotation <- run_annotators("path/to/corpus/directory")

# pull off data tables
token <- cnlp_get_token(annotation)
dependency <- cnlp_get_dependency(annotation)
document <- cnlp_get_document(annotation)
coreference <- cnlp_get_coreference(annotation)
entity <- cnlp_get_entity(annotation)
sentiment <- cnlp_get_sentence(annotation)
vector <- cnlp_get_vector(annotation)
}

}
\seealso{
Useful links:
\itemize{
  \item Report bugs at \url{http://github.com/Displayr/cleanNLP/issues}
}

}
\author{
\strong{Maintainer}: Taylor B. Arnold \email{taylor.arnold@acm.org}

Other contributors:
\itemize{
  \item Justin Wishart \email{justin.wishart@displayr.com} [contributor]
}

}
