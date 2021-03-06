% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ner_annotate.R
\name{NERAnnotate}
\alias{NERAnnotate}
\title{Run the annotation pipeline on a set of documents to extract entities}
\usage{
NERAnnotate(entity.mentions.only = FALSE)
}
\arguments{
\item{entity.mentions.only}{Logical to specify if only entity mention output from CoreNLP is used
in the extraction. If TRUE, this will extract personal pronouns as well as standard entities.
The benefit of the entity.mention output is it groups words that are from the same entity. E.g.
'John Smith' is a single person entity and 'New York City' is a single location entity. The 
entity mention output also classifies personal pronouns as entities and will be extracted.
If FALSE, the entity.mentions output from CoreNLP is validated against the CoreNLP token output.
The token output also identifies entities on the single word level and it doesn't classify personal
pronouns as entities. The net effect if set to FALSE is that entity mentions are extracted but
personal pronouns and other potential entity mentions that are not entities on the token level
are not extracted.}
}
\value{
data.frame with the details of the detected entities. The output data.frame has three
   columns. \itemize{
       \item \code{id} integer: the row index of the input file that has an extracted entity.
       \item \code{entity} character: The extracted entity word (e.g. William)
       \item \code{entity.tyoe} character: The entity type of the extracted entity (e.g. Person)
   }
}
\description{
Runs the entity detection algorithms from CoreNLP using CoreNLP java library via rJava.
It expects the CoreNLP java object to already be initialised with rJava with a call to 
\code{cnlp_init_corenlp_custom} with the appropriate annotators setup for named entity
recognition and a path to an input file that has the input text separately by new lines.
The input file must have Unix style line endings or will cause the CoreNLP java call to crash
with a null pointer exception. The function returns a \code{data.frame} showing the location
in the document whre each entity occurs and the entity type. If no entities are detected for a
document then an empty data.frame with no rows is returned.
}
\examples{
\dontrun{
simple.input.test <- c("John is a person", "Google is a company", "This is nothing")
input.file <- tempfile()
file <- file(input.file, "wb") # need linux style line endings
writeLines(simple.input.test, con = file)
close(file)
keys <- c("ssplit.eolonly", "annotators", "outputFormat", "file", "outputDirectory",
          "output.prettyPrint")
values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json", input.file, dirname(input.file),
            "true")

cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values)
simple.output <- NERAnnotate()
}
}
