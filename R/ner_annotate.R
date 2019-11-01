#' @title Run the annotation pipeline on a set of documents to extract entities
#'
#' @description Runs the entity detection algorithms from CoreNLP using CoreNLP java library via rJava.
#' It expects the CoreNLP java object to already be initialised with rJava with a call to 
#' \code{cnlp_init_corenlp_custom} with the appropriate annotators setup for named entity
#' recognition and a path to an input file that has the input text separately by new lines.
#' The input file must have Unix style line endings or will cause the CoreNLP java call to crash
#' with a null pointer exception. The function returns a \code{data.frame} showing the location
#' in the document whre each entity occurs and the entity type. If no entities are detected for a
#' document then an empty data.frame with no rows is returned.
#'
#' @param entity.mentions.only Logical to specify if only entity mention output from CoreNLP is used
#'    in the extraction. If TRUE, this will extract personal pronouns as well as standard entities.
#'    The benefit of the entity.mention output is it groups words that are from the same entity. E.g.
#'    'John Smith' is a single person entity and 'New York City' is a single location entity. The 
#'    entity mention output also classifies personal pronouns as entities and will be extracted.
#'    If FALSE, the entity.mentions output from CoreNLP is validated against the CoreNLP token output.
#'    The token output also identifies entities on the single word level and it doesn't classify personal
#'    pronouns as entities. The net effect if set to FALSE is that entity mentions are extracted but
#'    personal pronouns and other potential entity mentions that are not entities on the token level
#'    are not extracted.
#' @return data.frame with the details of the detected entities. The output data.frame has three
#'    columns. \itemize{
#'        \item \code{id} integer: the row index of the input file that has an extracted entity.
#'        \item \code{entity} character: The extracted entity word (e.g. William)
#'        \item \code{entity.tyoe} character: The entity type of the extracted entity (e.g. Person)
#'    }
#' @importFrom rJava .jcall
#' @examples
#' \dontrun{
#' simple.input.test <- c("John is a person", "Google is a company", "This is nothing")
#' input.file <- tempfile()
#' file <- file(input.file, "wb") # need linux style line endings
#' writeLines(simple.input.test, con = file)
#' close(file)
#' keys <- c("ssplit.eolonly", "annotators", "outputFormat", "file", "outputDirectory",
#'           "output.prettyPrint")
#' values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json", input.file, dirname(input.file),
#'             "true")
#' 
#' cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values)
#' simple.output <- NERAnnotate()
#' }
#' @export
NERAnnotate <- function(entity.mentions.only = FALSE) {
    
    if(!volatiles$corenlp$init)
        stop("Java CoreNLP not initialized. Named Entity Recognition cannot be executed.")
    if(is.null(volatiles$corenlp$properties$file))
        stop("Java CoreNLP properties doesn't have an input file path.",
             "Please set the input file path via cnlp_init_corenlp_custom")
    .jcall(volatiles$corenlp$corenlp, "V", "run")
  
    parseNEROutput(file = volatiles$corenlp$properties$file, entity.mentions.only = entity.mentions.only)
}
#' @title Parse the NER output from CoreNLP
#' @description Reads the JSON file output from coreNLP ner annotator and coverts to a data frame
#' @param file Character string with the path of the input file without extension to be processed
#' @param entity.mentions.only Logical to determine if only the entity mention output is processed. 
#' The entity mention output includes entities and any grouped words e.g. John Smith is a single entity
#' instead of two separate entities. However, this output also includes personal pronouns (he, she etc)
#' as entities. If this logical is set to FALSE, the entity output is validated agains the token output.
#' The token output specifies if a word is an entity and doesn't classify personal pronouns as entities.
#' @return data.frame with three columns, 
#' \itemize{
#'        \item \code{id} integer: the row index of the input file that has an extracted entity.
#'        \item \code{entity} character: The extracted entity word (e.g. William)
#'        \item \code{entity.tyoe} character: The entity type of the extracted entity (e.g. Person)
#'    }
#' @importFrom jsonlite fromJSON
parseNEROutput <- function(file, entity.mentions.only)
{
    while (!file.exists(paste0(file, ".json")))
        Sys.sleep(1)
    output <- fromJSON(paste0(file, ".json"))
    ner.mentions = output$sentences$entitymentions
    response = sapply(ner.mentions, nrow)
    if(all(sapply(response, is.null)))
        out <- data.frame(id = character(), entity = character(), entity.type = character())
    else {
        if(!entity.mentions.only)
        {
            # Validate ner.mentions against the token output
            ner.mentions <- mapply(function(x, y)
            {
                if(nrow(y) != 0)
                {
                    idx <- sapply(1:nrow(y), function(i)
                    {
                        z <- y[i, ]
                        ind <- c(z$tokenBegin + 1, z$tokenEnd)
                        all(x[ind, ]$ner != "O")
                    })
                    if(any(idx))
                        y <- y[idx, ]
                    else
                        return(data.frame())
                }
                y
            }, x = output$sentences$tokens, y = ner.mentions, SIMPLIFY = FALSE)
            # Check if filtered ner is not empty
            if(all(sapply(ner.mentions, nrow) == 0))
                return(data.frame(id = character(), entity = character(), entity.type = character()))
        }
        response <- sapply(ner.mentions, nrow)
        response <- rep(1:length(ner.mentions), response)
        ner.mentions = lapply(ner.mentions, function(x) {
            if(nrow(x) != 0)
                subset(x, select = c("text", "ner"))
        })
        # Remove the NULL list elements
        ner.mentions <- Filter(Negate(is.null), ner.mentions)
        out = cbind(response, do.call(rbind.data.frame, ner.mentions))
        names(out) <- c("id", "entity", "entity.type")
    }
    out
}

#' @title Run the annotation pipeline on a set of documents to extract entities with multiple threads
#'
#' @description Runs the entity detection algorithms from CoreNLP using CoreNLP java library via rJava.
#' It expects the CoreNLP java object to already be initialised with rJava with a call to 
#' \code{cnlp_init_corenlp_custom} with the appropriate annotators setup for named entity
#' recognition and a path to a filelist that contains a list of files (one file path per line)
#' each of the pointed file paths has the input text separately by new lines.
#' The input file must have Unix style line endings or will cause the CoreNLP java call to crash
#' with a null pointer exception. The function returns a \code{data.frame} showing the location
#' in the document whre each entity occurs and the entity type. If no entities are detected for a
#' document then an empty data.frame with no rows is returned.
#' 
#' @param separate.text Logical to specify if the threaded calculation was used on a single corpus 
#' that was split into separate processing threads. If \code{TRUE}, the files in the filelist are 
#' assumed to be unique (not related) and each output is treated separately. If \code{FALSE} the 
#' input files are assumed to be split from the same text input data. Then document ids are 
#' cumulative over all the threads and need to be adjusted and combined into a single data.frame.
#' @param row.adjustment Integer vector of cumulative counts of lines in each of the previous files.
#'    If the file split is already known, this can be used to speed up computation (prevent \code{readLines}
#'    executing on of a big bunch of files only to compute number of lines).
#' @param entity.mentions.only Logical to specify if only entity mention output from CoreNLP is used
#'    in the extraction. If TRUE, this will extract personal pronouns as well as standard entities.
#'    The benefit of the entity.mention output is it groups words that are from the same entity. E.g.
#'    'John Smith' is a single person entity and 'New York City' is a single location entity. The 
#'    entity mention output also classifies personal pronouns as entities and will be extracted.
#'    If FALSE, the entity.mentions output from CoreNLP is validated against the CoreNLP token output.
#'    The token output also identifies entities on the single word level and it doesn't classify personal
#'    pronouns as entities. The net effect if set to FALSE is that entity mentions are extracted but
#'    personal pronouns and other potential entity mentions that are not entities on the token level
#'    are not extracted.
#' @return If \code{separate.text} is \code{FALSE}, then a single data.frame with the details of the 
#' detected entities is returned. If \code{TRUE}, a list of data.frames is returned where each list 
#' element refers to each file input. The output data.frame has three
#'    columns. \itemize{
#'        \item \code{id} integer: the row index of the input file that has an extracted entity.
#'        \item \code{entity} character: The extracted entity word (e.g. William)
#'        \item \code{entity.tyoe} character: The entity type of the extracted entity (e.g. Person)
#'    }
#' @importFrom jsonlite fromJSON
#' @importFrom rJava .jcall
#' @examples
#' \dontrun{
#' simple.input.test <- c("John is a person", "Google is a company", "This is nothing")
#' 
#' input.file.1 <- tempfile()
#' file <- file(input.file.1, "wb") # need linux style line endings
#' writeLines(simple.input.test[1:2], con = file)
#' close(file)
#' input.file.2 <- tempfile()
#' file <- file(input.file.2, "wb") # need linux style line endings
#' writeLines(simple.input.test[3:4], con = file)
#' close(file)
#' filelist <- tempfile()
#' file <- file(filelist, "wb") # need linux style line endings
#' writeLines(c(input.file.1, input.file.2), con = file)
#' close(file)
#' 
#' keys <- c("ssplit.eolonly", "annotators", "outputFormat", "fileList", "outputDirectory",
#'           "output.prettyPrint")
#' values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json", filelist, dirname(filelist),
#'             "true")
#' 
#' cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values)
#' simple.output <- MultiThreadedNERAnnotate()
#' }
#' @export
MultiThreadedNERAnnotate <- function(separate.text = FALSE, row.adjustment = NULL,
                                     entity.mentions.only = FALSE)
{
    if(!volatiles$corenlp$init)
        stop("Java CoreNLP not initialized. Named Entity Recognition cannot be executed.")
    if(is.null(volatiles$corenlp$properties$fileList))
        stop("The CoreNLP java properties doesn't have the fileList property set. ",
             "Multi-threaded mode can only be used if fileList property is defined in the CoreNLP Java properties")
    .jcall(volatiles$corenlp$corenlp, "V", "run")
    
    file.paths <- readLines(volatiles$corenlp$properties$fileList)
    output.file.paths <- paste0(file.paths, ".json")
    if(!separate.text)
    {
        if(is.null(row.adjustment))
            row.adjustment <- c(0, sapply(file.paths[-length(file.paths)], function(x) length(readLines(x)), USE.NAMES = FALSE))
    }
    else
        row.adjustment <- rep(0, length(file.paths))
    output <- mapply(function(file, count, mention) {
        out <- parseNEROutput(file, entity.mentions.only = mention)
        if (nrow(out) > 0 & count != 0)
            out$id = out$id + count
        out
    }, file = file.paths, count = row.adjustment, mention = entity.mentions.only,
    SIMPLIFY = FALSE)
    if(!separate.text)
        output <- do.call(rbind.data.frame, unname(output))
    output
}