library(testthat)

# Input has variety of entities, first entry has person and personal pronoun
simple.input.test <- c("There is a person called Julie that went down the lane. She likes bubbles", 
                       "Toys are fine", #No entities
                       "There is trouble brewing in Hong Kong", #Location Hong Kong
                       "There are two people caled Jane and John") 
# Last input variable has three entities, the Number two and 2 people, Jane and John

# Test result when no entities are present
none.input <- c("There is no entity here",
                "Nor here", 
                "This is another null entity sentence",
                "Nothing to extract here")

simple.expected <- structure(list(id = c(1L, 3L, 4L, 4L, 4L), 
                                  entity = c("Julie", "Hong Kong", "two", "Jane", "John"),
                                  entity.type = c("PERSON", "CITY", "NUMBER", "PERSON", "PERSON")),
                             class = "data.frame",
                             row.names = c(NA, -5L))

multiple.expected <- list(structure(list(id = 1L, entity = "Julie", entity.type = "PERSON"),
                                        class = "data.frame", row.names = c(NA,-1L)),
                              structure(list(id = c(1L, 2L, 2L, 2L), 
                                             entity = c("Hong Kong", "two", "Jane", "John"), 
                                             entity.type = c("CITY", "NUMBER", "PERSON", "PERSON")),
                                        class = "data.frame", row.names = c(NA, -4L)))

simple.with.pronouns.expected <- structure(list(id = c(1L, 1L, 3L, 4L, 4L, 4L), 
                                                entity = c("Julie", "She", "Hong Kong", "two", "Jane", "John"),
                                                entity.type = c("PERSON", "PERSON", "CITY", "NUMBER", "PERSON",
                                                                "PERSON")),
                                           class = "data.frame",
                                           row.names = c(NA, -6L))

pronouns <- c("he's", "hes", "he is", "He is", "He Is", "she's", "She is")

none.expected <- data.frame(id = character(), entity = character(), entity.type = character())

multi.none.expected <- list(structure(list(id = structure(integer(0), .Label = character(0), class = "factor"), 
                                           entity = structure(integer(0), .Label = character(0), class = "factor"), 
                                           entity.type = structure(integer(0), .Label = character(0), class = "factor")),
                                      class = "data.frame", row.names = integer(0)), 
                            structure(list(id = structure(integer(0), .Label = character(0), class = "factor"), 
                                           entity = structure(integer(0), .Label = character(0), class = "factor"), 
                                           entity.type = structure(integer(0), .Label = character(0), class = "factor")),
                                      class = "data.frame", row.names = integer(0)))

keys <- c("ssplit.eolonly", "annotators", "outputFormat")
values <- c("true", "tokenize,ssplit,pos,lemma,ner", "json")

all.single.entity <- as.character(1:3)

all.single.output <- structure(list(id = 1:3,
                                    entity = c("1", "2", "3"),
                                    entity.type = c("NUMBER", "NUMBER", "NUMBER")),
                               class = "data.frame", row.names = c(NA, -3L))

# If this is throwing errors that you need to download Core NLP then the way to get testthat to 
# find CORENLP is to set CORENLP as a system environment variable with the path to CoreNLP
# CoreNLP directories in the package installation cannot be located by testthat
# Nor found by R CMD check 
# E.g. cleanNLP::cnlp_download_corenlp()

test_that("NERAnnotate consistency and MultiThreadNERAnnotate setup", {

    tmp.file <- tempfile()

    file <- file(tmp.file, "wb")
    writeLines(simple.input.test, con = file)
    close(file)

    basic.keys <- c(keys, "file", "outputDirectory")
    basic.values <- c(values, tmp.file, dirname(tmp.file))

    # Expect error if NERAnnotate is called before corenlp is initialised.
    expect_error(NERAnnotate(),
                 "^Java CoreNLP not initialized. Named Entity Recognition cannot be executed.$")
    expect_error(MultiThreadedNERAnnotate(),
                 "^Java CoreNLP not initialized. Named Entity Recognition cannot be executed.$")

    r.session <- callr::r_session$new()

    # Start a new R session, initialize core NLP
    r.session$run(function(keys, values)
        cleanNLP::cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values,
                                           corenlp.only = TRUE),
        args = list(keys = basic.keys, values = basic.values))

    expect_error(r.session$run(function() cleanNLP::MultiThreadedNERAnnotate("test")),
                 regex = "Multi-threaded mode can only be used if fileList property is defined in the CoreNLP Java properties$",
                 class = "callr_status_error")

    expect_error(simple.output <- r.session$run(function()
        cleanNLP::NERAnnotate()),
        NA)
    expect_identical(simple.output, simple.expected)

    expect_error(simple.output.with.pronouns <- r.session$run(function()
        cleanNLP::NERAnnotate(entity.mentions.only = TRUE)),
        NA)
    expect_identical(simple.output.with.pronouns, simple.with.pronouns.expected)

    file <- file(tmp.file, "wb")
    writeLines(none.input, con = file)
    close(file)

    expect_error(none.output <- r.session$run(function(tmp.file)
        cleanNLP::NERAnnotate(tmp.file),
        args = list(tmp.file = tmp.file)),
        NA)
    expect_identical(none.output, none.expected)

    file <- file(tmp.file, "wb")
    writeLines(pronouns, con = file)
    close(file)

    expect_error(pronoun.output.after.validation <- r.session$run(function()
        cleanNLP::NERAnnotate(entity.mentions.only = FALSE)),
        NA)
    expect_identical(pronoun.output.after.validation, none.expected)
    
    file <- file(tmp.file, "wb")
    writeLines(all.single.entity, con = file)
    close(file)
    
    expect_error(all.single.entity.output <- r.session$run(function()
      cleanNLP::NERAnnotate(entity.mentions.only = FALSE)), NA)
    expect_identical(all.single.entity.output, all.single.output)
    
    r.session$close()
})

# Split text input vector x into number of pieces specified by threads
# Write temp file for each piece. The create a single file 
# that contains the path for each of the generated files (one per line)
splitInput <- function(x, threads, filelist.path)
{
    split.groups <- ceiling(seq_along(x)/length(x) * threads)
    split.x <- split(x, split.groups)
    file.paths <- NULL
    for (i in 1:threads)
    {
        tmp.file <- tempfile()
        file <- file(tmp.file, "wb")
        writeLines(split.x[[i]], con = file)
        close(file)
        file.paths <- append(file.paths, tmp.file)
    }
    file <- file(filelist.path, "wb")
    writeLines(file.paths, con = file)
    close(file)
    return(invisible())
}

test_that("MultiThreadNERAnnotate consistency", {
    
    tmp.file <- tempfile()
    threads <- 2
    
    splitInput(simple.input.test, threads = threads, filelist.path = tmp.file)
    
    multi.keys <- c(keys, "fileList", "outputDirectory", "threads")
    multi.values <- c(values, tmp.file, dirname(tmp.file), threads)
    
    r.session <- callr::r_session$new()
    
    # Start a new R session, initialize core NLP
    r.session$run(function(keys, values)
        cleanNLP::cnlp_init_corenlp_custom(language = "en", mem = "2g", keys = keys, values = values,
                                           corenlp.only = TRUE),
        args = list(keys = multi.keys, values = multi.values))
    
    expect_error(multiple.output <- r.session$run(function(separate.text)
        cleanNLP::MultiThreadedNERAnnotate(separate.text = separate.text),
        args = list(separate.text = TRUE)), NA)
    expect_equal(unname(multiple.output), multiple.expected)
    
    expect_error(single.output <- r.session$run(function(separate.text)
        cleanNLP::MultiThreadedNERAnnotate(separate.text = separate.text),
        args = list(separate.text = FALSE)), NA)
    expect_equal(single.output, simple.expected)
    
    row.adj <- c(0, 2)
    
    expect_error(single.output.with.row <- r.session$run(function(separate.text, row.adjustment)
        cleanNLP::MultiThreadedNERAnnotate(separate.text = separate.text, row.adjustment = row.adjustment),
        args = list(separate.text = FALSE, row.adjustment = row.adj)), NA)
    
    expect_equal(single.output.with.row, simple.expected)
    
    splitInput(none.input, threads = 2, filelist.path = tmp.file)
    
    expect_error(multiple.none.output <- r.session$run(function(separate.text)
        cleanNLP::MultiThreadedNERAnnotate(separate.text = separate.text),
        args = list(separate.text = TRUE)), NA)
    expect_equal(unname(multiple.none.output), multi.none.expected)
    
    expect_error(single.none.output <- r.session$run(function(separate.text)
        cleanNLP::MultiThreadedNERAnnotate(separate.text = separate.text),
        args = list(separate.text = FALSE)), NA)
    expect_equal(single.none.output, none.expected)
    
    r.session$close()
})

