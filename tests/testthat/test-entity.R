library(testthat)

tom.cruise <- c("Nothing I really like him.", "he stuck on him self", "he is gay", 
                "RELIGOUS VIEWS", "Nothing", "He doesn't seem very genuine.", 
                "He thinks is too cool", "i hate everything about him.  he sucks as an actor", 
                "I think he is a great actor but not a very good husband", "I dont like that he acts like he is better than everyone else.", 
                "nothing", "He's  good actor, but hes a real jerk", "his ego", 
                "scientology", "his scientology beliefs", "nothing", "not sure", 
                "everything", "his apparant (whether real or not) arrogance; Scienetology", 
                "everything", "nothing", "Loves himself", "Vain Scientologist", 
                "He is a strange religious freak", "i like him and dislike nothing", 
                "he seems really weird and kinda crazy", "That he left Nicole Kidman.", 
                "he is full of him self,", "He is much too self centered!", "He is nice", 
                "I dislike that his personality seems unstable.", "He is a great actor.", 
                "that he isnt here,nothing else.", "There is nothing about tom cruise i dislike.", 
                "I really have nothing aganst him because he worked and earned money on his own merits'. On the other hand I hate Chelsea Clinton because she got handed high paying jobs' based on who her parents' are.", 
                "I fine him to be very argon", "His controlling nature", "to full of himself", 
                "just about everything", "his ridiculous religion", "snob", "n/a", 
                "nothing", "I like Tom Cruise over all but it seems like he isn't being totally honest about some things in his life.", 
                "his arrogant attitude", "Nothing.  He is ok!", "science christian", 
                "To good looking for me.", "nothing", "hes too arrogant", "he is a great actor good stunts in movies", 
                "Hes a smart ass", "absolutely NOTHING!!!!!!", "I hate the Scientology thing.", 
                "smiles too much and sometimes acts like an idiot", "none", "scientology", 
                "don't know much about him, have never seen any of his movies, so can't say if there is anything about him i dislike.", 
                "Thinks he's GOD", "no", "I just think the media makes him out to be crazy. The Scientology part is crazy", 
                "11", "hes an idiot that thinks hes better than anyone else", 
                "His church!", "he is a good actor", "he is flipping weird", 
                "He is a little strange.  I do not trust members of cults.", 
                "conceited", "his movies are good and his politics are not", 
                "his \"religion\"", "Something that he can't help but he is short.  I like tall leading men.", 
                "everything i think he is a idiot", "That he and his bitch ass church are allowed to treat people like shit.", 
                "He's a scientologist nut.", "i hate him. everything about him", 
                "don't know", "I don't think he is an honest person.", "I think Tom Cruise is somewhat arrogant.", 
                "he is ok", "nothing", "he seems arrogant", "He's arrogant enough to believe he is not crazy", 
                "nothing i dislike", "Style", "it's time for him to do movies that don't require his unrealistic stunts.  he's getting too old", 
                "his religion and he just thinks he is too good for everyone else.", 
                "they way he treated his wife and child", "weird", "he seem phony", 
                "He's a control freak in a crazy religion", "xxxxx", "nothing", 
                "his personality and looks", "he's a religious zealot/jerk", 
                "I like his movies but not much else.", "he's a scientologist.", 
                "Sometimes seems a bit arrogant", "He's too judgemental of others beliefs", 
                "There isn't anything that I dislike about Tom Cruise.", "He is a snob.", 
                "I like him, he is cool", "sometimes, he can be odd. But I think he is a great actor.", 
                "scientology", "he thinks he's better than everyone else", "he's so arrogent", 
                "I do not know enought about Tom Cruise to give an answer.", 
                "I neither like nor dislike Tom Cruise", "nothing", "nothing", 
                "However can I dislike a fellow, whom I don't know?  Ask a different question.", 
                "n/a", "I like Tom Cruise.", "I don't think he's a straightforward person; too involved in scientology", 
                "I believe he is stuck up and takes advantage of women.", NA, 
                "arrogance", "I don't think he is a good actor.", "I'm sorry that he is involved with Scientology however that is his business", 
                "the fact that he don't do fantasy movies any more.", "nothing", 
                "I've never met Tom Cruise so I don't know if I have anything to like or dislike about him.", 
                "scientology craziness", "nothing he is an idiot who is in a Scientology cult", 
                "all", "Fair enough actor, don't like the roles he plays.", "I like Tom Cruise. He's a decent actor but gets a lot of bad press for his religion which is stupid.", 
                "selfish", "he good actor", "nothing", "none", "his attitude       his religion", 
                "nothing", "he is too weird", "na", "nothing", "good", "Needs to do more action movies", 
                "Too much of a show off and too visible with his Scientology.", 
                "nothing", "Nothing", "Much to egotistical and standoffish, and full of himself.", 
                "his better than thou attitude", "i dont kow", "he is short and creepy", 
                "dfghjkl;'", "He is a brainwashed cult crazy lunatic with an intolerant agenda.", 
                "kblujb", "nothing", "He is a weirdo ever since the War of the World Movie", 
                "I love him", "nothing", "he is a weird person", "I think he's a little fanatical", 
                "he is crazy", "nothing", "Absolutely nothing.", "nothing", "His inflated ego.", 
                "I do like him!", "nothing", "I dislike everything I refuse to watch anything he is in.", 
                "He is over rated and a little weird", "I like him, there nothing to dislike about him", 
                "acting is good--his personnlity sucks", "Pretty much everything!", 
                "Nothing", "I like TOm cruise", "Nothing in particular", "he is a washed-up has-been.  His star is quite faded.", 
                "opinionated", "arrogant", "He seems arrogant and full of himself.  A know it all.", 
                "his religion", "i dont know much about him", "He is creapy", 
                "Everything", "scientology", "He is annoying and arrogant", "He can't act anymoreand his head is to big for his ego.  And all he does this the same thing, seen one mission seen them all.", 
                "He thinks he knows everything and is obnoxious", "He's sort of weird, and the fact that he divorced Nicole just before their 10th year anniversary was just low.", 
                "He acts like he is better than everyone else and he is the only one that is right.", 
                "nothing i dislike", "good actors and no dislike him", "hes just weird and a scientologist", 
                "Whether he actually is or not, his media persona is crazy, unhinged, controlling, and strange.", 
                "actor", "he's too full of  himself!", "Nothing", "nothing", 
                "he's arrogant", "He's full of himself.", "nothing    nothing", 
                "he is crazy", "Good actor but not sure about his religious connection.", 
                "He's pretty egocentric, and I don't think he's a very good actor - pretty much all the characters he plays are the same.", 
                "Good", "I think he's crazy", "He's a decent actor - his Scientology beliefs are nuts", 
                "He seems to get the STRANGEST publicity.", "He's crazy", "He thinks he knows more than other people because he is a Scientolgist", 
                "wimpy", "good actor", "Don't like anything about Tom Cruise.", 
                "i like tom cruise but he can have his moments he needs to break away from the crazy religion he is in and try to make it work with his ex wife to take care of his daughter", 
                "He's overrated and not a good actor", "his narrow views on other faiths", 
                "He seems crazy.", "he's cool", "everything", "his religious beliefs or sinintology practices", 
                "na", "that he's a Scientologist", "He is odd", "he's crazy", 
                "I thank he ok for an actor", "I think he is arrogant and has a weird religious philosophy.", 
                "i like his good acting and boyish look nothing i don't like about T.C.", 
                "His particular faith which he appeared to force his ex wife to accept and live according to their rules.", 
                "I like Tom cruise", "His Scientology is forced upon people.  He also appears smug.", 
                "Nothing", "that he is a christian scientist.", "Everyone thinks he's hot but I just think he's okay.", 
                "I have neutral feelings", "He's cocky and weird at the same time", 
                "no good movies hes played in lately", "He is too phonie", "he reflects an unreal image", 
                "He pushes his religion on people.", "Nothing dont know the guy.", 
                "maybe because he believe in scientogy", "Scientology", "He's a Scientologist.", 
                "Other than his religious views, nothing.", "None", "I respect the religion of eveyone ... But sciensology is enough...", 
                "He's a crazy cult member who puts other people down for not thinking the way he does.", 
                "He seems arrogant and \"a know it all\"", "that he assumes he is better that everyone with little regard to others.", 
                "His movies from the 90s and 80s. I dislike his psycho religion.", 
                "Hollywood", "nothing", "He appears very cocky", "I think he is overrated. I don't like his choice of movies, his style, or really anything about him.", 
                "his hair", "He judges people and their lifestyle. for example when said bad things about Brooke shields because she took anti depressants after she had her child.", 
                "scientology", "I like him as an actor and his private life none is concern of mine", 
                "Scientology!", "Crazy man, religious beliefs.", "Nothing really u can't take his great acting away from him , he's kida weird, but who r we to say", 
                "He's ok. I'm not quite sure who exactly he really is", "I LOVE TOM CRUIE DONT LIKE THE SCIENTOLOGY THING BUT LOVE IS ACTING", 
                "Nothing at all, I happen to think that he is an egomaniac, although i did like his wife Katie Holmes and I love their little girl Sury!", 
                "what is it?", "Not much.", "I don't like that he's a controlling mysogynist, or his involvement with the church of scientology", 
                "His religious affiliation in the last several years.", "Not sure just don't see why ppl like him so much.", 
                "nothing", "nothing", "He seems to be very cocky", "He's gone off the deep end.", 
                "His affiliation. With Scientology", "I don't care for his religious choices and how he treats his women.", 
                "Don't like the way he has and still does use his daughter to get his way with the baby's mother.", 
                "There is nothing about him that i don't like", "I like Tom cruise", 
                "I adore Tom Cruise however; the Scientology issue is extremely worrisome and very odd.", 
                "i like is good actor", "well i like that he is a great actor and i love most of the movies that he stars in i really dont have any dislikes about him.i like when he act in action pack movies those are my favorites", 
                "One of my favorite actors", "That he perches about Scientology to others is a dislike but I love his movies!!", 
                "That he's crazy", "Whats not to like?", "The way he keeps his children in tabloids and his religious.views being made into headline news", 
                "He seems to be elusive about his religion.", "his acting skill is excellent", 
                "His religion is a little bizarre and I'm not a fan of any of his movies.", 
                "I like Tom cruise have nothing negative about him?", "He's a self obsessed idiot.", 
                "Im not familiar with Tom Cruise", "I find him to be a little arrogant even though he is a pretty good actor.", 
                "Tom Cruise is a good actor. There isn't anything I don't like about him.", 
                "everything, I just dont think he is a good director, actor, husband to kate, or a good father to their children", 
                "I don't dislike anything about Tom Cruise", "He is arrogent and doesn't know what he is talking about.", 
                "I don't like him because of his cockiness he always seems to have whenever he is shown on tv in interviews or talkshows.", 
                "i don't have an opinion on tom cruise", "attitude", "The different religion he is fallowing", 
                "I like all off tom cruise", "Hfdthvi", "I do not really pay attension to who acts in movies I just watch the movie.", 
                "Tom Cruise is too cocky. It annoys me.", "His arrogance", "Tom Cruise just isn't that interesting in this day and age.", 
                "I dislike the way he changed over to Scientology and the drama with Katie Holmes and his self."
)

result <- structure(list(id = c("doc1", "doc8", "doc9", "doc12", "doc14", 
                                "doc15", "doc23", "doc27", "doc32", "doc35", "doc44", "doc47", 
                                "doc51", "doc54", "doc57", "doc61", "doc62", "doc65", "doc74", 
                                "doc78", "doc96", "doc99", "doc102", "doc103", "doc106", "doc107", 
                                "doc112", "doc113", "doc117", "doc118", "doc121", "doc122", "doc123", 
                                "doc125", "doc126", "doc126", "doc128", "doc138", "doc146", "doc149", 
                                "doc177", "doc179", "doc181", "doc181", "doc182", "doc185", "doc187", 
                                "doc195", "doc196", "doc199", "doc199", "doc204", "doc205", "doc207", 
                                "doc214", "doc217", "doc219", "doc221", "doc222", "doc224", "doc226", 
                                "doc234", "doc235", "doc239", "doc242", "doc242", "doc243", "doc248", 
                                "doc249", "doc250", "doc251", "doc255", "doc256", "doc259", "doc260", 
                                "doc266", "doc270", "doc271", "doc271", "doc272", "doc273", "doc274", 
                                "doc275", "doc282", "doc284", "doc285", "doc286", "doc286", "doc287", 
                                "doc287", "doc288", "doc295", "doc297", "doc299", "doc299", "doc300", 
                                "doc300"),
                         sid = c(0L, 2L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 0L, 1L, 0L, 0L, 1L, 1L, 1L, 
                                 1L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 1L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 0L, 
                                 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 
                                 1L, 0L, 1L, 1L, 1L, 1L, 1L, 0L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L, 1L,
                                 1L, 0L, 0L, 0L, 0L, 1L, 0L, 2L, 1L, 0L, 0L, 1L, 0L, 0L, 0L, 0L, 1L,
                                 0L, 0L, 0L, 0L, 0L, 1L, 1L, 0L, 0L, 0L, 0L, 0L), 
                         tid = c(2L, 5L, 7L, 4L, 1L, 2L, 2L, 4L, 5L, 7L, 3L, 2L, 5L, 4L, 1L, 2L, 1L,
                                 5L, 4L, 3L, 4L, 9L, 8L, 1L, 7L, 6L, 3L, 14L, 9L, 9L, 5L, 1L, 10L, 
                                 3L, 3L, 5L, 3L, 12L, 5L, 8L, 1L, 11L, 13L, 17L, 15L, 6L, 1L, 2L, 16L,
                                 5L, 8L, 2L, 6L, 8L, 5L, 7L, 15L, 3L, 2L, 5L, 3L, 1L, 4L, 5L, 5L, 7L, 
                                 1L, 8L, 1L, 6L, 1L, 8L, 21L, 17L, 5L, 2L, 3L, 3L, 8L, 5L, 9L, 1L, 5L,
                                 3L, 5L, 16L, 1L, 6L, 11L, 13L, 7L, 1L, 1L, 1L, 9L, 9L, 14L), 
                         tid_end = c(2L, 5L, 7L, 4L, 1L, 2L, 2L, 5L, 5L, 8L, 4L, 2L, 5L, 4L, 1L, 2L, 
                                     1L, 5L, 4L, 4L, 4L, 10L, 8L, 1L, 8L, 7L, 4L, 14L, 9L, 9L, 6L, 1L,
                                     11L, 3L, 4L, 5L, 3L, 12L, 5L, 8L, 1L, 11L, 13L, 18L, 15L, 6L, 1L,
                                     2L, 16L, 5L, 8L, 2L, 7L, 8L, 5L, 7L, 15L, 4L, 2L, 6L, 3L, 1L, 4L,
                                     5L, 5L, 7L, 1L, 8L, 1L, 6L, 1L, 8L, 22L, 19L, 8L, 2L, 4L, 4L, 8L,
                                     5L, 9L, 1L, 5L, 3L, 6L, 16L, 2L, 6L, 11L, 13L, 8L, 1L, 2L, 2L, 10L,
                                     9L, 15L), 
                         entity_type = c("NUMBER", "TITLE", "TITLE", "TITLE", "RELIGION", "RELIGION", 
                                         "RELIGION", "PERSON", "TITLE", "PERSON", "PERSON", "RELIGION",
                                         "TITLE", "RELIGION", "RELIGION", "RELIGION", "NUMBER", "TITLE",
                                         "RELIGION", "PERSON", "RELIGION", "PERSON", "TITLE","RELIGION",
                                         "PERSON", "PERSON", "PERSON", "RELIGION", "TITLE", "RELIGION",
                                         "PERSON", "RELIGION", "RELIGION", "TITLE", "PERSON", "TITLE",
                                         "TITLE", "RELIGION", "RELIGION", "CAUSE_OF_DEATH", "RELIGION",
                                         "NUMBER", "PERSON", "DATE", "NUMBER", "RELIGION", "TITLE",
                                         "TITLE", "TITLE", "TITLE", "RELIGION", "TITLE", "PERSON", 
                                         "TITLE", "RELIGION", "TITLE", "PERSON", "PERSON", "RELIGION",
                                         "RELIGION", "IDEOLOGY", "RELIGION", "RELIGION", "RELIGION",
                                         "NUMBER", "NUMBER", "CITY", "PERSON", "RELIGION", "TITLE",
                                         "RELIGION", "RELIGION", "PERSON", "IDEOLOGY", "DATE",
                                         "RELIGION", "PERSON", "PERSON", "RELIGION", "TITLE", "TITLE",
                                         "NUMBER", "RELIGION", "PERSON", "PERSON", "TITLE", "PERSON",
                                         "TITLE", "TITLE", "TITLE", "PERSON", "PERSON", "PERSON",
                                         "PERSON", "DATE", "RELIGION", "PERSON"), 
                         entity = c("I", "actor", "actor", "actor", "scientology", "scientology", 
                                    "Scientologist", "Nicole Kidman", "actor", "Chelsea Clinton", 
                                    "Tom Cruise", "christian", "actor", "Scientology", "scientology",
                                    "Scientology", "11", "actor", "scientologist", "Tom Cruise",
                                    "scientologist", "Tom Cruise", "actor", "scientology", "Tom Cruise",
                                    "Tom Cruise", "Tom Cruise", "scientology", "actor", "Scientology",
                                    "Tom Cruise", "scientology", "Scientology cult", "actor", 
                                    "Tom Cruise", "actor", "actor", "Scientology", "cult", "War", 
                                    "scientology", "one", "Nicole", "10th year", "one", "scientologist",
                                    "actor", "actor", "actor", "actor", "Scientology", "actor", 
                                    "Tom Cruise", "actor", "Scientologist", "actor", "T.C.", "Tom cruise",
                                    "Scientology", "christian scientist", "neutral", "Scientology", 
                                    "Scientologist", "cult", "90s", "80s", "Hollywood", "Brooke", 
                                    "scientology", "actor", "Scientology", "SCIENTOLOGY", "Katie Holmes",
                                    "church of scientology", "the last several years", "Scientology", 
                                    "Tom cruise", "Tom Cruise", "Scientology", "actor", "actor", "One",
                                    "Scientology", "Tom", "Tom Cruise", "actor", "Tom Cruise", "actor",
                                    "director", "actor", "Tom Cruise", "Hfdthvi", "Tom Cruise", 
                                    "Tom Cruise", "this day", "Scientology", "Katie Holmes")
                         , entity_normalized = c("0.0", "", "", "", "", "", "", "", "", "", "", "", 
                                                 "", "", "", "", "11.0", "", "", "", "", "", "", "",
                                                 "", "", "", "", "", "", "", "", "", "", "", "", "",
                                                 "", "", "", "", "1.0", "", "P1Y-#10", "1.0", "", "",
                                                 "", "", "", "", "", "", "", "", "", "", "", "", "", 
                                                 "", "", "", "", "90.0", "80.0", "", "", "", "", "", 
                                                 "", "", "", "PREV_IMMEDIATE PXY", "", "", "", "", "",
                                                 "", "1.0", "", "", "", "", "", "", "", "", "", "", "",
                                                 "", "THIS P1D", "", "")),
                    class = c("tbl_df", "tbl", "data.frame"), 
                    row.names = c(NA, -97L))

# If this is throwing errors that you need to download Core NLP then the way to get testthat to 
# find CORENLP is to set CORENLP as a system environment variable with the path to CoreNLP
# CoreNLP directories in the package installation cannot be located by testthat
# Nor found by R CMD check 
# E.g. wget
# 
test_that("get_entity consistency", {
  cnlp_init_corenlp_custom(language = "en", mem = "2g", 
                           keys="annotators", values="tokenize, ssplit, pos, lemma, ner")
  
  annotated <- cnlp_annotate(tom.cruise, as_strings = TRUE, backend = "coreNLP")
  table.output <- cnlp_get_entity(annotated)
  expect_identical(table.output, result)
})