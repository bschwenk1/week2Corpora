selectRandomTexts <- function(text, filenameOut, fraction){
    #Because the twitter, blogs and news files are too big to load into memory at once, we only want to get a random subsample.
    #Take random sample of the total text (of the size equal to the fraction) and write it to a file
    
    #example: selectRandomTexts(textObject,"en_US.blogs_smaller.txt",0.1)
    textOut <- text[rbinom(length(text),1,fraction)==1] #only use a small part of the text
    writeLines(textOut, filenameOut)
    textOut
}

saveNGramsTexts <- function(textDataFrame, filenameOut){
    write.csv(x=textDataFrame, file=filenameOut)
    print("documents saved")
}

increaseMemoryAllocation <- function(mb=8000){
    memory.size(max=mb)
    print(paste("memoryAllocation set to: ",mb," MB"))
}

loadLinesFromFile <- function(dir="", filename="", lines=-1L){
    #load all amount of lines (default = all) from textfiles in folder dir into a corpus. Default dir is current folder.
    #example: loadCorpusSourcesFromDir(dir="sources/")
    library(tm)
    old_dir <- getwd() #save directory to return to after finishing the function
    
    if (dir != "") setwd(dir) #change dir if necessary.
    con <- file(filename, "r") 
    text <- readLines(con, lines) ## Read the first x lines from the textfile
    close(con) 
    
    setwd(old_dir)
    
    print(paste(length(text)," lines from file ",filename," are loaded"))
    text #return corpus object as result
}

cleanStringTexts <- function(textObject){
    #When loading the texts there are many unrecognized characters outside ASCII text set. 
    #Do some basic cleaning on them
    library(stringr)
    textObject <- str_replace_all(textObject, "â€™", "'")
    textObject <- str_replace_all(textObject, "â€", "-")
    textObject <- str_replace_all(textObject, "â€œ", '"')
    textObject <- str_replace_all(textObject, "Ëœ", "'")
    textObject <- str_replace_all(textObject, "â€™", "'")
    textObject <- str_replace_all(textObject, "-œ", '"')
    textObject <- str_replace_all(textObject, "-\u009d -”", '"')
    textObject <- str_replace_all(textObject, "-\u009d", '')
    textObject <- str_replace_all(textObject, "\u009d", '')
    textObject <- str_replace_all(textObject, "â˜¹", '')
    textObject <- str_replace_all(textObject, "ï¿½", "'")
    textObject <- str_replace_all(textObject, "â˜…", " ")
    textObject <- str_replace_all(textObject, "â€“", "-")
    textObject <- str_replace_all(textObject, "â„¢“"," trademark ")
    textObject <- str_replace_all(textObject, "â‚¬"," euro ")
    textObject <- str_replace_all(textObject, "â€œ"," ")
    textObject <- str_replace_all(textObject, "â€¦"," ")
    textObject <- str_replace_all(textObject, "Ë†"," ")
    textObject <- str_replace_all(textObject, "Â®"," registered trademark ")
    textObject <- str_replace_all(textObject, "Â©"," copy right ")
    textObject <- str_replace_all(textObject, enc2utf8("â€™"),"'")
    textObject <- str_replace_all(textObject, enc2utf8("â€"),"-")
    textObject <- str_replace_all(textObject, enc2utf8("â€œ"),'"')
    textObject <- str_replace_all(textObject, "”"," ")
    textObject <- str_replace_all(textObject, "“"," ")
    
    
    textObject
}

textToCorpus <- function(textObject){
    #colnames(textObject) <- c("lines")
    #docs <- textObject$lines
    print(paste("converting text object with ",length(textObject)," lines to corpus. This can take some time..."))
    corpus <- VCorpus(VectorSource(textObject))
    print("text object loaded to corpus")
    corpus
}


loadCorpusSourcesFromDir <- function(dir=""){
    #load all textfiles in folder dir into a corpus. Default dir is current folder.
    #example: loadCorpusSourcesFromDir(dir="sources/")
    library(tm)
    old_dir <- getwd() #save directory to return to after finishing the function
       
       if (dir != "") setwd(dir) #change dir if necessary.
       myCorpus <-Corpus(DirSource(), readerControl = list(language="en_US"))
     
    setwd(old_dir)

    print(paste("Files in ",dir," are loaded into corpus"))
    myCorpus #return corpus object as result
}

getSummaryFromCorpus <- function(corpus){
    #Get a a few basic figures on the Corpus. 
    print(paste("Number of documents: ",length(corpus)))
    print(paste("The first document content: ",inspect(corpus[[1]])))
    print(paste("The last document content: ",inspect(corpus[[length(corpus)]])))
}

#convertToCharacter <- content_transformer(function(corpus, fromPattern, toPattern){return (gsub(fromPattern, toPattern, corpus))})


preProcessCorpus <- function(corpus){
    #first fix strange characters that do not fit Ascii coding in the corpus
    #replace by corresponding ascii coding or remove it.
    #example: preProcessCorpus(corpus)
    
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â\\€\\™", replacement = "'") #'replacement
    print("Corpus pre-processing 1% (bespoke cleaning starting)")
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â\\€", replacement = "-") #- replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "â™¥", replacement = "") #heart replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â\\“", replacement = " ") #remove
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\?", replacement = ".") #replace ? signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\*", replacement = " ") #replace * signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "!", replacement = ".") #replace ! signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ",", replacement = " ") #remove comma signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ";", replacement = ".") #replace ; signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â\\€\\¦", replacement = " ") #remove ... signs
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â\\€\\œ", replacement = '"')  #"replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "â€™", replacement = "'")  #"replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "â€", replacement = "-")  #"replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "Ëœ", replacement = "'")  #'replacement
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â€.*?(+ +){1}", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\€", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\™", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\â.*?(+ +){1}", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\ã.*?(+ +){1}", replacement = " ")  #strange word removal
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "http[^[:space:]]*", replacement = '')  #remove URLS
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\S+@\\S+|\\{(?:\\w+, *)+\\w+\\}@[\\w.-]+", replacement = '')  #remove emailadresses
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = '@\\S+', replacement = " ")  #twitter link replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\@", replacement = ' ')  #remove @ characters
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\|", replacement = "")  #| remove
    print("Corpus pre-processing 25% (bespoke cleaning continuing)")
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\+", replacement = " plus ")  #+ remove
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\&", replacement = " and ")  #& remove
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ":\\)", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ":-\\)", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ";\\)", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ";-S", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ":-\\(", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ":\\(", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = ":D", replacement = '')  #remove smiley
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\.\\.", replacement = '.')  #remove double dot
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\_", replacement = " ")  #_ replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\-", replacement = " ")  #- replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\/", replacement = " ")  #/ replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "+ +\\<", replacement = " ")  #< replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "10th", replacement = " tenth ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "9th", replacement = " nineth ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "8th", replacement = " eight ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "7th", replacement = " seventh ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "6th", replacement = " sixth ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "5th", replacement = " fifth ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "4th", replacement = " fourth ")  #th replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "3rd", replacement = " third ")  #rd replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "2nd", replacement = " second ")  #rd replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "1st", replacement = " first ")  #rd replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\>+ +", replacement = " ")  #> replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\:", replacement = " ")  #: replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\(", replacement = " ")  #( replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\)", replacement = " ")  #) replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\{", replacement = " ")  #{ replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\}", replacement = " ")  #} replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\|", replacement = " ")  #| replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\¦", replacement = " ")  #¦ replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\'+ +", replacement = " ")  #' replacement by space (beginning)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "\\`+ +", replacement = " ")  #` replacement by space (beginning)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "+ +\\'", replacement = " ")  #' replacement by space (ending)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "+ +\\`", replacement = " ")  #` replacement by space (ending)
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = '\\"', replacement = " ")  #" replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = '\\\"', replacement = " ")  #\ replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = '#\\S+', replacement = " ")  #twitter hashtag replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = '\\#"', replacement = " ")  # hashtag replacement by space
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(\\.){2,10}", replacement = ".")  # remove multiple points
    corpus <- tm_map(corpus, content_transformer(gsub), pattern = "(+ +){2,10}", replacement = " ")  # remove more than one spaces
    print("Corpus pre-processing 50% (bespoke cleaning finished)")
    
    
    #do various transformations to clean the dataset
    corpus <- tm_map(corpus, stripWhitespace)  #strip white spaces
    print("Corpus pre-processing 60% (whitespace cleaning finished)")
    
    corpus <- tm_map(corpus, removeNumbers)    #remove all numbers
    print("Corpus pre-processing 70% (number cleaning finished)")
    
    corpus <- tm_map(corpus, removePunctuation, preserve_intra_word_contractions=TRUE, preserve_intra_word_dashed=TRUE) #remove all punctuations.
    print("Corpus pre-processing 80% (punctuation cleaning finished)")
    
    corpus <- tm_map(corpus, content_transformer(tolower)) #set all caps to lower
    print("Corpus pre-processing 100% (all to low caps cleaning finished)")
    #corpus <- tm_map(corpus, removeWords, stopwords("english")) #remove common stop words from text (NOT DO THIS, BECAUSE STOP WORDS SHOULD ALSO BE PREDICTED AS NEXT WORD)
    
    print("Corpus pre-processing finished (e.g. remove strange characters, strip white space and remove caps.")
    corpus
}

getWordsList<-function(corpus){
    dtm<-DocumentTermMatrix(corpus)
    dtm$dimnames
}

getUniqueWordsCount<-function(corpus){
    dtm<-DocumentTermMatrix(corpus)
    dtm$ncol
}

getProfaneWordsList <- function(){
    old_dir <- getwd() #save directory to return to after finishing the function  
    setwd("profanities")
    #full list of profane words downloaded from: https://www.freewebheaders.com/full-list-of-bad-words-banned-by-google/; 8-11-2018
    list <- read.delim("full-list-of-bad-words_comma-separated-text-file_2018_07_30.txt", sep=";", header=FALSE) #load words
    
    list <- data.frame(t(list)) #transpose and convert to data frame
    names(list) <- "words"
    list$words <- as.character(list$words)  
    setwd(old_dir)
    
    #do some processing to get correct naming
    library(stringr)
    list<-str_replace(list$words, "  ", " ") #replace double space by single space
    list <- list[nchar(list)>3] #only select words of certain length or words that have numbers (otherwise too much resemblence to normal words)
    list <- list[!is.na(list)] #remove any NA's from the list
    
    #not filter on a few words that are common in normal contexts or can be found in normal words (mostly 4 letter words). ,
    notFilterOn <- c("murder","cracy", "drunk","snuff","racy","meth","tart","prod","pron","slope","escort", "snatch", "teat", "teste", "teets","stfu","spook","spic","spik","spac","sodom","rump", "revue", "poon","poof","pawn","wang","weed","womb","muff","mong","mofo","maxi","massa", "lube","lech","loin","lust","kwif","jock","hump","hell","fook","fice","dink","dike","dirty","darn","crap", "crack","bloody","balls", "vodka","toke", "strip", "pust", "prig", "hebe", "heeb", "ginger", "flaps","tramp","thug", "stroke", "organ", "tart", "racy","shi+")
    list <- list[!grepl(pattern=paste(notFilterOn, collapse = "|") ,x=list)]
    
    list <- data.frame(list)   #convert to nice structured data frame
    names(list) <- "words"
    list$words <- as.character(list$words) 
    
    list
}

GetProfaneWordsInCorpus<-function(corpus){
    #filter profane words from the corpus text
    profane <- getProfaneWordsList()
    words <- data.frame(as.character(getWordsList(corpus)$Terms))
    names(words) <- "terms"
    words$terms <- as.character(words$terms)
    
    #see if there is a match between the corpus text and profane words list.
    Pattern = paste(profane$words, collapse="|")
    match <- data.frame(words$terms, result=grepl(Pattern, words$terms, ignore.case = TRUE)) 
    
    profaneWordsInCorpus <- match[match$result==TRUE,]
    print(paste(nrow(profaneWordsInCorpus)," unique profane Words where found in the corpus text."))
    
    #only return list if words where found
    if (sum(profaneWordsInCorpus$result>0)){
        as.character(profaneWordsInCorpus$words.terms)
    } else NULL   
    
}

RemoveProfaneWordsFromCorpus <- function(corpus){
    #remove words that are profane from the corpus
    #watch out: this changes of course the order (next word is removed. maybe is replace by xxxx better, so the whole sencence could be removed)
    corpus <- tm_map(corpus, removeWords, GetProfaneWordsInCorpus(corpus))
    print("Profane words removed from corpus.")
    corpus
}

ReplaceProfaneWordsFromCorpus <- function(corpus){
    #replace words that are profane from the corpus by *beep*
    #watch out: this changes word too *beep*. When guessing the next word, all combinations with *beep* should be later ignored.
    profane <- GetProfaneWordsInCorpus(corpus)
    
    #remove common combinations of profane words, that are not profane
    profane<-setdiff(profane, c("cocktails","cocktail","analytics","buttons","button","trimming","trim","classes","class","butter","psychology","pakistan","choreographing","humanitarian","participating","staff","anticipate","analysis","analyse","class","glasses","librarian","librarianship","naked","participate","passes","participatory","shore","saturday","sophomoric","psychoanalysis","skill","skills","superstition","trimming","vegetarian"))
    
    #going through list and replace all words
    if (length(profane)>0){
        for (i in 1:length(profane)){ 
            if(i==1) print("0% starting -beeping- profane words. Processing can be slow!")
            if(i==round(length(profane)/20,0)) print("5%")
            if(i==round(length(profane)/4,0)) print("25%")
            if(i==round(length(profane)/2,0)) print("50%")
            if(i==round(length(profane)/4*3,0)) print("75%")
            if(i==length(profane)) print("100%")
            
            corpus <- tm_map(corpus, content_transformer(gsub), pattern = profane[i], replacement = "*beep*")  #strange word removal
        }
    } 
    print("Profane words removed from corpus.")
    corpus
}

BigramTokenizer <- function(corpus){
        #code taken from: http://tm.r-forge.r-project.org/faq.html#Encoding coding example from tm package
        unlist(lapply(ngrams(words(corpus), 2), paste, collapse = " "), use.names = FALSE)
}

ThreegramTokenizer <- function(corpus){
    unlist(lapply(ngrams(words(corpus), 3), paste, collapse = " "), use.names = FALSE)
}


GetCommonWordsInCorpus <- function(corpus,minimal=5){
    #get all words that are more or equally common than the minimal (this is actually equal to already existing function: findFreqTerms(dtm, minimal))
    dtm <- DocumentTermMatrix(corpus)
    commonWords <- as.character(data.frame(dtm$dimnames)$Terms[dtm$v >= minimal])
    print(paste("These words are listed more than ",minimal-1," times: ",commonWords))
    commonWords
}

GetDocumentTermMatrixFromCorpus <- function(corpus){
    #get all word counts (dtm$v) from words in the corpus (dtm$dimnames)
    dtm <- DocumentTermMatrix(corpus)
    dtm
}

GetTermDocumentMatrixFromCorpus <- function(corpus){
    #get all word counts (dtm$v) from words in the corpus (dtm$dimnames)
    tdm <- TermDocumentMatrix(corpus)
    print(paste("The most common terms in the corpus are: ",inspect(tdm)))
    tdm
}

FilterDTMForSparseTerms <- function(documentTermMatrix){
    dtm <- inspect(removeSparseTerms(documentTermMatrix, 0.3))
    
}

PartOfSpeechTaggingOnCorpus <- function(corpus){
    library("openNLP") 
    
    tagged <- tagPOS(corpus)
    tagged
}


