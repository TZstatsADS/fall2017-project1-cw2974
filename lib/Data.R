###set directory
folder.path=paste0(getwd(),"/data/InauguralSpeeches/")
speeches=list.files(path = folder.path, pattern = "*.txt")
prex.out=substr(speeches, 6, nchar(speeches)-4)
list.path=paste0(getwd(),"/data/InaugurationInfo.csv")
inaug.list<-read.csv(list.path,stringsAsFactors = FALSE)

###filter president with 2 term
second_term<-inaug.list[inaug.list[,3]==2,]
second_term_repub<-na.omit(second_term_repub<-second_term[second_term$Party=="Republican",])
second_term_demo<-na.omit(second_term[second_term$Party=="Democratic",])

###filter president with only 1 term
first_term<-anti_join(inaug.list, second_term, by = "President")
first_term_repub<-first_term[first_term$Party=="Republican",]
first_term_demo<-first_term[first_term$Party=="Democratic",]

#transformation of corpora, remove meaningless character
ff.all<-Corpus(DirSource(folder.path))
ff.all<-tm_map(ff.all, stripWhitespace)
ff.all<-tm_map(ff.all, content_transformer(tolower))
ff.all<-tm_map(ff.all, removeWords, stopwords("english"))
ff.all<-tm_map(ff.all, removeWords, character(0))
ff.all<-tm_map(ff.all, removePunctuation)

tdm.all<-TermDocumentMatrix(ff.all)
tdm.tidy=tidy(tdm.all)

###compute TF-IDF weighted document-term matrices for individual speeches
dtm <- DocumentTermMatrix(ff.all,
                          control = list(weighting = function(x)
                            weightTfIdf(x, 
                                        normalize =FALSE),
                            stopwords = TRUE))
ff.dtm=tidy(dtm)

###find the document in the path
first<-paste0("inaug",first_term$File,"-1.txt")
second<-paste0("inaug",second_term$File,"-1.txt")

first_demo<-paste0("inaug",first_term_demo$File,"-1.txt")
first_repub<-paste0("inaug",first_term_repub$File,"-1.txt")

second_demo<-paste0("inaug",second_term_demo$File,"-1.txt")
second_repub<-paste0("inaug",second_term_repub$File,"-1.txt")


###making subset
tdm.all = ff.dtm
tdm.tidy = subset(ff.dtm,document %in% speeches)

tdm.trump = subset(tdm.tidy,document == 'inaugDonaldJTrump-1.txt')

tdm_first = subset(tdm.tidy,document ==first)
tdm_second = subset(tdm.tidy,document ==second)

tdm_first_demo = subset(tdm.tidy,document ==first_demo)
tdm_first_repub= subset(tdm.tidy,document ==first_repub)

tdm_second_demo = subset(tdm.tidy,document ==second_demo)
tdm_second_repub= subset(tdm.tidy,document ==second_repub)



###sentence list for sentence length and sentiment analysis
inaug.list$fulltext=NA
sentence.list=NULL

for(i in seq(nrow(inaug.list))) {
  fileName<-paste0(folder.path,speeches[i])
  text <- readLines(fileName)
  inaug.list$fulltext[i]=text
  sentences=sent_detect(inaug.list$fulltext[i],
                        endmarks = c("?", ".", "!", "|",";"))
  
  if(length(sentences)>0){
    emotions=get_nrc_sentiment(sentences)
    word.count=word_count(sentences)
    sentence.list=rbind(sentence.list,
                        cbind(inaug.list[i,-ncol(inaug.list)],
                              sentences=as.character(sentences),
                              word.count,
                              emotions,
                              sent.id=1:length(sentences)
                        )
    )
  }
}

###get only first inaugral speech
sentence.list<-sentence.list[sentence.list$Term==1,]
