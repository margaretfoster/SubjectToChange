## Takes UCDP data frame
## for one specific group
## runs searchK, prints plot

library(quanteda)
library(stm)
library(dplyr)

## takes a ucdp data subset,
## runs a two--topic STM on the full articles
group.searchK <- function(data, groupID, kmax){
    set.seed(6889)
    
    print(dim(data))

    tmp.articletext <- data$source_article
    
    ## Remove numbers
    data$source_article<- gsub(pattern='[[:digit:]]+',
                               replacement="",
                               x= data$source_article)

    data$source_article<- tolower(data$source_article) 
    
    tmp.corpus <- quanteda::corpus(data,
                                   docid_field="id",
                                   text_field="source_article")
    
    
    ## pull out words that are very likely to refer to news agencies:
    ## and news reporting:
    news.agencies <- c("bbc*", "reuters","xinhua", "agenc*",
                       "post", "monitor*", "associ*",
                       "*service*", "afp", "france-press",
                       "*news*", "*source*","timeline*",
                       "*tv*","television", "*radio*",
                       "*voice*","audio", "dispatch",
                       "repor*", "track*", "media",
                       "ap", "*press*", "times",
                       "france","pti","international", ## international might drop content, but mostly about desks
                       "cnn", "nbc", "pna",  ## PNA is philippines news agency
                       "hrw",
                       "network", "*sharq*", "watan", ## al-shariqyah; al-Sharq Al Aswat
                       "*jazeera*", "*hayat*",
                       "*manar", "*iraqiyah",
                       "dagbladet", "summar*", ## summaries
                       "data*", ##database, datasheet
                       "programm*","post-dispatch",
                       "all-india", "satp",## satp = southeast asian terrorism portal
                       "meib", "mipt", "*diyar",
                       "london-based","trt", "irna",
                       "allafrica", "globe", "mail",
                       "shabelle", "kanti*", ##somali and  nepalese news service
                       "broadcast*","corpor*",
                       "say", "said", "paper",
                       "analy*", "insight", "review",
                       "interfax", "tribune",
                       "info*", "inquirer",
                       "yearbook", "satellite*") 

    markup <-c("www*", "http*", "website", ##markeup and website words
               "english", "gmt", "*htm*",
               "*.com","*.net","*.org",
               "updat*", "web*", "site*",
               "right*", "reserv*", "fm",
               "timelin*", "access*", "correct*",
               "copyright*", "permiss*",
               "repro*", "material", "except")## copyright notices

    languages <- c("arabic", "english")
    months <- c("jan*", "*feb*", "mar*",
                "april", "may", "*june", "*jul*",
                "aug*", "sept*", "oct*",
                "nov*", "dec*", "month*",
                "yesterday", "today")
    directions <- c("north*", "east*", "south*", "west*")
    
    tmp.dfm <- quanteda::dfm(tmp.corpus,
                             tolower=TRUE,
                             stem=TRUE, ## stemming probably conflates lots of similar stuff
                             remove_url=TRUE,
                             remove_symbols=TRUE,
                             remove=c(stopwords("english"),
                                 news.agencies, markup,
                                 months, directions, languages),
                             remove_numbers=TRUE,
                             remove_punct=TRUE,
                             min_nchar = 2, ## get rid of the one-letter words
                             ngrams=1)
    
    
    tmp.stm <- quanteda::convert(tmp.dfm,
                                 to="stm")
    
    docnames(tmp.dfm) <- tmp.dfm$id
    
    tmp.docs <- tmp.stm$documents
    tmp.vocab <- tmp.stm$vocab
    tmp.meta <- tmp.stm$meta

    ## Break here if the number of words in the vocab is
    ## less than ~10:
    
    if(length(tmp.vocab) <= 10){
        print("Less than 10 words, passing")

        res <- data.frame(
            id = 1, 
            maxtopic = "TS",
            maxvalue = "TS")

        return(outlist=list(model=NULL, results=res))
    }

    kvect=c(2:kmax)
    tmp.stm <- stm::searchK(documents=tmp.docs,
                            vocab=tmp.vocab,
                            data=tmp.meta,
                            seed=6889,
                            prevalence=~s(year), 
                            K=kvect)

    pdf(file=paste0("searchKplotGroup", groupID,".pdf"))
    plot(tmp.stm)
    dev.off()

    print("check plot")
}
