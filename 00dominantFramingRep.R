
## Wrapper around topic model x yearly topic proportion:

dominantFraming <- function(groupID, ucdpdata){

    ## clip out the data about the group:
    dat <- ucdpdata[which(ucdpdata$side_b_new_id==groupID|
                          ucdpdata$side_a_new_id == groupID),]

    ## K=2 topic model; dominant topic in each article
    tst <- article.analysis(dat)

    ## sink group ID to debug (so I can get back faster)
    sink("process.txt",append=TRUE)
    print(groupID)
    sink()

    print(head(tst$results))

  
    ## The fallthrough:
    if(dim(tst$results)[1]==1){ ## if only one row in results
        ## make a record:
        sink("too-short.txt", append=TRUE)           
        print(paste0("10 or fewer words for group ", groupID))
        sink()
        outlist <- list(yearsum=NULL, basedata=NULL)
        return(outlist)
      }

    ## Merge dominant topic into the data:
    ## dat is the basedata
    dat <- merge(dat,
                 tst$results,
                 by.x="id",
                 by.y="id",
                 all.x=TRUE)

    ## summarize by max topic:

    print(head(dat))
    
    dat$factor <- as.factor(dat$factor)
    tmp3 <- dat %>%
        group_by(year) %>%
            summarize(
                countT1 = sum(factor=="T1", na.rm = T),
                countT2 =  sum(factor=="T2", na.rm = T))

    ## tmp3 becomesm "yearsum"
    
    tmp3$propT1 <- round(tmp3$countT1/
                         (tmp3$countT1+tmp3$countT2), 2)
    tmp3$propT2 <- round(tmp3$countT2/
                         (tmp3$countT1+tmp3$countT2), 2)
    
    ## diff in proportions:
    ## Proportion 2 - proportion 1
    ## thus negative implies more T1; positive implies more T2
    tmp3$propdiff <- tmp3$propT2-tmp3$propT1
    ## lagged:
    
    tmp3 <- tmp3 %>% arrange(year) %>%
        mutate(propdif.L1 = propdiff - lag(propdiff))
    
    tmp3$groupID <- groupID
    
    ## put words in for year-dominant topic
    topics <- labelTopics(tst$model, topics=c(1,2), n=10)

    t1frex <-  paste0(topics$frex[1,], collapse=",")
    t2frex <-  paste0(topics$frex[2,], collapse=",")

    ##print(t1frex)
    ##print(t2frex)

    ##FREX Words into basedata (so that I'll have full coverage):
    ## for groups that have only one topic dominate at the year level
    
    dat[which(dat$maxtopic==1), "frexWords"] <- t1frex
    dat[which(dat$maxtopic==2), "frexWords"] <-t2frex
    dat[is.na(dat$maxtopic), "frexWords"] <- "NA"

    ##FREX Words into yearsum:   
    if(tmp3$countT1==tmp3$countT2){
        tmp3$frexWords <- "Both"
    }else{
        tmp3[which(tmp3$countT1 > tmp3$countT2),
             "frexWords"] <- t1frex
        tmp3[which(tmp3$countT1 < tmp3$countT2),
             "frexWords"] <-  t2frex
    }

    
    outlist <- list(yearsum=tmp3, basedata=dat)
    return(outlist)
}
