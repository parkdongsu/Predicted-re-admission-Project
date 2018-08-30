#텍스트만 가져옴.
engram_list <- doc.df[,2]

#engrme (n =2) 설정
engram2_list = vector('list',length(engram_list))
for(i in 1:length(engram_list)){
    #짧은 문항은 따로 else 로 설정해서 처리 
    if (nchar(gsub("[^ ]",'',engram_list[[i]])) >= 3){
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # 정규표현식을 통해 걸러서 사용 
        en_gram2 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[2]-1)))
        
        for(k in 1:(length(pos_start)-2)){
            n2word <- gsub(' ','',substr(engram_list[[i]],(pos_start[k]+1),(pos_start[k+2]-1)))
            en_gram2 <- paste(en_gram2,n2word)    
        }
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-1]+1),nchar(engram_list[[i]])))
        engram2_list[[i]] <- paste(en_gram2,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 2){
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # 정규표현식을 통해 걸러서 사용
        en_gram2 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[2]-1)))
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-1]+1),nchar(engram_list[[i]])))
        engram2_list[[i]] <- paste(en_gram2,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 1){
        engram2_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
    else{
        #없는 경우에는 어떻게 할까?
        engram2_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
}
#engrme (n =3) 설정
engram3_list = vector('list',length(engram_list))
for(i in 1:length(engram_list)){
    #짧은 문항은 따로 else 로 설정해서 처리 
    if (nchar(gsub("[^ ]",'',engram_list[[i]])) >= 4){
    pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # 정규표현식을 통해 걸러서 사용 
    en_gram3 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[3]-1)))
    
    for(k in 1:(length(pos_start)-3)){
        n3word <- gsub(' ','',substr(engram_list[[i]],(pos_start[k]+1),(pos_start[k+3]-1)))
        en_gram3 <- paste(en_gram3,n3word)
    }
    last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-2]+1),nchar(engram_list[[i]])))
    engram3_list[[i]] <- paste(en_gram3,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 3){
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # 정규표현식을 통해 걸러서 사용
        en_gram3 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[3]-1)))
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-2]+1),nchar(engram_list[[i]])))
        engram3_list[[i]] <- paste(en_gram3,last_sentence)
        
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 2){
        engram3_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
    else{
        #없는 경우에는 어떻게 할까?
        engram3_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
}

doc.df <- cbind(doc.df,unlist(engram2_list))
doc.df <- cbind(doc.df,unlist(engram3_list))

doc.df <- doc.df[,c(1,2,4,5,3)]
colnames(doc.df) <- c('NOTE_ID','NOTE_TEXT','NOTE_TEXT_N2','NOTE_TEXT_N3','outcomeCount')

