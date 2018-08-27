#�ؽ�Ʈ�� ������.
engram_list <- doc.df[,2]

#engrme (n =2) ����
engram2_list = vector('list',length(engram_list))
for(i in 1:length(engram_list)){
    #ª�� ������ ���� else �� �����ؼ� ó�� 
    if (nchar(gsub("[^ ]",'',engram_list[[i]])) >= 3){
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # ����ǥ������ ���� �ɷ��� ��� 
        en_gram2 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[2]-1)))
        
        for(k in 1:(length(pos_start)-2)){
            n2word <- gsub(' ','',substr(engram_list[[i]],(pos_start[k]+1),(pos_start[k+2]-1)))
            en_gram2 <- paste(en_gram2,n2word)    
        }
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-1]+1),nchar(engram_list[[i]])))
        engram2_list[[i]] <- paste(en_gram2,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 2){
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # ����ǥ������ ���� �ɷ��� ���
        en_gram2 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[2]-1)))
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-1]+1),nchar(engram_list[[i]])))
        engram2_list[[i]] <- paste(en_gram2,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 1){
        engram2_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
    else{
        #���� ��쿡�� ��� �ұ�?
        engram2_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
}
#engrme (n =3) ����
engram3_list = vector('list',length(engram_list))
for(i in 1:length(engram_list)){
    #ª�� ������ ���� else �� �����ؼ� ó�� 
    if (nchar(gsub("[^ ]",'',engram_list[[i]])) >= 4){
    pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # ����ǥ������ ���� �ɷ��� ��� 
    en_gram3 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[3]-1)))
    
    for(k in 1:(length(pos_start)-3)){
        n3word <- gsub(' ','',substr(engram_list[[i]],(pos_start[k]+1),(pos_start[k+3]-1)))
        en_gram3 <- paste(en_gram3,n3word)
    }
    last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-2]+1),nchar(engram_list[[i]])))
    engram3_list[[i]] <- paste(en_gram3,last_sentence)
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 3){
        print(i)
        pos_start <- as.vector(gregexpr(' ',engram_list[[i]])[[1]]) # ����ǥ������ ���� �ɷ��� ���
        en_gram3 <- gsub(' ','',substr(engram_list[[i]],1,(pos_start[3]-1)))
        last_sentence <-  gsub(' ','',substr(engram_list[[i]],(pos_start[length(pos_start)-2]+1),nchar(engram_list[[i]])))
        engram3_list[[i]] <- paste(en_gram3,last_sentence)
        
    }
    else if (nchar(gsub("[^ ]",'',engram_list[[i]])) == 2){
        engram3_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
    else{
        #���� ��쿡�� ��� �ұ�?
        engram3_list[[i]] <- gsub(" ",'',engram_list[[i]])
    }
}

doc.df <- cbind(doc.df,unlist(engram2_list))
doc.df <- cbind(doc.df,unlist(engram3_list))

doc.df <- doc.df[,c(1,2,4,5,3)]
colnames(doc.df) <- c('NOTE_ID','NOTE_TEXT','NOTE_TEXT_N2','NOTE_TEXT_N3','outcomeCount')
