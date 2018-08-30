#ÇÑ±Û hunspell (ÇüÅÂ¼Ò ´ÜÀ§·Î ³ª´®) -> levenshtein(ÆíÁı°Å¸® 1¼³Á¤)
#ÇÕÃÄÁ® ÀÖ´Â °Í.


#ÇÑ±Û

library(progress)

tmp_similar_word <-c()
similar_word <- c()
val = 1

#pb <- progress_bar$new(total=(nrow(doc.df)))
for(i in 1:nrow(doc.df)){
    #pb$tick()
    text_length <- length(strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
    if(text_length != 0){
        for(k in 1:text_length){
            
            word <- strsplit(doc.df$NOTE_TEXT[i],' ')[[1]][k]
            
            if (nchar(word)>2){
                
                levenshtein_list <- RecordLinkage::levenshteinDist(word,dictionary)
                
                word_distence <- min(levenshtein_list)
                
                if(length(which(levenshtein_list == 1)) != 0){
                    tmp_similar_word <- c(tmp_similar_word,paste(word,':',dictionary[which(levenshtein_list == 1)]))
                    
                }
            }
        }
        
        if(i == 10000*val){
            similar_word <-  c(similar_word,tmp_similar_word)
            tmp_similar_word <- c()
            val = val + 1 
        }
    }
    
}

#¿µ¾î
library(progress)

#»çÀü »ı¼º
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#¼Ò¹®ÀÚ·Î ¹Ù²ãÁÜ. 
dictionary_eng <- tolower(dictionary_eng)

#Áø´Ü¼­ÀÇ ¸ğµç ¿µ¾î ´Ü¾î ÃßÃâ(ÇÑ±Û Á¦°Å)
eng_tmp_word <- c()
eng_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_eng <-gsub('[°¡-ÆR]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
    only_eng <-unique(only_eng)
    only_eng <- only_eng[-which(only_eng == "")]
    

    eng_tmp_word <- c(eng_tmp_word,only_eng)
    
    if(i%%100 == 0){
        eng_word <- c(eng_word,eng_tmp_word)
        eng_tmp_word <- c()
    }
}
eng_word <- c(eng_word,eng_tmp_word)
eng_word <- unique(eng_word)
length(eng_word)

#»çÀü¿¡ ÀÖÀ¸¸é ¿öµå Á¦°Å 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    exist_tmp_word <- dictionary_eng[which(dictionary_eng == eng_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)



#¿ÀÅ»ÀÚ Ã£±â.
tmp_similar_word_eng <-c()
similar_word_eng <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    word <- eng_word[i]

    if (nchar(word)>2){ # 3±ÛÀÚ ÀÌ»ó¸¸ Ã£À½.
        
    levenshtein_eng_list <- RecordLinkage::levenshteinDist(word,dictionary_eng)
    
    word_distence <- min(levenshtein_eng_list)
    
    if(length(which(levenshtein_eng_list == 1)) != 0){
        tmp_similar_word_eng <- c(tmp_similar_word_eng,paste(word,':',dictionary_eng[which(levenshtein_eng_list == 1)]))
    }
    }
    
    if(i%%100 == 0){
        similar_word_eng <- c(similar_word_eng,tmp_similar_word_eng)
        tmp_similar_word_eng <- c()
    }
    
}
similar_word_eng <- c(similar_word_eng,tmp_similar_word_eng)

length(similar_word_eng)


write.csv(similar_word_eng,'D:/levenshtein.csv')

similar_word_eng_df <- data.frame(similar_word_eng,stringsAsFactors = FALSE)



library(Rcpp)
library(digest)



#»çÀü »ı¼º
dictionary #dictionary. R ÆÄÀÏ¿¡ ÀÖÀ½

#Áø´Ü¼­ÀÇ ¸ğµç ÇÑ±Û ´Ü¾î ÃßÃâ(¿µ¾î Á¦°Å)
kor_tmp_word <- c()
kor_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_kor <-gsub('[a-zA-Z]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
    only_kor <-unique(only_kor)
    only_kor <- only_kor[-which(only_kor == "")]
    
    
    kor_tmp_word <- c(kor_tmp_word,only_kor)
    
    if(i%%100 == 0){
        kor_word <- c(kor_word,kor_tmp_word)
        kor_tmp_word <- c()
    }
}
kor_word <- c(kor_word,kor_tmp_word)
kor_word <- unique(kor_word)

#»çÀü¿¡ ÀÖÀ¸¸é ¿öµå Á¦°Å 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(kor_word))
for(i in 1:length(kor_word)){
    pb$tick()
    exist_tmp_word <- dictionary[which(dictionary == kor_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)


#¿ÀÅ»ÀÚ Ã£±â







N_gram_1 <- doc.df$NOTE_TEXT
N_gram_2 <- doc.df$NOTE_TEXT2
N_gram_3 <- doc.df$NOTE_TEXT3

lapply(N_gram1,MACTHING)

#dictionary¿¡¼­ ´Ü¾î ÇÏ³ª¾¿ °¡Á®¿Í¼­ Á¤±ÔÇ¥Çö½Ä ¾È¿¡ ³Ö¾î¼­ ex) grexper('[',dictionary[i],']',N_gram1) grexper <- ÀÌ°Ç ¿¹½ÃÀÓ µı°ÅÀÏ ¼ö ÀÖÀ½.
#TRUE¸é ¿öµå Ã¢°í¿¡ ÀúÀå.¤¾¤¾¤¾



#µ¥ÀÌÅÍ ÇÁ·¹ÀÓ¿¡¼­ n1,n2,n3ÀÏ¶§ ±âÁØ ³ª´²¼­ »çÀü¿¡ ÀÖ´Â ´Ü¾î·Î °Ë»öÇßÀ»¶§ ³ª¿À´Â ´Ü¾î -> °Å±â¼­ ´Ü¾î ¶Ç ºĞ¸®ÇØ¾ßµÇ°í ¾È½¬¿ïµí
#¶ç¾î¾²±â ´ÜÀ§·Î ³ª´²¼­ Á¤±ÔÇ¥Çö½ÄÀ» ÅëÇØ Æ÷ÇÔµÈ´Ù¸é ±× ´Ü¾î¸¦ ÅäÇÈÀ¸·Î °¡Á®¿ÀÀÚ.

for(i in 1:nrow(doc.df)){
    N_gram_1_list <- strsplit(N_gram_1[i],' ')[[1]]
    
    if(length(N_gram_1_list) > 0){
        for(k in 1:length(N_gram_1_list)){
            N_gram_1_list[k]
        }
        
    }
}

#¾Æ¿¹ ¹®ÀÚ°¡ ¾øÀ»‹š´Â ¤Ó¤¸¿ìÀÚ.












grep('ÇÕº´Áõ',N_gram_1_word_list)



