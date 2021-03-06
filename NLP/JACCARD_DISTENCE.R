#한글 hunspell (형태소 단위로 나눔) -> levenshtein(편집거리 1설정)
#합쳐져 있는 것.


#한글

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

#영어
library(progress)

#사전 생성
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#소문자로 바꿔줌. 
dictionary_eng <- tolower(dictionary_eng)

#진단서의 모든 영어 단어 추출(한글 제거)
eng_tmp_word <- c()
eng_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    only_eng <-gsub('[가-힣]','',strsplit(doc.df$NOTE_TEXT[i],' ')[[1]])
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

#사전에 있으면 워드 제거 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    exist_tmp_word <- dictionary_eng[which(dictionary_eng == eng_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)



#오탈자 찾기.
tmp_similar_word_eng <-c()
similar_word_eng <- c()
pb <- progress_bar$new(total=length(eng_word))
for(i in 1:length(eng_word)){
    pb$tick()
    word <- eng_word[i]

    if (nchar(word)>2){ # 3글자 이상만 찾음.
        
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



#사전 생성
dictionary #dictionary. R 파일에 있음

#진단서의 모든 한글 단어 추출(영어 제거)
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

#사전에 있으면 워드 제거 
exist_word <- c()
exist_tmp_word <- c()
pb <- progress_bar$new(total=length(kor_word))
for(i in 1:length(kor_word)){
    pb$tick()
    exist_tmp_word <- dictionary[which(dictionary == kor_word[i])]
    exist_word <- c(exist_word,exist_tmp_word)
}
eng_word <- setdiff(eng_word,exist_word)


#오탈자 찾기







N_gram_1 <- doc.df$NOTE_TEXT
N_gram_2 <- doc.df$NOTE_TEXT2
N_gram_3 <- doc.df$NOTE_TEXT3

lapply(N_gram1,MACTHING)

#dictionary에서 단어 하나씩 가져와서 정규표현식 안에 넣어서 ex) grexper('[',dictionary[i],']',N_gram1) grexper <- 이건 예시임 딴거일 수 있음.
#TRUE면 워드 창고에 저장.ㅎㅎㅎ



#데이터 프레임에서 n1,n2,n3일때 기준 나눠서 사전에 있는 단어로 검색했을때 나오는 단어 -> 거기서 단어 또 분리해야되고 안쉬울듯
#띄어쓰기 단위로 나눠서 정규표현식을 통해 포함된다면 그 단어를 토픽으로 가져오자.

for(i in 1:nrow(doc.df)){
    N_gram_1_list <- strsplit(N_gram_1[i],' ')[[1]]
    
    if(length(N_gram_1_list) > 0){
        for(k in 1:length(N_gram_1_list)){
            N_gram_1_list[k]
        }
        
    }
}

#아예 문자가 없을떄는 ㅣㅈ우자.












grep('합병증',N_gram_1_word_list)



