#병원 용어만 나와야함. 


#진단서의 모든 단어 추출
all_tmp_word <- c()
all_word <- c()
pb <- progress_bar$new(total=nrow(doc.df))
for(i in 1:nrow(doc.df)){
    pb$tick()
    word_piece <-strsplit(doc.df$NOTE_TEXT[i],' ')[[1]]
    
    all_tmp_word <- c(all_tmp_word,word_piece)
    
    if(i%%100 == 0){
        all_word <- c(all_word,all_tmp_word)
        all_tmp_word <- c()
    }
}

table_all_word <- table(all_word)

all_word_df <-data.frame(table_all_word,stringsAsFactors = FALSE)
colnames(all_word_df) <- c('word','freq')

dictionary_df <- data.frame(word = c(dictionary),stringsAsFactors = FALSE)
dic_han_df <- merge(dictionary_df,all_word_df,by.x = 'word')

dictionary_df_eng <- data.frame(word = c(dictionary_eng),stringsAsFactors = FALSE)
dic_eng_df <- merge(dictionary_df_eng,all_word_df,by.x = 'word')

wordcloud_no <- rbind(dic_han_df,dic_eng_df)

wordcloud_real <- wordcloud_no[wordcloud_no$freq < 10000,]

for(i in nrow(wordcloud_real):1){
    if (nchar(wordcloud_real[i,1]) == 1){
        wordcloud_real <- wordcloud_real[-i,]
    }
}


z <- wordcloud_real[1:5,]
z[-1,]

write.csv(wordcloud_real,'D:/wordcloud.csv',row.names = F)
