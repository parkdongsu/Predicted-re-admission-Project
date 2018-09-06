path <- 'D:/Dongsu/NLP_Sample/R_CODE/DIC/'

#사전 정보 불러오기############################################
dic1 <-     read.csv(paste(path,'dic_1.csv',sep = ''),stringsAsFactors = FALSE)
dic2_6 <-   read.csv(paste(path,'dic_2~6.csv',sep = ''),stringsAsFactors = FALSE)
dic7_13 <-  read.csv(paste(path,'dic_7~13.csv',sep = ''),stringsAsFactors = FALSE)
dic14 <-    read.csv(paste(path,'dic_14.csv',sep = ''),stringsAsFactors = FALSE)
dic15 <-    read.csv(paste(path,'dic_15.csv',sep = ''),stringsAsFactors = FALSE)
dic16 <-    read.csv(paste(path,'dic_16.csv',sep = ''),stringsAsFactors = FALSE)
dic17 <-    read.csv(paste(path,'dic_17.csv',sep = ''),stringsAsFactors = FALSE)
dic18 <-    read.csv(paste(path,'dic_18.csv',sep = ''),stringsAsFactors = FALSE)
dic19 <-    read.csv(paste(path,'dic_19.csv',sep = ''),stringsAsFactors = FALSE)
dic20 <-    read.csv(paste(path,'dic_20.csv',sep = ''),stringsAsFactors = FALSE)
dic21 <-    read.csv(paste(path,'dic_21.csv',sep = ''),stringsAsFactors = FALSE)
dic22 <-    read.csv(paste(path,'dic_22.csv',sep = ''),stringsAsFactors = FALSE)
dic23 <-    read.csv(paste(path,'dic_23.csv',sep = ''),stringsAsFactors = FALSE)
dic24 <-    read.csv(paste(path,'dic_24.csv',sep = ''),stringsAsFactors = FALSE)
dic25 <-    read.csv(paste(path,'dic_25.csv',sep = ''),stringsAsFactors = FALSE)
dic26 <-    read.csv(paste(path,'dic_26.csv',sep = ''),stringsAsFactors = FALSE)
##############################################################

dictionary <- c(dic1$x,dic2_6$x,dic7_13$x,dic14$x,dic15$x,dic16$x,dic17$x,dic18$x,dic19$x,dic20$x,dic21$x,dic22$x,dic23$x,dic24$x,dic25$x,dic26$x)

dictionary <- unique(dictionary)
dictionary_part <- rep('nc',length(dictionary))
dictionary_df <- data.frame(dic = c(dictionary), nc = c(dictionary_part),stringsAsFactors = FALSE)



#사전 생성
dictionary_eng <-read.table('D:/Dongsu/NLP_Sample/R_CODE/DIC/dic_english.txt',stringsAsFactors = FALSE)
dictionary_eng <- dictionary_eng$V1
#소문자로 바꿔줌. 
dictionary_eng <- tolower(dictionary_eng)




