#사전 정보 불러오기############################################
dic1 <- read.csv('D:/Dongsu/R_code/sql/dic_1.csv',stringsAsFactors = FALSE)
dic2_6 <- read.csv('D:/Dongsu/R_code/sql/dic_2~6.csv',stringsAsFactors = FALSE)
dic7_13 <- read.csv('D:/Dongsu/R_code/sql/dic_7~13.csv',stringsAsFactors = FALSE)
dic14 <- read.csv('D:/Dongsu/R_code/sql/dic_14.csv',stringsAsFactors = FALSE)
dic15 <- read.csv('D:/Dongsu/R_code/sql/dic_15.csv',stringsAsFactors = FALSE)
dic16 <- read.csv('D:/Dongsu/R_code/sql/dic_16.csv',stringsAsFactors = FALSE)
dic17 <- read.csv('D:/Dongsu/R_code/sql/dic_17.csv',stringsAsFactors = FALSE)
dic18 <- read.csv('D:/Dongsu/R_code/sql/dic_18.csv',stringsAsFactors = FALSE)
dic19 <- read.csv('D:/Dongsu/R_code/sql/dic_19.csv',stringsAsFactors = FALSE)
dic20 <- read.csv('D:/Dongsu/R_code/sql/dic_20.csv',stringsAsFactors = FALSE)
dic21 <- read.csv('D:/Dongsu/R_code/sql/dic_21.csv',stringsAsFactors = FALSE)
dic22 <- read.csv('D:/Dongsu/R_code/sql/dic_22.csv',stringsAsFactors = FALSE)
dic23 <- read.csv('D:/Dongsu/R_code/sql/dic_23.csv',stringsAsFactors = FALSE)
dic24 <- read.csv('D:/Dongsu/R_code/sql/dic_24.csv',stringsAsFactors = FALSE)
dic25 <- read.csv('D:/Dongsu/R_code/sql/dic_25.csv',stringsAsFactors = FALSE)
dic26 <- read.csv('D:/Dongsu/R_code/sql/dic_26.csv',stringsAsFactors = FALSE)
##############################################################

dictionary <- c(dic1$x,dic2_6$x,dic7_13$x,dic14$x,dic15$x,dic16$x,dic17$x,dic18$x,dic19$x,dic20$x,dic21$x,dic22$x,dic23$x,dic24$x,dic25$x,dic26$x)

dictionary <- unique(dictionary)
dictionary_part <- rep('nc',length(dictionary))
dictionary_df <- data.frame(dic = c(dictionary), nc = c(dictionary_part),stringsAsFactors = FALSE)


useSejongDic()

zz<- data.frame(c(dictionary),c('nc'))


mergeUserDic(zz)









