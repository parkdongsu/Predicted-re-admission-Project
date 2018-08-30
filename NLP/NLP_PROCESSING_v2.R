#1.°ø¶õÃ³¸® -> /n /t, 2Ä­¶³¾îÁø°Í ¸ğµÎ ÇÑÄ­À¸·Î ¹Ù²ãÁÜ
#2.´ë¼Ò¹®ÀÚ ÅëÀÏ -> ÀÇ´ë»ıºĞ°ú »óÀÇ ÈÄ ÀÛ¾÷
#3.¼ıÀÚÇ¥Çö -> ¼ıÀÚµéÀ» ¸ğµÎ _number_·Î ¹Ù²ã ÀÎ½Ä or ¼ıÀÚ ±×´ë·Î ÀÎ½Ä or ¼ıÀÚ¿Í µÚÀÇ ´Ü¾î¸¦ ÇÕÃÄ ÀúÀå ex) 3È¸, 3¹ø µî ¾î¶»°Ô ÇÒ °ÍÀÎÁö
#4.¹®ÀåºÎÈ£ ¹× Æ¯¼ö¹®ÀÚ Á¦°Å -> Æ¯ÀÌ ÄÉÀÌ½º¸¸ ºĞ·ùÇØ ÇÕÃÄÁà ÀúÀå.  ¸¸¾à ±×·¸´Ù¸é ÀÇ´ë»ıºĞ°ú ÀÛ¾÷, ¾Æ´Ï¶ó¸é ¾î¶»°Ô ÇÒ °ÍÀÎÁö
#5.ºÒ¿ë´Ü¾î Á¦°Å ->·ÎÁ÷ »ı°¢ÇØ¼­ Àû¿ë ÇÑ±ÛÀº ÇÊ¿ä¾ø´Â Ç°»ç Á¦°Å / (¸í»ç´Â ¸í»ç·Î ³²±â°í ´Ù¸¥ Ç°»ç´Â ¶Ç ¾î¶»°Ô Ã³¸®ÇÒ°ÍÀÎÁö °í¹Î) 
#6.¾î±Ù µ¿ÀÏÈ­ Ã³¸® -> ÇÑ±ÛÀº Àû¿ëX,  ¿µ¾î´Â ¾î¶»°Ô ÇÒ°ÍÀÎÁö
#7.¿£±×·¥(n¹ø ¿¬ÀÌ¾î µîÀåÇÏ´Â ´Ü¾îµéÀÇ ¿¬¼â) ->ÇÏ³ªÀÇ ÀÇÇĞ¿ë¾î´Â ºÙ¿©¾ßÇÏ´ÂÁö µû·Î ÇÒ °ÍÀÎÁö ºÙÀÎ´Ù¸é ÀÇ´ë»ıºĞ°ú »óÀÇÈÄ ÀÛ¾÷ #¹æ¹ıÀº ÁöÁ¤µÈ ¿µ¾î¹®ÀÚ·Î
#º¯°æÈÄ ex) Unique_word1 ³ªÁß¿¡ POS_EXTRACTIONÇÔ¼ö ½ÇÇàÇÏ¸é ¿µ¾î´Ï±î ±×´ë·Î ³ª¿È ±× ÈÄ list¿¡¼­ Ã£¾Æ¼­ ´Ù½Ã ¹Ù²ãÁÜ 
#8.¿µ¾î Á¢µÎ»ç, Á¢¹Ì»ç Á¦°ÅÇØ¾ßÇÒÁö?  ¸Â´Ù¸é ±×°Í¸¸ ÇÏ¸é µÇ´ÂÁö # ÀÏ´Ü ³¡³ª°í ÇØ¾ßÇÔ ÁöÁ¤ ¿ë¾î¿Ü¿£ ´Ü¾î Á¤¸³ÈÄ ¾ÕµÚ·Î ¶¿Áö??

#ubuntu È¯°æ¿¡¼­ topicmodels install ½Ã ctm.c:29:25: fatal error: gsl/gsl_rng.h: No such file or directory ¿À·ù ³ª¿À¸é
#sudo apt-get install libgsl0-dev ·Î ÇØ°á

#»çÀü ÁØºñ»çÇ×########################################
#XML_Parsing_Pro7¿¡¼­ file=""¿¡ RDS ÆÄÀÏ °æ·Î¸¦ ½áÁÖ°í ½ÇÇàÇÑ ÈÄ ½ÇÇà ÇÒ °Í.
#C:\Program Files\R\R-3.5.1\library\base\R\RProfile¿¡ options(java.parameters = c("-Xmx16384m","-Dfile.encoding=UTF-8")) Ãß°¡ # KoNLP¿¡·¯ ¹æÁö 
#options(java.parameters = c("-Xmx16384m","-Dfile.encoding=UTF-8"))
#options("java.parameters")$java.parameters
######################################################

#POS ÃßÃâ ÇÔ¼ö
K_POS_EXTRACTION <- function(wordlist){
    #5.ºÒ¿ë ´Ü¾î Á¦°Å
    wordlist <- gsub('/F+','/CW+',wordlist)
    wordlist <- gsub('/NC+','/CW+',wordlist)
    
    pos_start <- as.vector(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]]) # Á¤±ÔÇ¥Çö½ÄÀ» ÅëÇØ °É·¯¼­ »ç¿ë 
    pos_length <- as.vector(attr(gregexpr('[^+]+\\/CW[+]',wordlist)[[1]],'match.length'))
    
    pos_end <- pos_start+pos_length-5
    
    word_data = rep(NA,length(pos_start))
    word <- c()
    for(i in 1:length(pos_start)){
        word_data[i] <- substr(wordlist,pos_start[i],pos_end[i])
        word <- paste(word,word_data[i])
    }
    word <- substr(word,2,nchar(word))
    
    return(word)
}

#¹®ÀåÁß Á¦°ÅÇÒ ºÎºĞÀ» ¼±Ã³¸®ÇØÁÖ´Â ÇÔ¼ö  
NLP_PROCESSING <- function(xmldf){
    #4.Æ¯¼ö ¹®ÀÚ º¯°æ ¹× Á¦°Å
    xmldf <- gsub('&#x0D;', " ", xmldf) # ¶ç¾î¾²±â´Â ¸¶Áö¸·¿¡ ¹«Á¶°Ç ÇÏ³ª·Î ÅëÀÏ ÇØÁÖ´Â ºÎºĞÀÌ ÀÖÀ½. 
    xmldf <- gsub('&lt;', " ", xmldf)
    xmldf <- gsub('&gt;', " ", xmldf)
    xmldf <- gsub('&amp;', " ", xmldf)
    xmldf <- gsub('&quot;', " ", xmldf)
    
    xmldf <- gsub("[\\]","", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("[\\+]|[\\{]|[\\}]|[\\(]|[\\)]|[\\<]|[\\>]"," ", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("\\[","", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("\\]","", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("\\/","", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("\\'"," ", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub('\\"'," ", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    xmldf <- gsub("[~!@#$><%¡Ã=^&¡¿*-:¡Ü¡Ú¢´]"," ", xmldf)#Æ¯¼ö¹®ÀÚ Á¦°Å
    
    xmldf <-xmldf <- gsub(',', " ", xmldf) # ÄŞ¸¶´Â ÇÑÄ­ ¶³¾î¶ß·ÁÁÜ.
    
    #2.´ë¼Ò¹®ÀÚ ÅëÀÏ(¼±ÅÃ°¡´ÉÀ¸·Î ¸¸µé °Í)
    #xmldf<- toupper(xmldf) # ´ë¹®ÀÚ 
    xmldf<- tolower(xmldf)# ¼Ò¹®ÀÚ
    
    xmldf <- gsub('[¤¿-¤Ó]*','',xmldf)
    xmldf <- gsub('[¤¡-¤¾]*','',xmldf)
    
    #6.¾î±Ù µ¿ÀÏÈ­ Ã³¸®
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' are ',' be ',xmldf)
    #xmldf <- gsub(' is ',' be ',xmldf)
    
    #¶Ç´Â Áß¿äÇÏÁö¾ÊÀº ´Ü¾îµéÀ» »©°í ½Í´Ù
    #xmldf <- gsub('and|of|as|in',"",xmldf)# ¼±Ã³¸® ÈÄ °ø¶õ Ã³¸® ÇÒ °Í.
    
    #7.¿£±×·¥
    #xmldf <- sub('[^A-Za-z °¡-ÆR]*graphic[ _-]variant[^A-Za-z °¡-ÆR]*','graphicvariant',xmldf) # KoNLP Ã³¸®½Ã ¿µ¾î¹®ÀåÀº ±×´ë·Î ³ª¿À±â ¶§¹®¿¡ ÇÑ´Ü¾î·Î ¹Ù·Î ³ª¿È.
    
    #ÇÑ±Û, ¿µ¾î°¡ ºÙ¾îÀÖ´Â °æ¿ì¿¡ ¶³¾î¶ß·ÁÁÜ.
    pos_start <- as.vector(gregexpr('[^°¡-ÆR ]*[A-Za-z]+[^°¡-ÆR ]*',xmldf)[[1]]) # Á¤±ÔÇ¥Çö½ÄÀ» ÅëÇØ °É·¯¼­ »ç¿ë 
    pos_length <- as.vector(attr(gregexpr('[^°¡-ÆR ]*[A-Za-z]+[^°¡-ÆR ]*',xmldf)[[1]],'match.length'))
    pos_end <- pos_start+pos_length-1
    
    word_data <- c()
    if(length(pos_start) > 0){  
        for(i in 1:length(pos_start)){
            word_data[i] <- substr(xmldf,pos_start[i],pos_end[i])
        }
        
        new_word_data <- paste("",toupper(word_data),"")
        
        for(i in 1:length(word_data)){
            xmldf <- sub(word_data[i],new_word_data[i],xmldf)
        }
    }
    xmldf<- tolower(xmldf)# ´Ù½Ã ¼Ò¹®ÀÚ Ã³¸®¸¦ ÇØÁÜ.
    
    
    #1.°ø¶õÃ³¸®
    xmldf <- stringr::str_replace_all(xmldf,"[[:space:]]{1,}"," ")# ÇÑÄ­ÀÌ»óÀÇ ¶ç¾î¾²±â¸¦ ÇÑÄ­À¸·Î ÅëÀÏ
    
    #¹Ù²Ü Çü½Ä
    xmldf <- paste(xmldf,'.',sep = '')#¹®ÀåÀÌ ¾Æ´Ñ °æ¿ì ³¯Â¥, ¾à¸í µî Áß¿äÇÑ Á¤º¸°¡ Àß¸®´Â °æ¿ì°¡ ÀÖÀ½. ex) 12-02-02 ´Ü¾î ÇÏ³ªÀÖÀ¸¸é 12-02-0 °ú 2·Î ³ª´¸.
    #¾îÂ÷ÇÇ ¸¶Áö¸· . Ãß°¡ÇØÁÖ¸é µû·Î ³ª´²Áö°í Á¤±ÔÇ¥Çö½Ä¿¡¼­ °Å¸£Áö ¾ÊÀ¸´Ï ±¦ÂúÀ»°Å¶ó°í »ı°¢ÇÔ.
    
    return(xmldf)
}
#Ç°»ç ºĞ¼®ºÎ
POS_ANALYSIS <- function(word_df){
    word_list <- KoNLP::SimplePos22(word_df)
    if(length(word_list) ==1){
        word_vector <- word_list[[1]]
        result_word_list <- c(word_vector)
    } 
    else{
        word_vector <- word_list[[1]]
        for (k in 2:length(word_list)){
            word_vector <- paste(word_vector,'+',word_list[[k]],sep = '')
        }
        result_word_list <- c(word_vector)
    }
    return(result_word_list)
}


# load packages
if(!require(rJava)) {
    install.packages('rJava')
}
if(!require(KoNLP)) {
    install.packages('KoNLP')
}
if(!require(devtools)) {
    install.packages('devtools')
}
#library(devtools)
#install_github('haven-jeon/NIADic/NIAdic', build_vignettes = TRUE)
if(!require(topicmodels)) {
    install.packages('topicmodels')
}
if(!require(openNLP)) {
    install.packages('openNLP')
}
if(!require(NLP)) {
    install.packages('NLP')
}
if(!require(parallel)) {
    install.packages("parallel")
}

Sys.setenv(JAVA_HOME="C:\\Program Files\\Java/jdk1.8.0_171") 
library(KoNLP)
library(rJava)
library(topicmodels)
library(stringr)
library(parallel)

# ÄÚ¾î °³¼ö È¹µæ
numCores <- parallel::detectCores() - 1
# Å¬·¯½ºÅÍ ÃÊ±âÈ­
myCluster <- parallel::makeCluster(numCores)

useSejongDic()
#mergeUserDic(dictionary_df)
#mergeUserDic(zz)
########MAIN CODE##############################################################



search_df <- result_xml_df[result_xml_df$`<MN>`=='Çöº´·Â',] # ÅÂ±×, °Ë»ö¾î ÁöÁ¤ ex) <MN>, '¾à¸í'

tag ='<TD>' # NLP Ã³¸®ÇÏ°í ½ÍÀº tag ÀÔ·Â

#°Ë»ö ÈÄ tag°¡ NAÀÎ ÇàÀ» »èÁ¦
search_df[,tag][is.na(search_df[,tag])] <- ""

for (i in nrow(search_df):1){# µÚ¿¡¼­ºÎÅÍ »èÁ¦ÇØ ÇàÀÌ ¹Ğ·Á¼­ »èÁ¦ µÇÁö ¾Êµµ·Ï Ã³¸®ÇÔ.
    if(search_df[i,tag] == ""){
        search_df <- search_df[-i,]
    }
}

#NLP ¿ë df ¸¸µé±â
xml_df <- search_df[tag]

#NLP_PROCESSING ÇÔ¼ö¸¦ ÅëÇÑ ÃÊ±â ¼³Á¤(º´·ÄÃ³¸®)
word_df <- as.data.frame(parApply(myCluster,xml_df,1,NLP_PROCESSING))

#ÇüÅÂ¼Ò ºĞ¼®ÈÄ ÇÕÄ¡±â
result_word_list <- apply(word_df,1,POS_ANALYSIS)
#result_word_list <- parApply(myCluster,word_df,1,POS_ANALYSIS)
result_word_list<- unlist(result_word_list)

#¿øÇÏ´Â Ç°»ç ÃßÃâ ÈÄ ÇÏ³ªÀÇ ¹®ÀåÀ¸·Î ÇÕÃÄÁÜ. (º´·ÄÃ³¸®)
doc.list <- parallel::parLapply(myCluster,result_word_list,K_POS_EXTRACTION)

#DF ¿¡ ÀúÀå 
doc.tmp_df <- data.frame(unlist(doc.list),stringsAsFactors = FALSE)
doc.df <- data.frame(c(search_df['NOTE_ID'],doc.tmp_df,search_df['outcomeCount']),stringsAsFactors = FALSE)

#colname ÁöÁ¤
colnames(doc.df) <- colnames(doc.df) <- c('NOTE_ID','NOTE_TEXT','outcomeCount')


# Å¬·¯½ºÅÍ ÁßÁö
parallel::stopCluster(myCluster)