library(httr)
library(rvest)
library(progress)



#ÃÖÁ¾»çÀü ÀúÀå º¤ÅÍ
dic <- c()


#°Ë»ö Á¶°Ç
alpha <- c('a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t','u','v','w','x','y','z')

#°íÁ¤ URL
url1 <- 'https://term.kma.org/search/list.asp?pageno='
url3 <- '&sch_txt='
url5 <- '&sch_type1=twdContent6&sch_type2=twdDong6'


for(alphabet in 14:14){
    
    url2 <- '1'
    url4 <- alpha[alphabet]
    
    #url paste
    url <- paste(url1,url2,url3,url4,url5,sep='')
    url
    nv<-read_html(url)
    nvns<-html_nodes(nv, "p.total")
    title<-html_text(nvns)
    num <- as.integer(gsub('[°¡-ÆR .]*','',title))
    page_value <- as.integer(num/15)+1
    
    
    pb <- progress_bar$new(total=page_value)
    for(count in 1:page_value){
        pb$tick()
        
        url2 <- count
        
        url <- paste(url1,url2,url3,url4,url5,sep='')
        
        nv<-read_html(url)
        nvns<-html_nodes(nv, "dl.searchList")
        title<-html_text(nvns)
        
        title <- stringr::str_replace_all(title,"[[:space:]]{1,}","")
        title <- stringr::str_replace_all(title,"\\([°¡-ÆR]\\)","")
        title <- stringr::str_replace_all(title,"[^°¡-ÆR]*"," ")
        title <- stringr::str_replace_all(title,"[ ]{2}",",")
        title <- stringr::str_replace_all(title,"[ ]{1}","")
        
        title <- strsplit(title,",")[[1]][-1]

        
        dic <- c(dic,title)

        
    }
    dic_list[[alphabet]] <- c(dic)
    path <- paste('D:/Dongsu/R_code/sql/dic_',alphabet,'.csv',sep='')
    write.csv(unlist(dic_list[[alphabet]]),path)
    dic = c()

}







