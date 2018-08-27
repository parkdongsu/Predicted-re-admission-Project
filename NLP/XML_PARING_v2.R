#사전 준비사항########################################
#PatientLevelPrediction.R 실행 후 실행할 것.
######################################################
library(progress)

#XML_parser -> 정규표현식을 통해 파싱 진행

XML_PARSING <- function(xmlList){
    pattern_start <- as.vector(gregexpr('<[^/<>]+>[^<>]+<\\/[^<>]+>',xmlList)[[1]])
    pattern_length <- as.vector(attr(gregexpr('<[^/<>]+>[^<>]+<\\/[^<>]+>',xmlList)[[1]],'match.length'))
    pattern_end <- pattern_start+pattern_length-1
    
    xml_data = rep(NA,length(pattern_start))
    for(i in 1:length(pattern_start)){
        xml_data[i] <- substr(xmlList,pattern_start[i],pattern_end[i])
    }
    
    return(xml_data)
}

#diag_Processer(맨 아랫단의 태그의 정보만 뽑고 첫 태그부터 다음 첫태그로 구분함.)
#ex) EMR 약명   약명    ##약
#    EMR 진단명 진단명  감기

#파싱하며 각 약명은 ?? 진단명은 ??를 구분하기 위한 함수.
DIAG_PROCESSING <- function(diag_list){
    #첫번째 > 를 기준으로 태그를 추출
    tag_vector  <- as.vector(regexpr('>',diag_list))
    text_vector <- as.vector(regexpr('</',diag_list))
    
    #tag와 text를 구별해 담아줌
    tag_data_vector <- substr(diag_list,1,tag_vector)
    text_data_vector <- substr(diag_list,tag_vector+1,text_vector-1)
    
    #첫번째 태그부터 다음 첫 태그까지 구간 나눠 할당
    first_tag_vector <- as.vector(regexpr(tag_data_vector[1],tag_data_vector))# 첫번째 패턴이 나오면 1로 변환
    
    #1의 위치를 찾아 위치 정보 넣어줌
    data =c()
    for (i in 1:length(first_tag_vector)){
        if (first_tag_vector[i] == 1){
            data[i] <- i
        }
    }
    #NA 제거
    data <- data[!is.na(data)]
    
    #마지막 첫번째 태그의 나머지 값
    data[length(data)+1] <- length(first_tag_vector)+1
    
    #tag를 행이름 설정
    df <- data.frame(stringsAsFactors = FALSE)
    for (i in unique(tag_data_vector)){
        df[i] <- character(0)
    }
    
    #df에 값 넣기
    cnt <- 1
    for (i in 1:(length(data)-1)){
        val <- (data[i+1])-(data[i])
        for (k in 1:val){
            df[i,tag_data_vector[cnt]] <- text_data_vector[cnt]
            cnt <- cnt+1
        }
    }
    
    return(df)
}


########MAIN CODE##############################################################


# load packages
if(!require(parallel)) {
    install.packages("parallel")
}
library(parallel)

# 코어 개수 획득
numCores <- parallel::detectCores() - 1
# 클러스터 초기화
myCluster <- parallel::makeCluster(numCores)

Sys.time()

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="128.1.99.58",
                                                              schema="Dolphin_CDM.dbo",
                                                              user="atlas",
                                                              password="qwer1234!@")
connection <- DatabaseConnector::connect(connectionDetails)
connectionDetails <-connectionDetails
connection <- connection



#
diag_note <- DatabaseConnector::dbGetQuery(conn = connection,statement = "SELECT TOP 100 * FROM DBO.NOTE JOIN COHORT ON NOTE.person_id = COHORT.subject_id AND NOTE.NOTE_DATE = COHORT.COHORT_START_DATE WHERE cohort_definition_id = 747 AND NOTE_TITLE = \'퇴원요약\'") ;

#조건 내에 부합하는 df들의 merge 값 설정###############################################
cohort_outCount_df <- merge(outcomeCount_df,diag_note,by = c("PERSON_ID","NOTE_DATE"))
#######################################################################################

#필요한 값만 가지고와 df 생성
cohort_outCount_df <- data.frame(c(cohort_outCount_df['NOTE_ID'],cohort_outCount_df['NOTE_TEXT'],cohort_outCount_df['outcomeCount']),stringsAsFactors = FALSE)

#XML 파서로 나눔(병렬처리)
diagnosis_list <- parallel::parLapply(cl = myCluster, X = cohort_outCount_df$NOTE_TEXT, fun = XML_PARSING)

#결과 저장할 df 생성
final_xml_df <- data.frame(stringsAsFactors = FALSE) 

#진단서 하나의 DataFrame을 list에 저장(병렬처리)
result_xml_list <- parallel::parLapply(cl = myCluster, X = diagnosis_list, fun = DIAG_PROCESSING)

#한개의 진단서당 NOTE_ID 삽입
for (i in 1:length(result_xml_list)){
    result_xml_list[[i]][,'NOTE_ID'] <- cohort_outCount_df[['NOTE_ID']][i]
    result_xml_list[[i]][,'outcomeCount'] <- cohort_outCount_df[['outcomeCount']][i]
}
    
#제일 큰 값을 찾음
max_col <- 0
for(i in 1:length(result_xml_list)){
    col_value <- length(result_xml_list[[i]])
    if(max_col < col_value){
        max_col <- col_value
    }
}


result_tmp_df <- data.frame(stringsAsFactors = FALSE)
result_tmp2_df <- data.frame(stringsAsFactors = FALSE)
result_xml_df <- data.frame(stringsAsFactors = FALSE)
div = 1000
flag <- 0
if(div >= length(result_xml_list)){
    for(i in 1:length(result_xml_list)){
        if(length(result_xml_list[[i]]) == max_col){
            result_tmp_df <- rbind(result_tmp_df,result_xml_list[[i]])     
        }
    }
    flag <- 1
    result_xml_df <- result_tmp_df
}
if(flag == 0 ){
    for(i in 1:length(result_xml_list)){
        
        if(length(result_xml_list[[i]]) == max_col){
            
            result_tmp_df <- rbind(result_tmp_df,result_xml_list[[i]])
            
            if(i%%div ==0 & i>=div){
                result_xml_df <- rbind(result_xml_df,result_tmp_df)
                result_tmp_df <- data.frame(stringsAsFactors = FALSE)
            }
        }
        else{
            # 다른 것들과 다른 컬럼 개수를 가지고 있는 애들을 어떻게 처리해줄지 고민해봐야함.
        }
    }
    
    result_xml_df <- rbind(result_xml_df,result_tmp_df)
}

