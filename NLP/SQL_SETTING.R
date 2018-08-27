Sys.setenv(JAVA_HOME="C:\\Program Files\\Java\\jdk1.8.0_171") 
library(rJava)

#install.packages("devtools")
library(devtools)
#install.packages("drat")
#drat::addRepo("OHDSI")
#install.packages("FeatureExtration")

#install.packages("drat")
#drat::addRepo("OHDSI")
#install_github("ohdsi/FeatureExtraction", args = "--no-multiarch") 
#install_github("ohdsi/PatientLevelPrediction", args = "--no-multiarch") 

library(DatabaseConnector)
library(SqlRender)
library(FeatureExtraction)
library(PatientLevelPrediction)

workingFolder<-"D:/Dongsu/R_code/sql"
setwd(workingFolder)

connectionDetails<-DatabaseConnector::createConnectionDetails(dbms="sql server",
                                                              server="128.1.99.58",
                                                              schema="Dolphin_CDM.dbo",
                                                              user="atlas",
                                                              password="qwer1234!@")
connection <- DatabaseConnector::connect(connectionDetails)
connectionDetails <-connectionDetails
connection <- connection

cdmDatabaseSchema<-"Dolphin_CDM.dbo" 
targetDatabaseSchema<-"Dolphin_CDM.dbo"
targetCohortTable<-"cohort"
targetCohortId <- 747
outcomeCohortId <- 748
cdmversion <- "5"

#CREATE TABLE & INPUT VALUE
#DB ���� 7���̻� �Կ��� ȯ�ڵ��� ������ �̾� INSERT
#===========================================
# (T)
#===========================================

sql <- SqlRender::readSql(file.path(workingFolder,"all_admission.sql")) #local
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=targetCohortId
                            
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql

DatabaseConnector::executeSql(connection,sql)

#��� ���޽��Կ� ȯ�ڵ�
#===========================================
# (O)
#===========================================

sql <- SqlRender::readSql("ed_visit.sql") #local
sql <- SqlRender::renderSql(sql,
                            cdm_database_schema=cdmDatabaseSchema,
                            target_database_schema=targetDatabaseSchema,
                            target_cohort_table=targetCohortTable,
                            target_cohort_id=outcomeCohortId
)$sql
sql <- SqlRender::translateSql(sql,
                               targetDialect=connectionDetails$dbms)$sql
DatabaseConnector::executeSql(connection,sql)

#30�� �̳��� ���Կ��� ȯ�ڸ� DF�� �̾Ƴ��� �Լ�.
covariateSettings <- FeatureExtraction::createCovariateSettings(useDemographicsGender = FALSE,
                                                                useDemographicsAge = FALSE, useDemographicsAgeGroup = FALSE,
                                                                useDemographicsRace = FALSE, useDemographicsEthnicity = FALSE,
                                                                useConditionOccurrenceLongTerm = FALSE,
                                                                useDrugExposureLongTerm = FALSE,
                                                                useProcedureOccurrenceLongTerm = FALSE,
                                                                useMeasurementLongTerm = FALSE,
                                                                useObservationLongTerm = FALSE,
                                                                useDistinctConditionCountLongTerm =FALSE,
                                                                useVisitCountLongTerm = FALSE,
                                                                longTermStartDays = 0,
                                                                endDays = 30)

plpData <- PatientLevelPrediction::getPlpData(connectionDetails = connectionDetails,
                                              cdmDatabaseSchema = cdmDatabaseSchema,
                                              #oracleTempSchema = oracleTempSchema,
                                              cohortDatabaseSchema = targetDatabaseSchema,
                                              cohortTable = "cohort",
                                              cohortId = targetCohortId,
                                              washoutPeriod = 0,
                                              covariateSettings = covariateSettings,
                                              outcomeDatabaseSchema = targetDatabaseSchema,
                                              outcomeTable = "cohort",
                                              outcomeIds = outcomeCohortId,
                                              cdmVersion = cdmversion
                                              )

population <- PatientLevelPrediction::createStudyPopulation(plpData, population = NULL, 
                                                            outcomeId=outcomeCohortId, 
                                                            binary = T,
                                                            includeAllOutcomes = T, 
                                                            firstExposureOnly = FALSE, 
                                                            washoutPeriod = 0,
                                                            removeSubjectsWithPriorOutcome = FALSE, 
                                                            priorOutcomeLookback = 99999,
                                                            requireTimeAtRisk = T, 
                                                            minTimeAtRisk = 1, 
                                                            riskWindowStart = 1,
                                                            addExposureDaysToStart = FALSE, 
                                                            riskWindowEnd = 30,
                                                            addExposureDaysToEnd = F)


#################################################################################################################
#�ʿ��� ������ ���� dataframe���� ���� -> 30�� �̳��� �Կ� ������ �ľ��� ����� �����.
outcomeCount_df <- data.frame(c(population["subjectId"],population["cohortStartDate"],population["outcomeCount"]))
#�����ϱ� ���� NOTE_TABLE�� �Ӽ� �̸��� ���� ������ 
colnames(outcomeCount_df) <-c("PERSON_ID","NOTE_DATE","outcomeCount")
#################################################################################################################

#DatabaseConnector::dbDisconnect(conn = connection)
