############################################
##  PARADATA EXPORT FUNCTION
##  v0.1.0
##  071217



fileCollectorPara<-function(server="https://xxx.mysurvey.solutions",
                            apiUser="ApiUser",
                            apiPass="ApiPass",
                            questID="xxxx-xxx-xxxx-xxx-xxx",
                            version=1,
                            format_para="Paradata"){ 
  ##  check detail
  require(data.table)
  require(httr)
  require(jsonlite)
  server=paste0(server, "/api/v1/export/")
  test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"), 
                   authenticate(apiUser, apiPass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  print(test_json)
  
  
  ##  Create the FILE (para data)
  
  test_post<-POST(url = paste0(server, format_para,"/", questID, "$", version, "/", "start"), 
                  authenticate(apiUser, apiPass, type = "basic"), timeout(1200))
  ##  check detail
  test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"), 
                   authenticate(apiUser, apiPass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  
  while (!is.null(test_json$RunningProcess)) {
    test_detail<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", "details"), 
                     authenticate(apiUser, apiPass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
  }
  
  
  ##  Download the FILE
  test_exp<-GET(url = paste0(server, format_para,"/", questID, "$", version, "/", ""), 
                authenticate(apiUser, apiPass, type = "basic"))
  
  
  aZipFile<-tempfile()
  writeBin(content(test_exp, "raw"), aZipFile)

  
  ########################### FILE ############################################ 
  para_data<-list()
  
  #############################################################################  
  uploadFile<-aZipFile
  FILE.list<-unzip(uploadFile, list=T)
  #########################################################################
  ##  Unpack, create data.tables and stor in list, create subsets by action
  #########################################################################
  nfiles<-length(FILE.list[,1])
  
  ##  Retrieving the GPS variable from user input
  
  thefiles<-tempdir()
  unzip(uploadFile, files=NULL, exdir = thefiles)
  ##  Unpack (w. function and lapply)
  prog<-1
  unpack<-function(FILE.name, gps1=gps[[1]][1], gps2=gps[[1]][2], gps3=gps[[1]][1]){
    ID<-strsplit(FILE.name, ".tab")[[1]]
    FILE<-read.delim(paste0(thefiles, "/", FILE.name), header=F, sep="\t", stringsAsFactors=F, fileEncoding ="UTF-8-BOM")
    FILE$counter<-1:length(FILE$V1)
    FILE$V1<-as.factor(FILE$V1)
    FILE$ID<-ID
    
    FILE<-data.table(FILE)
    setkey(FILE, ID)
    FILE[, name_int:= V2[!is.na(V2)&V1=="AnswerSet"][1L], by=ID]
    FILE[, name_sv:= V2[!is.na(V2)&(V1=="ApproveBySupervisor"|V1=="RejectedBySupervisor")][1L], by=ID]
    #FILE[, GPS:= V7[!is.na(V7)&(V6==gps1|V6==gps2|V6==gps3)][1L], by=ID]
    #FILE[, lon:=strsplit(GPS, (",|\\[|\\]"))[[1]][1]]
    #FILE[, lat:=strsplit(GPS, (",|\\[|\\]"))[[1]][2]]
    #FILE[, prec:=strsplit(GPS, (",|\\[|\\]"))[[1]][3]]
    #FILE[, alti:=strsplit(GPS, (",|\\[|\\]"))[[1]][4]]
    
    return(FILE)
    prog<-prog+1
  }
  
  
  paradata_files<-lapply(FILE.list[,1], unpack)
  ##  check empty columns in df
  sum(unlist(lapply(paradata_files, function(x)lapply(x, function(y)sum(is.na(y))))))
  
  ##  get all the factor levels used for action
  allLevels<-lapply(paradata_files, function(x) levels(x[, V1]))
  actionLevels<-allLevels[which.max(lapply(allLevels, length))]
  
  ##  Subset with function, key and lapply
  subsetDataTableAction<-function(x, action) {
    setkey(x, V3)
    FILE<-x[J(c(action))]
    return(FILE)
  }
  
  ##  Identify Date time
  is.char.date<-function(x){
    #identifies date by this pattern: 2018-04-04T10:58:43
    patt="[0-9]{4}-[0-9]{2}-[0-9]{2}T[0-9]{2}:[0-9]{2}:[0-9]{2}"
    grepl(pattern = patt, x)
  }
  ##  Convert date and time into data table format
  convertDate<-function(DTfile){
    ## get the date column
    date.col<-names(DTfile)[sapply(DTfile[1], is.char.date)]
    print(date.col)
    DTfile[,c("date", "time"):=tstrsplit(DTfile[[date.col]], "T")]
    DTfile[,c("date", "time"):=.(as.IDate(date), as.ITime(time, format = "%H:%M:%S"))]
    # DTfile$time<-as.ITime(DTfile[,V5], format = "%H:%M:%S")
    # DTfile$date<-(as.IDate(DTfile$date))
    return(DTfile)
  }
  
  ##  Calculate time difference
  ##  After 3 minutes it is considered an unusual interruption, if smaller 0, then 0 because error
  calcTimeDiff<-function(DTfile, by=c("ID","date")){
    DTfile[order(rank(counter)), ID]
    DTfile[, resp_time:=c(0, ifelse((diff(time))>180|(diff(time))<=0, 0,(diff(time)))), by=by]
    #DTfile[, breaks:=ifelse((diff(time))>180,1,0), by=by]
    return(DTfile)
  }
  
  ########################################################################################
  ##  Manipulating the data 
  ########################################################################################
  ##  Creating separate list for each Action and transform to single data.table in one (lapply, do.call)
  AnswerSet<-do.call(rbindlist, list(lapply(paradata_files, subsetDataTableAction, action="AnswerSet"),
                                     fill=T))
  AnswerSet<-AnswerSet[!is.na(AnswerSet$ID),]
  AnswerSet<-convertDate(AnswerSet)
  AnswerSet<-calcTimeDiff(AnswerSet)
  AnswerSet[,c("V2","V3", "V4", "V5"):=NULL]
  names(AnswerSet)<-c('quest','dateTime' ,'var',
                      'resp', 'ros_id', 'pos', 'id', 'int', 'sv', 'date', 'time', 'resp_time')
  para_data$AnswerSet<-AnswerSet
  
  AnswerRemoved<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="AnswerRemoved"),
                                        fill=T))
  AnswerRemoved<-AnswerRemoved[!is.na(AnswerRemoved$ID),]
  AnswerRemoved<-convertDate(AnswerRemoved)
  AnswerRemoved[, count:=length(counter), by=ID]
  AnswerRemoved[,c("V2","V3", "V4", "V5"):=NULL]
  names(AnswerRemoved)<-c('quest','dateTime' ,'var_id','resp','ros_id',
                          'pos', 'id', 'int', 'sv', 'date', 'time', 'count')
  para_data$AnswerRemoved<-AnswerRemoved
  
  ApproveByHeadquarter<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="ApproveByHeadquarter"),
                                               fill=T))
  ApproveBySupervisor<-do.call(rbindlist, list(lapply(paradata_files, subsetDataTableAction, action="ApproveBySupervisor"), 
                                               fill=T))
  Completed<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="Completed"), fill=T))
  GroupEnabled<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="GroupEnabled"), fill=T))
  GroupDisabled<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="GroupDisabled"), fill=T))
  InterviewerAssigned<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="InterviewerAssigned"), fill=T))
  para_data$InterviewerAssigned<-InterviewerAssigned
  
  
  QuestionDeclaredInvalid<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="QuestionDeclaredInvalid"), fill=T))
  QuestionDeclaredInvalid<-QuestionDeclaredInvalid[!is.na(QuestionDeclaredInvalid$ID),]
  QuestionDeclaredInvalid[, count:=length(counter), by=ID]
  QuestionDeclaredInvalid[,c("V2","V3", "V4", "V5", "V8"):=NULL]
  names(QuestionDeclaredInvalid)<-c('quest','var_id' ,'resp','ros_id', 'pos', 'id', 'int', 'sv', 'count')
  para_data$QuestionDeclaredInvalid<-QuestionDeclaredInvalid
  
  QuestionDeclaredValid<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="QuestionDeclaredValid"), fill=T))
  QuestionDeclaredValid<-QuestionDeclaredValid[!is.na(QuestionDeclaredValid$ID),]
  QuestionDeclaredValid[, count:=length(counter), by=ID]
  para_data$QuestionDeclaredValid<-QuestionDeclaredValid
  
  QuestionEnabled<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="QuestionEnabled"), fill=T))
  para_data$QuestionEnabled<-QuestionEnabled
  QuestionDisabled<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="QuestionDisabled"), fill=T))
  ReceivedByInterviewer<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="ReceivedByInterviewer"), fill=T))
  ReceivedBySupervisor<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="ReceivedBySupervisor"), fill=T))
  RejectedBySupervisor<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="RejectedBySupervisor"), fill=T))
  Restarted<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="Restarted"), fill=T))
  SupervisorAssigned<-do.call(rbindlist,list(lapply(paradata_files, subsetDataTableAction, action="SupervisorAssigned"), fill=T))
  
  return(para_data)
  
}