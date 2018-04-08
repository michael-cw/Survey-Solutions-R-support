########################################
##  Function 1 contains all SUSO Questionnaire API commands
##  Function 2 creates a DT of the variable names, types etc.

getQuestDeails<-function(url="https://xxx.mysurvey.solutions",
                         usr="ApiUsr",
                         pass="ApiPass",
                         quid="xxx-xxx-xxx-xxx",
                         version = 1,
                         limit=40,
                         offset=0,
                         operation.type=c("list", "statuses", "structure", "interviews")) {
  require(httr)
  require(jsonlite)
  ##  Define the api 
  url=paste0(url, "/api/v1/questionnaires/")
  operation.type=ifelse(is.null(operation.type), "list", operation.type)
  
  
  ## 1. Get all Questionnaires on the server
  #https://mm-iva.mysurvey.solutions/api/v1/questionnaires
  if (operation.type=="list"){
    test_detail<-GET(url = paste0(url), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
  } else if (operation.type=="statuses") {
    ## 3. Questionnaire statuses
    print(paste0(url, "statuses"))
    test_detail<-GET(url = paste0(url, "statuses"), authenticate(usr, pass, type = "basic"))
    
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    #CHECK<-readBin(aJsonFile, "character", n = 100000L, size = 100000)
    test_json<-fromJSON(aJsonFile)
  } else if (operation.type=="structure") {
    ## 4. Export Questionnaire structure
    if (is.null(quid) |is.null(version)) stop("Quid and/or version missing.")
    test_detail<-GET(url = paste0(url, quid,"/", version, "/document"), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    #CHECK<-readBin(aJsonFile, "character", n = 100000L, size = 100000)
    test_json<-fromJSON(aJsonFile)
    json_mod<-gsub("(^\")(.*)(\"$)", "\\{\\1\\{\\3", test_json)
    test_json<-fromJSON(json_mod, flatten = T)
  } else if (operation.type=="interviews") {
    if (is.null(quid) |is.null(version)) stop("Quid and/or version missing.")
    test_detail<-GET(url = paste0(url, quid,"/", version, "/interviews?limit=", limit, "&offset=", offset), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
  }
  
  return(test_json)
  ##########################F I N#########################################################
}

fullMeta<-function(input=qestStruct)  {
  ## Get section names
  secNames<-qestStruct$Children$Title
  
  qstr<-list()
  nSection<-length(input$Children[[2]])
  for(i in 1:nSection) {
    checkName<-names(input$Children[[2]][[i]])
    rosterpos<-grep(checkName, pattern = "IsRoster")
    if (length(rosterpos)==0) {
      qstr[[secNames[i]]]<-data.table(varname=input$Children[[2]][[i]]$VariableName,
                                      
                                      type=input$Children[[2]][[i]]$`$type`,
                                      text=input$Children[[2]][[i]]$QuestionText)
      
    } else {
      qstr[[secNames[i]]]<-data.table(varname=input$Children[[2]][[i]]$VariableName,
                                      isrost=input$Children[[2]][[i]]$IsRoster,
                                      type=input$Children[[2]][[i]]$`$type`,
                                      text=input$Children[[2]][[i]]$QuestionText)
      
    }
  }
  return(qstr)
  
  
}














