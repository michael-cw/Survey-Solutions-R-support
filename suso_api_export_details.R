######################################################################################
##  Function for details (i.e. date)
######################################################################################

getSuSoDetails<-function(url="https://xxx.mysurvey.solutions", 
                         usr="", 
                         quid="",
                         version = 1, 
                         format="STATA",
                         q_name="questionnaire name") {
  require(httr)
  require(jsonlite)
  ##  Define the api 
  url=paste0(url, "/api/v1/export/")
  quid=paste0(quid, "$", version)
  #q_name=paste0(q_name, ".dta")
  
  ##  Receive the status
  test_detail<-GET(url = paste0(url, format,"/", quid, "/", "details"), authenticate(usr, pass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  return(test_json)
  ##########################F I N#########################################################
}
