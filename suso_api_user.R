################
## API USER management




getSV<-function(url="https://xx.mysurvey.solutions",
                usr="ApiUser",
                pass="ApiPass"){
  ## Load the libraries
  library("httr")
  library("xml2")
  library("jsonlite")
  
  ##  Define the api 
  url=paste0(url, "/api/v1/supervisors")
  print(url)
  test_detail<-GET(url = paste0(url, "?limit=200"), 
                   authenticate(usr, pass, type = "basic"))
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  return(test_json)
}


#####################################################
##  Requires SV ID!!
##    DF is Users, interviewer id/name is col UserID/UserName
getINT<-function(url="https://xx.mysurvey.solutions",
                 usr="ApiUser",
                 pass="ApiPass",
                 sv="sv_id"){
  ## Load the libraries
  library("httr")
  library("xml2")
  library("jsonlite")
  
  ##  Define the api 
  url=paste0(url, "/api/v1/supervisors/")
  test_detail<-GET(url = paste0(url,sv,"/interviewers?limit=200"), 
                   authenticate(usr, pass, type = "basic"))
  
  aJsonFile<-tempfile()
  writeBin(content(test_detail, "raw"), aJsonFile)
  test_json<-fromJSON(aJsonFile)
  return(test_json)
  
}

