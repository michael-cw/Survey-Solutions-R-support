##################################################
##  API file for:
#################################################
# https://gabon1.mysurvey.solutions/api/DataExportApi/AllData/
#  3dcc4ce9-8393-4d38-9cfd-fea95d7fd573?version=1&format=STATA&status=null"


## Load the libraries
library("httr")
library("xml2")
library("jsonlite")
library("readstata13")

createASS<-function(df=UPLOADdataCHECK,
                    url="https://xxx.mysurvey.solutions",
                    usr="",
                    pass="",
                    QUID = NULL,
                    version =NULL){
  ## LIB
  ## Load the libraries
  library("httr")
  library("xml2")
  library("jsonlite")
  
  ##  API parameters 
  url=paste0(url, "/api/v1/assignments")
  quid=paste0(QUID,"$", version)
  ##  the post
  resp<-df$responsible
  df[,responsible:=NULL]
  status_list<-list()
  
  for(i in 1:nrow(df)){
    print(resp[i])
    js_ch<-list(Responsible=unbox(resp[i]),
                Quantity=unbox(1),
                QuestionnaireId=unbox(quid),
                IdentifyingData=data.frame(Variable=names(df), 
                                           Answer=unlist(df[i,],use.names = F)))
    test_post<-httr::POST(url = url,
                          accept_json(),add_headers(charset="utf-8"),
                          authenticate(usr, pass, type = "basic"),
                          body=js_ch, encode = "json")
    ##  ATTENTION: USE THE RESPONSE
    aJsonFile<-tempfile()
    writeBin(content(test_post, "raw"), aJsonFile)
    status_list[[i]]<-fromJSON(aJsonFile)
  }
  return(status_list)
}
