#########################
##  SUSO API
##  ASSIGNMENT 
##  v1.0.0
#########################

assignments<-function(questName="",
                      server="https://xxx.mysurvey.solutions",
                      apiUser="ApiUser",
                      apiPass="ApiPass",
                      Id=NULL,
                      version=1,
                      responsible = NULL,
                      order.by="Id",
                      supervisorId=NULL,
                      quantity = NULL) {
  
  require(data.table)
  require(httr)
  require(jsonlite)
  ############
  # 1. create variables
  url<-paste0(server, "/api/v1/assignments/")
  usr<-apiUser
  pass<-apiPass
  
  ###########
  # 2 Operations
  
  full_data<-list()
  if (is.null(Id)) {
    # 2.1. All Assignments, without filtering, and ordered by ID
    #   --> LIMIT on API is set to 100
    #       --> get first batch of 100 assignments
    #       --> take variable totalcount and add to Id of first assignment
    # i. First Run
    test_detail<-GET(url = paste0(url, "?order=", order.by, "&limit=100"), authenticate(usr, pass, type = "basic"))
    aJsonFile<-tempfile()
    writeBin(content(test_detail, "raw"), aJsonFile)
    test_json<-fromJSON(aJsonFile)
    full_data[[1]]<-data.table(test_json$Assignments, key = "Id")
    
    
    # ii. get the total and the first number
    tot<-test_json$TotalCount
    first<-test_json$Assignments$Id[1]
    
    # iii. create a loop to get the rest
    rest<-tot-100
    loop.counter<-ceiling(rest/100)
    for(i in 1:loop.counter){
      offset<-100*i
      test_detail<-GET(url = paste0(url, "?order=", order.by,"&offset=", offset, "&limit=100"), authenticate(usr, pass, type = "basic"))
      aJsonFile<-tempfile()
      writeBin(content(test_detail, "raw"), aJsonFile)
      test_json<-fromJSON(aJsonFile)
      listIndex<-i+1
      full_data[[listIndex]]<-data.table(test_json$Assignments, key = "Id")
    }
    test_json<-rbindlist(full_data)
    return(test_json)
    
  } else if (!is.null(Id) & (!is.null(responsible))){
    
    ## LIB
    ## Load the libraries
    library("httr")
    library("xml2")
    library("jsonlite")
    
    ##  API parameters 
    url=paste0(url, Id, "/assign")
    ##  the post
    print(url)
    js_ch<-list(Responsible=unbox(responsible))
    test_post<-httr::PATCH(url = url,
                           accept_json(),add_headers(charset="utf-8"),
                           authenticate(usr, pass, type = "basic"),
                           body=js_ch, encode = "json")
    ##  ATTENTION: USE THE RESPONSE
    aJsonFile<-tempfile()
    writeBin(content(test_post, "raw"), aJsonFile)
    status_list<-fromJSON(aJsonFile)
    return(status_list)
  }
}

