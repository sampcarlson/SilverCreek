library(curl)

destDir="C:/Users/sam/Documents/SilverCreek/Backup/SSCFTP"

destDir=paste0(destDir,"/",Sys.Date())
dir.create(destDir)

ssc=new_handle(verbose=F,username="Tagging@savesilvercreek.com",password=Sys.getenv("SSC_PASS"))

#for all files:
#sscUrl="ftp://savesilvercreek.com/"

#for track a trout only:
sscUrl="ftp://savesilvercreek.com/Track_A_Trout/"


#ftp test
c=rawToChar(curl_fetch_memory(sscUrl,ssc)$content)


sscContent=data.table::fread(rawToChar(curl_fetch_memory(sscUrl,ssc)$content),blank.lines.skip = T,fill=T)

#look ok?
sscContent

dlFiles=function(fileNames,saveDir,baseUrl,sscHandle=ssc){
  for(fName in fileNames){
    this.url=paste0(baseUrl,fName)
    this.dest=paste0(saveDir,"/",fName)
    print(this.url)
    try(
      curl_download(url=this.url,destfile = this.dest,handle=sscHandle),
      silent=F
    )
  }
}

#download all top level files
readDlDir=function(addDir=NULL,baseUrl=sscUrl,baseDir=destDir,sscHandle=ssc){
  if(!is.null(addDir)){
    dirUrl=paste0(baseUrl,addDir,"/")
    localDir=paste0(baseDir,"/",addDir)
    dir.create(localDir)
  } else {
    dirUrl = baseUrl
    localDir=baseDir
  }
  print(paste("dir:",dirUrl))
  
  allNames=data.table::fread(rawToChar(curl_fetch_memory(dirUrl,sscHandle)$content),blank.lines.skip = T)$V9
  dirNames=grep("\\.",allNames,invert=T,value=T)
  fileNames=grep("\\....",allNames,invert=F,value=T)
  
  #download files in this dir
  dlFiles(fileNames,saveDir=localDir,baseUrl=dirUrl)
  
  #apply dir function to dirs within this dir
  summary=sapply(dirNames,FUN=readDlDir,baseUrl=dirUrl,baseDir=localDir)
}

readDlDir()
