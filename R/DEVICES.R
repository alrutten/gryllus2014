getSD = function(db = get('logger_path',envir = .gr),dtime,burrow ) {
   
 
   outfile    = paste(get('RAWdir', envir = .gr),substr(dtime,1,4),substr(dtime,1,10),burrow,"BOX001.TXT",sep="\\")
   #outautorun = gsub('BOX001.TXT','AUTORUN.TXT',outfile)
   dtime=gsub("\\.","-",dtime)
   
   drv=findRemovable()
   
   
     if (!(file.exists(outfile))) { 
        dir.create(dirname(outfile),showWarnings=FALSE,recursive=TRUE)
       
	    
	   
  if (length(drv)>0)  {   # SD card gets detected
    filename=paste0(drv[1],"\\BOX001.TXT")
    
	if (file.exists(filename)){        # file present on card
	  insize=file.info(filename)$size   # this information is no longer accessible after trying file.copy because of i/o errors    
	  file.copy(filename,outfile,overwrite=FALSE) #sadly, file.copy() returns TRUE for partial copies
	#  file.copy(gsub(get('DataFN',envir=.gr),get('AutorunFN',envir=.gr),filename),outautorun, overwrite = FALSE)
	  
	  sqliQuery(db,paste0("INSERT INTO file_status (path,year_,date_,filesize,burrow,upload_status, SD_problems) VALUES('",gsub('\\\\','/',outfile),"',",substr(dtime,1,4),
	                ",'",dtime,"',",file.info(outfile)$size/1000,",",burrow,",0,",ifelse(insize == file.info(outfile)$size,0,1),")"))
					
	 out = paste('files copied to',outfile,ifelse(insize == file.info(outfile)$size,'\nno','\nsome'),' problems with the SD-card')  
    } else out = 'no datafile on card'
}   else   out = 'no SD-card detected'
}	else   out=paste('ERROR: destination file ',outfile,' already exists. File not copied!',sep="")

cat(out)
return(out)
}



readRaw = function(path){
d = read.table(file=path,colClasses="character",row.names=NULL,col.names="V1",comment.char="",blank.lines.skip=TRUE,stringsAsFactors=FALSE)
	
	if (nrow(d)>0) bouts = data.frame(full=rle(d$V1)[[2]], bout_length = rle(d$V1)[[1]],stringsAsFactors=FALSE)	else
		
	  bouts=data.frame(full=format(file.info(path)$mtime,"%d%m%y%H%M%S"), bout_length=0,stringsAsFactors=FALSE)
return(bouts)
}


extractVars <- function(d)  {

	dfr=data.frame(id = NA, burrow=NA, full = d[,1],
	              datetime_=as.POSIXct(strptime(substring(d[,1],1,12),format = "%d%m%y%H%M%S")), 
				  rest=substring(d[,1],13,nchar(d[,1])), bout_length=d[,2], stringsAsFactors=FALSE)
	if(nrow(dfr)>1) {	
	  dfr$datetime_=ifelse(is.na(dfr$datetime_),as.POSIXct(strptime(paste(substring(dfr$full,1,4),"65",substring(dfr$full,7,12),sep=""),format="%d%m%y%H%M%S")),dfr$datetime_) #for if year is not numeric
	  dfr$datetime_=backtoPOSIXct(dfr$datetime_)
	  tst = c("transp","bv","pirlb")
	for (k in 1:length(tst)) {
	  td=regexpr(regexpString(tst[k]), dfr$rest) 
	  dfr[,paste(tst[k])]=substring(dfr$rest, td, td + attr(td, "match.length") - 1)
	  if ( k!=2) dfr$rest= paste(substring(dfr$rest,1,td-1),substring(dfr$rest,td+attr(td,"match.length"),nchar(dfr$rest)),sep="") else
	  dfr$rest=ifelse(td>-1,paste(substring(dfr$rest,1,td-1)),dfr$rest)
		}
	if 	(median(nchar(dfr$pirlb[dfr$pirlb!='']),na.rm=TRUE)<4) { 
		tpir=regexpr("[12]+",dfr$pirlb)
		tlb=regexpr("[34]+",dfr$pirlb)
		dfr$PIR = ifelse(as.numeric(row.names(dfr))<=max(which(nchar(dfr$pirlb)<4),na.rm=TRUE),
						substring(dfr$pirlb,tpir,tpir+attr(tpir,"match.length")-1),
						substring(dfr$pirlb,1,2)) 
		dfr$LB = ifelse(as.numeric(row.names(dfr))< max(which(nchar(dfr$pirlb)<4),na.rm=TRUE),
						substring(dfr$pirlb,tlb,tlb+attr(tlb,"match.length")-1),
						substring(dfr$pirlb,3,4))} else {
		dfr$PIR = substring(dfr$pirlb,1,2)
		dfr$LB = substring(dfr$pirlb,3,4)
		}
		
		dfr$PIR=ifelse(is.na(as.numeric(dfr$PIR)),NA,dfr$PIR)
		dfr$LB=ifelse(is.na(as.numeric(dfr$LB)),NA,dfr$LB)
	    dfr$transp=ifelse(dfr$transp=="",NA,dfr$transp)
		dfr$bv=ifelse(dfr$bv=="",NA,dfr$bv)
		} else dfr$transp=dfr$bv=dfr$pirlb=dfr$PIR=dfr$LB=NA
		return(dfr)
	


  
  }
  
regexpString = function(item, year=9999, version=0) { 

#  ideal string patterns; dd mm yy hh mm ss pir lb tr

	transp     = "\\w{7,11}F0001"  
	ddmm ="((0[1-9])|([12]\\d)|(30(?!02))|(31(?!((0[2469])|11))))((0[1-9])|(1[012]))"
				#day 0-29    day 30 notfeb day 31 not 30daymonths        month
	prevyear =substring(year-1,3,4)
	anyDate =paste(ddmm,"(\\d\\d)(([01]\\d)|(2[0-4]))([0-5]\\d)([0-5]\\d)",sep="") #taking max days per month into account
						   #year          hh            mm        ss
	specDate = paste("(((0[1-9])|([12]\\d)|(3[01]))((0[1-9])|(1[012]))(",substring(year,3,4),"|",prevyear,")(([01]\\d)|(2[0-4]))([0-5]\\d)([0-5]\\d)([1234]{0,4}|[012]{4}))", sep="")
	
	pirlb = "(^[012]{4}|(^1234|^123|^124|^134|^234|^12|^13|^14|^23|^24|^34|^1|^2|^3|^4))" # if version<2 pirlb should be ascending, can't be done in regexp hence had to list all combinations
	pir = ifelse(version<2,"[12]{1,2}","[01][02]")	
	lb = ifelse(version<2,"[34]{1,2}","[01][02]")	
	
	bv 			= "(BV|RT).{5}"	
	
	Record = paste(specDate,".*?(?=(",specDate,"|$))", sep="")  # valid date, random junk, until next valid date

	
	switch(item, transp =transp, ddmm=ddmm, anyDate=anyDate, specDate=specDate, pirlb=pirlb, pir=pir, lb=lb, bv=bv, Record=Record)
}

loadSingle=function(db =get('logger_path',envir=.gr), lb = get('logger_path',envir=.gr),id,check = TRUE,...) 
{ 
  focal  = sqliQuery(db,paste("select * from file_status where id=",id,sep=""))
  prev   = sqliQuery(db,paste("SELECT max(date_) md from file_status where burrow =",focal$burrow," and id <",focal$id))$md
  year   = as.numeric(substring(focal$date_,1,4))
 
  
  
  if (check) {
   indb=sqliQuery(lb,paste("select * from RFID where id=",id," limit 1",sep=""))
   if (nrow(indb)>0) sqliQuery(lb,paste0("DELETE FROM RFID where id = ",id))
   }
  # read & extract data
  dframe=extractVars(readRaw(path=tolower(focal[1,"path"])))
 
  focal$upload_status = 1
  # CHECKS
  {
 
  

  # valid/non-NA datetimes
  
  if (!is.na(focal$date_)) {
   if (is.na(prev)) prev = as.POSIXct(focal$date_)-30*86400
    days    = format(seq.Date(from=as.Date(prev)-1,to=as.Date(focal$date_)+1,by='day'),format='%m-%d')
    dtvalid = round(100*(sum(!is.na(dframe$datetime_)&dframe$bout_length!=-1&format(dframe$datetime_,format = '%m-%d')%in%days))/nrow(dframe),2)
   
 
  # wrong year
    yrvalid = round(100*sum(as.numeric(format(dframe$datetime_,"%Y"))%in% c((year-1):(year+1)))/nrow(dframe),2)
  
	} else dtvalid = yrvalid = 'NULL'
 
  # number of timejumps
  n_timejumps = sum(as.numeric(c(dframe$datetime_[-1], dframe$datetime_[length(as.vector(dframe$datetime_))])) <
        as.numeric(dframe$datetime_), na.rm = TRUE)
		
  focal$year_valid	   = yrvalid
  focal$datetime_valid = dtvalid
  focal$n_timejumps    = n_timejumps  

  
  }
   
  if (nrow(dframe)>0)
  {
    dframe=dframe[,-c(3,5,9)]
    dframe$id=focal[1,"id"]
	dframe$burrow=focal[1,"burrow"]
	

    bvframe = dframe[!is.na(dframe$bv),c("datetime_","burrow","id","bv")] #this is separate because in snb, there's a separate battery table
	if (nrow(bvframe)>0) 
    {
	  bvframe$type = substring(bvframe$bv,1,2)
	  bvframe$bv = substring(bvframe$bv,3,6)
      lastBatDt= bvframe$datetime_[nrow(bvframe)]
	  lastRT=bvframe$bv[max(which(bvframe$type=='RT'))]
	  lastBV=bvframe$bv[max(which(bvframe$type=='BV'))] 
	 
	} else lastRT=lastBV=lastBatDt=-1
	
	dframe$r_pk=NA
 
	
	sqliQuery(db,paste("Update file_status SET upload_status=",focal[1,"upload_status"], 
	                                      ", datetime_valid=",focal[1,"datetime_valid"],
										   ", year_valid=",focal[1,"year_valid"],
										    ", n_timejumps=",focal[1,"n_timejumps"],
										  ", dt_loaded='",Sys.time(),
										  "', lastBatdt='",lastBatDt,
										  "', lastRT=",ifelse(is.na(lastRT),-1,lastRT),
										  ", lastBV=", ifelse(is.na(lastBV),-1,lastBV),
										  ", dt_last='", dframe$datetime_[nrow(dframe)], 
									  "' WHERE id =",focal[1,"id"],sep="")) 
	if (nrow(dframe)>1)	{
	dframe$datetime_ = paste(dframe$datetime_)
       sqliSave(dframe[,c('burrow','datetime_','PIR','LB','transp','bv','id','bout_length','r_pk')],"RFID", lb)
	}
		
    
  } else 
  {
      sqliQuery(db, paste("Update file_status SET upload_status = -1, remarks ='upload unsuccessful critical errors' WHERE id=", focal[1,"id"]))	

  }
  
  gc()
}


loadAll = function(db =get('logger_path',envir=.gr), lb = get('logger_path',envir=.gr) ){
  print("compiling list")
  flist = sqliQuery(db, "SELECT * FROM file_status f Where upload_status=0") 
  indb=sqliQuery(lb,"SELECT distinct id from RFID")   # prevent double uploading
  doubles=flist[flist$id %in% indb$id,] 
  
  if (nrow(doubles)>0) {tkmessageBox(message=paste("the following id's were already uploaded \nand will be removed from the database first:\n\n",
											paste(doubles$id,collapse=", "),sep=""),icon="warning")}

  if (nrow(flist)>0)
  {
    print(paste(nrow(flist),"files will be uploaded"))
    for (i in 1:nrow(flist)) 
      try(loadSingle(id=flist[i,"id"], check = FALSE))
	  print(nrow(flist)," files processed",sep="")
  } else print('no new data to upload')
 
 
}	
getSDGui=function(dte=format(Sys.time()-86400,"%Y.%m.%d %H:%M:%S")) {
  
   sdw=tktoplevel()
   
    boxsel=tclVar("666")
	datesel=tclVar(dte)
	
	

	
	boxentry= tkentry(sdw, textvariable=boxsel)
	boxlabel = tklabel(sdw,text="burrow:")
	dateentry = tkentry(sdw,textvariable=datesel)
	datelabel=tklabel(sdw,text="datetime ")
	
	OnOk=function(bx=tclvalue(boxsel),dt_=tclvalue(datesel)){
     # if (missing(con)) con=snbcon()
	  cardThere=findRemovable()
	  if(length(cardThere)==0) foo=tkmessageBox(message="no card detected. Proceed?\n (yes= after inserting card (or if it's already there)\n no= cancel upload",type='yesno') else foo=tclVar('yes')
	  if (tclvalue(foo)=='yes'){
	  txt=getSD(dtime=dt_,burrow=bx)
	  tkmessageBox(message=paste(txt))
	  } else tkmessageBox(message="upload canceled")
     }
	  OK.but <-tkbutton(sdw,text="START UPLOAD",command=function() OnOk())
	  Cancel.But =tkbutton(sdw,text="CANCEL/DONE",command=function() tkdestroy(sdw))
	  tkgrid(boxlabel,boxentry,datelabel,dateentry)
	  tkgrid(tklabel(sdw,text="    "))
	  tkgrid(OK.but,Cancel.But) 
	  tkgrid.configure(OK.but,column=1)
	  tkgrid.configure(Cancel.But,column=2)
	  }
	  
	  
fs.findnew <- function(lb = get('logger_path',envir = .gr),path =get('RAWdir',envir = .gr),pattern = get('Datapattern',envir=.gr)){
	
	
	fl = toupper(gsub("\\\\|//","/",list.files(path, recursive = TRUE, pattern = pattern, full.names = TRUE)))
	indb =sqliQuery(lb,"select * from file_status")
	
	indb$path= toupper(gsub("\\\\|//","/",indb$path))
	
	fl=fl[!fl %in% indb$path]
	filesize = round(file.info(fl)$size/1000,digits=0)
	burrow = as.numeric(basename(dirname(fl)))
	date_ =  as.POSIXct(strptime(basename(dirname(dirname(fl))), "%Y.%m.%d"))
	date_[is.na(date_)]= as.POSIXct(strptime(basename(dirname(dirname(dirname(fl[is.na(date_)])))), "%Y.%m.%d")) #for when there were two downloads on one day
	
	d = data.frame(id=NA,path= fl, filesize, date_, dt_last=NA,burrow, lastBatdt=NA, upload_status=0, datetime_valid=NA, year_valid=NA, n_timejumps = NA, 
	               lastRT=NA,lastBV=NA,remarks=NA,dt_loaded=NA, year_ = format(date_,'%Y'), SD_problems = NA) 
	sqliSave(d,"file_status",lb)
   cat(nrow(d),"new files found\n")
}




