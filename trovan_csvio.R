devtools::install_github('alrutten/gryllus2014')

require(gryllus2014)
require(lattice)
require(grid)
require(plyr)
require(ggplot2)

options(stringsAsFactors=FALSE)

datadir = 'D://crickets'
pngdir = 'D://crickets/png'

#fetch data
fl = list.files(datadir,full.names=TRUE,pattern = '.CSV')

d = list()

for (i in 1:length(fl)) 
  d[[i]] = read.table(fl[i],sep = ';', header=TRUE)

d = do.call(rbind,d)
names(d) = c("loggerID","identifier","BV","GPS","date_","time","unit","antenna","Type","tag","weight","stat_in","stat_out",'event')
d$datetime_ = as.POSIXct(strptime(paste(d$date_,d$time),"%d-%m-%Y %H:%M:%S",tz = 'UTC') + 7200) #tz conversions suck
d$act = 10
d$burrow_id = paste(d$loggerID,d$unit)

#number of distinct burrows read per bus
ddply(d,.(loggerID),summarise,n_burrows = length(unique(unit)), 
      units = paste(unique(unit),collapse=','))


#ACTOGRAMS PER BURROW
{
allburrows = unique(d$burrow_id)

#create actograms: either as separate png's (first) or as pdf (last)

# ACTOGRAMS IN PNG
for ( i in 1:length(allburrows)) {
  fn = paste0(pngdir,"/",allburrows[i],"_",Sys.Date(),"_%03d.png")
  dd = d[which(d$burrow_id==allburrows[i]),]
  png(filename=fn)
  #p = actogram(act~datetime_, dat=dd, groups = 'tag', scale=1,layout = c(1,5), doublePlot = FALSE, main = paste0("bus,logger:",allburrows[i]))
  p =actogram(~datetime_,dat=dd,groups='tag') 
  p =  p +
    labs(title = paste('burrow',allburrows[i]))
  print(p)
  dev.off()
}

#ACTOGRAMS AS PDF
pdfname = paste0(pngdir,"/allburrows_",Sys.Date(),".pdf")
pdf(file=pdfname)
for ( i in 1:length(allburrows)) {
  
  dd = d[which(d$burrow_id==allburrows[i]),]
  
 # p = actogram_old(act~datetime_, dat=dd, groups = 'tag', scale=1,layout = c(1,5), doublePlot = FALSE, main = paste0("bus,logger:",allburrows[i]))
   p =actogram(~datetime_,dat=dd,groups='tag') 
 p =  p +
   labs(title = paste('burrow',allburrows[i]))
 
 print(p)
  
}
dev.off()
shell.exec(pdfname)
}

#ACTOGRAMS PER BUS (readers separate, transponder per reader lumped)
#ACTOGRAMS AS PDF
pdfname = paste0(pngdir,"/perBus_",Sys.Date(),".pdf")
pdf(file=pdfname)

allBus = unique(d$loggerID)

#aggregate per second
dd = ddply(d,.(loggerID,datetime_,unit), summarise,act = length(tag))
dd$logact = log(dd$act)
dd$unit = paste('unit',dd$unit)

plots = list()
for ( i in 1:length(allBus)) {
  
  ddd = dd[which(dd$loggerID==allBus[i]),]
  
  # p = actogram_old(act~datetime_, dat=dd, groups = 'tag', scale=1,layout = c(1,5), doublePlot = FALSE, main = paste0("bus,logger:",allburrows[i]))
  p =actogram(logact~datetime_,dat=ddd,groups='unit')
  p =  p +
    labs(title = paste('bus',allBus[i]))
  plots[[i]] = p
  print(p)
  
}
dev.off()
shell.exec(pdfname)
}

# relation between number of readings per second and subsequent lag:
d =d[order(d$loggerID,d$datetime_),]
b = ddply(d,.(loggerID,datetime_), summarise,act = length(tag))
b  = ddply(b,.(loggerID), mutate, 
                                dt_nxt = c(datetime_[-1],datetime_[length(datetime_)]),
                                lag = as.numeric(dt_nxt - datetime_))
xyplot(lag~act,data=b,groups = loggerID)
