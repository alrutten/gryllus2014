actogram = function(formula, 
                    dat, 
                    groups, ...) {
  dat = dat
  
  if(!inherits(formula, "formula")) stop("not a formula object.")
  
  if(length(all.vars(formula)) !=2) stop("Formula must be of form: activity ~ time")
  x = deparse(formula[[3L]])         #datetime
  y = deparse(formula[[2L]])         #activity
  
  dat[, x] = as.POSIXct(dat[, x])
  
  dat$day  = as.Date(trunc(dat[, x] , "day") )
  dat$Time = as.numeric(difftime(dat[, x], trunc(dat[, x], "day"), units = "hours"))
  
  #add missing days
  days     = seq.Date(min(dat$day),max(dat$day),by='days')
  dat      = merge(dat,data.frame(day = days),all=TRUE)
  
  
  dat = dat[!is.na(dat$day),]
  
  
  # make y-values
  if (!missing(groups)) dat$groups = dat[,groups] else dat$groups = 1
  transps = unique(dat$groups)
  
  transps$act = 2*(length(transps):1)
  
  
  dat = merge(dat,transps,by='groups')
  dat$ymin = dat$act-1
  dat$ymax = dat$act+1
  
  #right strip
  
  dates = unique(dat$day)
  dates$time=dates$act = 12
  dates$dateLab = format(strptime(dates$day,'%Y-%m-%d'),'%d %b')
    
  
  p = ggplot(dat,aes(x=time,y=act)) +
    geom_linerange(aes(ymin=ymin,ymax=ymax,group = groups,colour=col)) +
    geom_vline(xintercept = c(0,24),lwd=0.6,col='black') +
    geom_rect(xmin=-10,xmax=-0.1,ymin=0,ymax = max(transps$act)+5,fill='white',colour='white') +
    geom_text(data=dates,aes(label = dateLab,cex=0.6),y = mean(transps$act),x=-5,hjust=0) +
    facet_grid(day~.) +
    geom_text(aes(x=-2.9,y=act,label = groups,cex=0.6,colour = col),data=transps,hjust=0) +
    theme(
      axis.title.y=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank(),
      plot.background  = element_blank(),
      panel.background = element_blank(),
      panel.border = element_rect(colour = 'black',fill=NA),
      axis.line.x = element_line(colour='black'),
      axis.text.x = element_text(colour='black'),
      panel.grid.major=element_line(colour = 'dark grey'),
      panel.grid.minor=element_line(colour = 'light grey'),
      strip.text = element_blank(),
      legend.position = 'none',
      strip.background = element_blank(),
      plot.background = element_blank()
      
    ) +
    scale_x_continuous(breaks = c(0:12)*2,limits = c(-4,23)) +
    scale_y_continuous(breaks = transps$act-1,minor_breaks=NULL,limits = c(0,max(transps$act)+1)) +
   # scale_colour_identity() + 
  #  scale_fill_identity() +
    labs(title = paste('box',unique(dat$box)))
  
  return(p)
}



# TODO; COG, circular mean, etc
# TODO; day, night bar
# TODO: avoid data doubling for doublePlot = TRUE 


actogram_old <- function(formula, 
					dat, 
					groups, 
					strip.left.classes, 
					strip.left.cex = .8, 
					strip.left.format = "%b-%d", 
					doublePlot = TRUE,
					strip.left = TRUE,
					rug = FALSE,
					type = "h",					
					scale = 2,
					xaxt = TRUE,
					groups.key = TRUE, 
					xlab = "Hours", 
					ylab = "Activity",
                    layout,
                    key,...) {

dat = dat

if(!inherits(formula, "formula")) stop("not a formula object.")

if(length(all.vars(formula)) !=2) stop("Formula must be of form: activity ~ time")
x = deparse(formula[[3L]])         #datetime
y = deparse(formula[[2L]])         #activity

dat[, x] = as.POSIXct(dat[, x])

dat$day  = as.Date(trunc(dat[, x] , "day") )
dat$Time = as.numeric(difftime(dat[, x], trunc(dat[, x], "day"), units = "hours"))

#add missing days
days     = seq.Date(min(dat$day),max(dat$day),by='days')
dat      = merge(dat,data.frame(day = days),all=TRUE)

# double data
if(doublePlot) {
	aa = dat
	aa$Time = aa$Time + 24
    aa$day  = aa$day - 1
	dat = rbind(dat,aa)
	}


#groups
groups = if(!missing(groups)) dat[, groups] else NULL

# xy scales
if(xaxt) 
	scales = list( x = list(at = 0 : (if(doublePlot) 48 else 24), alternating = 3,
					labels = rep(format(seq.POSIXt(trunc(Sys.time(), "day"), 
							trunc(Sys.time(), "day") +23*3600, "hours"),  "%H:%M"), len = 49),
					rot = 90,
					cex = .7,
					limits = c(0,(if(doublePlot) 48 else 24))),
					y = list(draw = FALSE)		
				) else
		scales =  list(draw = FALSE)
				

# strip.left color factor
if(missing(strip.left.classes)) { 
	strip.left.classes   = "noClass"
	dat$noClass = "a"
	}

sl = dat[!duplicated(dat$day), c("day", strip.left.classes)]

z = data.frame(table(sl[, strip.left.classes]))
z$cols = trellis.par.get("superpose.polygon")$col[1:nrow(z)]
sl =  merge(sl, z, by.x = strip.left.classes, by.y = 'Var1', all.x = TRUE )
sl = sl[order(sl$day), ]

#strip.left
if(strip.left)			
strip.left	= function(which.panel, ...) {
					LAB = format(sl[which.panel, "day"], strip.left.format)
					grid.rect(gp = gpar(fill = sl[which.panel, "cols"] ))
					ltext(.5, .5, cex = strip.left.cex, LAB )
		      }

panel = function(x,y,...) {
				   y = y/scale
				   panel.abline(v = c(0:12)*2,col = 'dark grey')
				   panel.abline(v=(c(0:11)*2)+1,col = 'light grey')
				   panel.xyplot(x,y, type = type, ...,
				   )
				   
				   if(rug) panel.rug(x, regular = FALSE,  col = 2) # non- missing data
				   panel.abline(v = 24, col = "grey")
				    # panel.number() 
				   }			  
# legend
if (missing(key)) {
 if(!is.null(groups) & nrow(z) > 1) {
	groupLevels = levels(factor(groups))
	stripLevels = levels(factor(z$Var1))
			  
	key = list(..., adj = 1,
				text = list(stripLevels),
				rectangles = list(col = as.character(z$cols)),
				text = list(groupLevels) , 
				points = list(pch = 15, cex = 2, col =  trellis.par.get("superpose.symbol")$col[1:length(groupLevels)]),
				rep = FALSE
				)
	}	else if
	(!is.null(groups) & nrow(z) == 1) {
	groupLevels = levels(factor(groups))
	key = list(..., adj = 1,
				text = list(groupLevels) , 
				points = list(pch = 15, cex = 2, col =  trellis.par.get("superpose.symbol")$col[1:length(groupLevels)])
				)
	} else if
	(is.null(groups) & nrow(z) > 1) {
	stripLevels = levels(factor(z$Var1))
	key = list(..., adj = 1,
				text = list(stripLevels),
				rectangles = list(col = as.character(z$cols))
				)
	} else
	key = NULL

 if(!groups.key)
 key = NULL
 }

#layout
if (missing(layout)) layout = c(1, length(unique(dat[, "day"])))

#xyplot
xyplot(..., as.formula(paste(y, "~ Time|day")), data = dat, 
			lattice.options = list(layout.widths = list(strip.left = list(x = max(nchar(dat$day)) ))),
			layout = layout, 	
			as.table = TRUE,
			groups = groups,
			strip = FALSE,
			strip.left = strip.left,
			panel = panel,	
			xlab = xlab, 
			ylab = ylab,
			scales = scales,
			key = key
	)	

}		

visits_actogram_data = function ( lb = get("logger_path", 
    envir = .gr), burrow, startd, endd)
{	
# summarises transponder readings per minute
 
  dat    = sqliQuery(lb,paste("SELECT distinct burrow, datetime(round(CAST(strftime('%s.%f',datetime_)/60 as REAL)/1,0)*1*60,'unixepoch') as datetime_, transp from RFID
	                             where  datetime_>=",shQuote(startd),
	                             " AND datetime_<=",shQuote(endd),
								 " AND datetime_ IS NOT NULL",
								 " AND burrow = ",burrow,
								 " ORDER BY burrow,datetime_",sep=''))
  
#  testtr = sql(con,"SELECT transponder from test_transponders")
#  dat$transp[dat$transp%in%testtr$transp]= 'visit'
#  dat$transp[which(is.na(dat$transp))] = ''
  
  dat$datetime_ = as.POSIXct(dat$datetime_)
  #dat$day       = as.Date(trunc(dat$datetime_, "day"))
  #dat$time      = as.numeric(difftime(dat$datetime_,trunc(dat$datetime_,"day"),   #DO NOT USE dat$day here!! (the time will be wrong)
                                         #units = "hours"))  
 dat$act = 10
  return(dat)
  }

actogram_plot = function(d, tf = tempfile(fileext='.pdf'), show = TRUE) {   
# TODO: streamline this monstrous function.

  if (missing(d)) stop('no data')
  
  d$act = 10
  visit_actogram = actogram(act~datetime_,dat = d,groups = 'transp',scale = 1, layout = c(1,30), doublePlot = FALSE, main = paste0('burrow ',d$burrow[1]))
 	 
 # init pdf, graph stuff
 
  pdf(tf, paper='a4', width=8, height=11.6929134)
 
  print(visit_actogram)
    dev.off()
    if (show) shell(tf)
} 


 actogramGUI = function() {
  
   agw=tktoplevel()
   
    boxsel    = tclVar("666")
	sdatesel  = tclVar('2012-03-01')
	edatesel  = tclVar('2012-06-20')
	
	
	boxentry   = tkentry(agw, textvariable=boxsel)
	boxlabel   = tklabel(agw, text = "burrow:")
	sdateentry = tkentry(agw,textvariable=sdatesel)
	edateentry = tkentry(agw,textvariable=edatesel)
	sdatelabel = tklabel(agw,text="startdate ")
	edatelabel = tklabel(agw,text="enddate ")
	
	OnOk=function(bx=tclvalue(boxsel),sdate = tclvalue(sdatesel),edate = tclvalue(edatesel)){
    
	  actogram_plot(act~datetime_,dat = visits_actogram_data(burrow = as.numeric(bx),startd = sdate,endd = edate))
	  
     }
	  OK.but     = tkbutton(agw,text="MAKE ACTOGRAM",command = function() OnOk())
	  Cancel.But = tkbutton(agw,text="CANCEL/DONE",command=function() tkdestroy(agw))
	  
	  tkgrid(boxlabel,boxentry)
	  tkgrid(sdatelabel,sdateentry)
	  tkgrid(edatelabel,edateentry)
	  tkgrid(tklabel(agw,text="    "))
	  tkgrid(OK.but,Cancel.But) 
	  tkgrid.configure(OK.but,column=0)
	  tkgrid.configure(Cancel.But,column=2)
	  }	
	
