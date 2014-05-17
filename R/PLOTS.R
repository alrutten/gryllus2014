# TODO; COG, circular mean, etc
# TODO; day, night bar
# TODO: avoid data doubling for doublePlot = TRUE 


actogram <- function(formula, 
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
 
  events    = sqliQuery(lb,paste("SELECT distinct burrow, datetime(round(CAST(strftime('%s.%f',datetime_)/60 as REAL)/1,0)*1*60,'unixepoch') as datetime_, transp from RFID
	                             where  datetime_>=",shQuote(startd),
	                             " AND datetime_<=",shQuote(endd),
								 " AND datetime_ IS NOT NULL",
								 " AND burrow = ",burrow,
								 " ORDER BY burrow,datetime_",sep=''))
  
#  testtr = sql(con,"SELECT transponder from test_transponders")
#  events$transp[events$transp%in%testtr$transp]= 'visit'
#  events$transp[which(is.na(events$transp))] = ''
  
  events$datetime_ = as.POSIXct(events$datetime_)
  events$day       = as.Date(trunc(events$datetime_, "day"))
  events$time      = as.numeric(difftime(events$datetime_,trunc(events$datetime_,"day"),   #DO NOT USE events$day here!! (the time will be wrong)
                                         units = "hours"))  
 
  return(events)
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
    
	  actogram_plot(d = visits_actogram_data(burrow = as.numeric(bx),startd = sdate,endd = edate))
	  
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
	
