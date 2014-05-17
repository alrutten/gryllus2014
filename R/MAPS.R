burrow_data = function(db = get("database_path", envir = .gr)) {

d = sqliQuery(db, "SELECT b.burrow, c.xcoord,c.ycoord from BURROWS b
                   LEFT JOIN Coordinates c on b.blockID = c.block AND b.x = c.x AND b.y=c.y") 
				   }
				   
burrow_map = function(d=burrow_data(), Pdf = TRUE) {

if (Pdf) {
        f = paste(tempfile(), "pdf", sep = ".")
        pdf(f, width = 10, height = 10, title = date(), paper = "a4", 
            useDingbats = FALSE)
     
    }
	 par(mar = c(0, 0, 0, 0))
	 
	 plot(c(-1:25),c(-1:25),type='n')
	 text(x=c(1:20)-0.5,y = 25,labels = rep(c(1:10)),col='grey',pos=4)
	 text(x=22,y=c(24:1)-0.5,labels = rep(c('A','B','C','D','E','F','G','H','I','J','K'
,'L')),col='grey',pos=3)
      abline(v = c(0.5,10.5,20.5), h= c(0.5,12.5,24.5),col='dark grey',lwd=2)
	  abline(v = c(1:20)-0.5, h= c(1:24)-0.5,col='grey')
	 points(d$xcoord,d$ycoord, pch=19)
	
	 text(d$xcoord,d$ycoord,d$burrow,pos = 2)
	 text(21,-1,labels=Sys.time())
	 
	  if (Pdf) {
        dev.off()
        shell.exec(f)
    }
	}

				   