.onLoad =  function(...) { 
    
	 CRANdep   = c("tcltk","tcltk2","RSQLite","RSQLite.extfuns","plyr","lattice","grid")
	# Rforgedep = c('actogram')
     MPIOdep   = c('utilsMPIO')	 
	 installed.packs = row.names(installed.packages())
	 for(i in 1:length(CRANdep)) { 
	 if (!CRANdep[i]%in%installed.packs) install.packages(CRANdep[i],repos='http://cran.us.r-project.org')
	 require(CRANdep[i], character.only = TRUE) 
	 }
	 #for(i in 1:length(Rforgedep)) { 
	 #if (!Rforgedep[i]%in%installed.packs) install.packages(Rforgedep[i], repos = "http://R-Forge.R-project.org")
	 #require(Rforgedep[i], character.only = TRUE) 
	 #}
	  for(i in 1:length(MPIOdep)) { 
	 if (!MPIOdep[i]%in%installed.packs) install.packages(MPIOdep[i], contriburl ='http://scidb.orn.mpg.de/scidbextras/SOFTWARE/R_repository')
	 require(MPIOdep[i], character.only = TRUE) 
	 }
    # load settings into dedicated environment
	assign(".gr", new.env(), envir = .GlobalEnv )
	sys.source( system.file("gr.settings.R", package = "gryllus2014"), envir = .gr)
	sep = paste("\n", paste(rep("-", 61), collapse = ""), "\n", 
        collapse = "")
	cat(paste(sep, "This is gryllus", packageDescription("gryllus2014")$Version, "\n\nType", sQuote("gr()"), 
        "to start the graphical user interface \n", 
        sep))
		
		gr()


} 

