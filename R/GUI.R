
#Messages

startMessages = function() {


Message(is.friday())


}

gr =  function () 
{
  shell(system.file('Utils_d_d_d.exe', package='gryllus'),wait=FALSE)
  
    if (exists("guiMsgs", envir = .gr)) 
        assign("guiMsgs", NULL, envir = .gr)
    startMessages()
    tt = tktoplevel() 
    tkwm.title(tt, paste("cricket data management", packageDescription("gryllus")$Version))
  
    
        MENUS = tkframe(tt, relief = "ridge", borderwidth = 2, 
            background = "grey97")
			
		dataEntry = tkmenubutton(MENUS, text = "DATA\nENTRY", 
            background = "grey97")
        dataEntryMenu = tkmenu(dataEntry, tearoff = TRUE)
        tkconfigure(dataEntry, menu = dataEntryMenu)
		 tk2tip(dataEntry, paste("database interface functions!"))
        tkpack(dataEntry, side = "left")
        tkadd(dataEntryMenu, "command", label = "Open sqliteStudio", 
            command = function() {dataGUI()
			                      gryllus::backup()})
        
		
        
        #tk2tip(nopec, paste("this is not clickable."))
       
        set = tkbutton(MENUS, text = "Change\nsettings", command = function() changeSettings(envir=.gr))
        #tk2tip(set, paste("change package settings (use with care)"))
        tkpack(set, side = "right")
        tkpack(MENUS, fill = "x")
 #MAPS & PLOTS
 
       frmx2 = tkframe(tt, relief = "ridge", borderwidth = 2, 
            background = "grey92")
        labx2 = tklabel(frmx2, text = "MAPS! PLOTS!", 
            font = "Arial 14 bold", foreground = "navyblue", 
            background = "grey92")
        tkgrid(labx2, sticky = "e", rowspan = 1)
        mapsBut = tkbutton(frmx2, relief = "groove", text = "MAP GRID", 
            command = function() burrow_map())
        
         actogramBut = tkbutton(frmx2,relief = 'groove', text = 'ACTOGRAMS', command = function() actogramGUI() )			
       tkgrid(mapsBut, row = 0, column = 1, sticky = "e", padx = 3, 
            pady = 1)
	   tkgrid(actogramBut, row = 0, column = 2, sticky = "e", padx = 3, 
            pady = 1)
        tkpack(frmx2, fill = "x")

  #DEVICES
  frmx1 = tkframe(tt, relief = "ridge", borderwidth = 2, 
            background = "grey92")
        labx1 = tklabel(frmx1, text = "DATA UPLOAD ", font = "Arial 14 bold", 
            foreground = "navyblue", background = "grey92")
        tkgrid(labx1, sticky = "e", rowspan = 1)
        rfid = tkmenubutton(frmx1, relief = "groove", text = "RFID DATA\n")
        
        #tk2tip(rfid, paste("download feeder data to server/ upload feeder data from server to loggerdatabase"))
       
  	    rfidMenu = tkmenu(rfid)
        tkconfigure(rfid, menu = rfidMenu)
        tkadd(rfidMenu, "command", label = "download to server", 
            command = function() getSDGui())
			 tkadd(rfidMenu, "command", label = "find new files that i accidentally copied without using the menu option above", 
            command = function() fs.findnew())
        tkadd(rfidMenu, "command", label = "upload all files to database", 
            command = function() loadAll())
        tkgrid(rfid, row = 0, column = 1, sticky = "e", padx = 3, 
            pady = 1)
        
        tkpack(frmx1, fill = "x")  
   
    {
        msg = tkframe(tt, relief = "ridge", borderwidth = 2, 
            background = "grey92")
        scr = tkscrollbar(msg, repeatinterval = 1, command = function(...) tkyview(txt, 
            ...))
        txt = tktext(msg, bg = "white", font = "courier", yscrollcommand = function(...) tkset(scr, 
            ...))
        tkgrid(txt, scr, column = 1, row = 1, sticky = "ns")
        x = unique(get("guiMsgs", envir = .gr))
        x = paste("-", x, "\n", collapse = "")
        if (length(x > 0)) {
            for (i in 1:length(x)) tkinsert(txt, "end", x[i])
        }
        tkconfigure(txt, state = "disabled")
        tkfocus(txt)
        tkpack(msg, fill = "x")
    }
    {
        OnOK = function() {
            sys.source(system.file("gr.settings.R", package = "gryllus"), 
                envir = .gr)
            tkdestroy(tt)
            gr()
        }
        frmx3 = tkframe(tt, relief = "ridge", borderwidth = 2, 
            background = "grey92")
        butx3 = tkbutton(frmx3, text = "[ RESET ]", command = OnOK, 
            font = "Arial 14 bold", foreground = "red")
        tkgrid(butx3, row = 2, column = 2)
        tkpack(frmx3, fill = "x")
    }
}









 
 