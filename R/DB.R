backup = function(db = get('database_path',envir=.gr), backup = get('backup_path',envir = .gr)) {

oldname = paste(backup,basename(db),sep='/')
newname = paste(backup,format(Sys.time(),'%Y%m%d_%H%M%S_gryllus_backup.sqlite',),sep='/')
file.copy(db,oldname,overwrite = TRUE)
file.rename(oldname,newname)
}