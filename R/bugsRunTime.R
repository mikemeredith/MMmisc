bugsRunTime <-
function(look.in=NULL){
   if(is.null(look.in))
      look.in <- c(tempdir(), getwd())
   for(dir in look.in)  {
      logtime <- file.info(file.path(dir, "log.txt"))$mtime
      if(!is.na(logtime)) break
   }
   if(!is.na(logtime)) {
      cat("WinBUGS run terminated at", strftime(logtime), "\n")
      runtime <- round(logtime - 
            file.info(file.path(dir, "script.txt"))$mtime, 2)
   } else {
      cat("'log.txt' and 'script.txt' not found.\n")
      runtime <- NA
   }
   return(runtime)
}
