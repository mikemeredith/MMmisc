rattach <-
function(db, pos=2, overwrite=NA, ...) { 
   name <- deparse(substitute(db))
   if(!exists(name)) stop(paste("object \"", name, "\" not found", sep=""))
   # Check .GlobalEnv for identical names # pinched from R2WinBUGS
    rem <- names(db) %in% ls(.GlobalEnv)
    if (!any(rem)) 
        overwrite <- FALSE
    rem <- names(db)[rem]
    if (is.na(overwrite)) {
        question <- paste(
"The following objects in .GlobalEnv will mask
objects in the attached database:\n\n", 
            paste(rem, collapse = "\n"),
               "\n\nRemove these objects from .GlobalEnv?", 
            sep = "")
        if (interactive()) {
            if (.Platform$OS.type == "windows") 
                overwrite <- "YES" == winDialog(type = "yesno", 
                  question)
            else overwrite <- 1 == menu(c("YES", "NO"), graphics = FALSE, 
                title = question)
        }
        else overwrite <- FALSE
    }
    if (overwrite) 
        remove(list = rem, envir = .GlobalEnv)

   det <- FALSE   # flag to show if object was detached before (re)attaching.
   while(name %in% search()) {
      cat("Detaching   :", name, "\n")
      det <- TRUE
      eval(substitute(detach(n), list(n=name)))
   }
   if(det)
      cat("Reattaching :", name, "\n")
   attach(db, pos=pos, name=name, ...)
}
