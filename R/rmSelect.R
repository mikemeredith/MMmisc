rmSelect <-
function() {
   present <- ls(1)
   if(length(present) ==0) {
      cat("Nothing to remove\n")
      return(invisible(NULL)) # invisible inserted 080603
   }
   choice <-  select.list(present, multiple=T, title="Select items to remove")
   if(length(choice) == 0) {
      cat("No deletions selected\n")
      return(invisible(NULL)) # invisible inserted 080603
   }
   for(to.rm in choice)
      while(to.rm %in% search()) {
         cat("Detaching:", to.rm, "\n")
         eval(substitute(detach(n), list(n=to.rm)))
     }
   cat("Removing:\n")
   rm(list=choice, pos=1)
   return(choice)
}
