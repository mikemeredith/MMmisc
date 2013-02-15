detachSelect <-
function() {
   tmp <- search()
	present <- tmp[2:(length(tmp)-1)]
   choice <-  select.list(present, multiple=TRUE,
		title="Select items to detach")
   if(length(choice) == 0) {
      cat("No objects selected for detachment.\n")
      return(invisible(NULL)) # invisible inserted 080603
   }
   for(to.rm in choice)
      while(to.rm %in% search()) {
#         cat("Detaching:", to.rm, "\n")
         eval(substitute(detach(n), list(n=to.rm)))
     }
   return(choice)
}
