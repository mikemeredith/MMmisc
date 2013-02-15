dumpFunc <-
function(name) {
  if(!is.character(name))
    name <- deparse(substitute(name))  # Added 7 Sept 2012
  file.name <- paste("fun_", name, ".R", sep="")
  dump(name, file=file.name)
}
