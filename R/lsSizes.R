lsSizes <-
function(name=1) {
   present <- ls(name)
   z <- round(unlist(lapply(present, function(x) object.size(get(x)))) / 1024, 2)
   names(z) <- present
   data.frame(Kb=sort(z, decreasing=TRUE))
}
