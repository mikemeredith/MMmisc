


leaf.colors <- function(n, alpha = 1)
{
  j <- n%/%4 ; i <- n - j
  c(rgb( 255, 255, seq(220,0,length=j), alpha*255, maxColorValue=255),
    rgb( seq(255,0,length=i), seq(255,100,length=i), seq(0,50,length=i), alpha*255, maxColorValue=255))
}



