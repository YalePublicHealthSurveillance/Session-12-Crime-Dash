
crimeplot.fun <- function(ds=sub1, opacity1=0.1){

  p1 <- ggplot(ds, aes(x=x.jit, y=y.jit)) +
    geom_point( col='red',
                alpha=opacity1,
                cex=0.5, #Size of the markers
                pch=16) +
    #theme_classic()+
    theme_void()+
    geom_polygon(data=shp.nhv2, aes(x=long, y=lat, group=group), fill=NA, colour='black', alpha=0) 
  return(p1)
}
