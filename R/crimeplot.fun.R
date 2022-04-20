
crimeplot.fun <- function(select.crimes=c('DISORDERLY CONDUCT'), opacity1=0.1){
  sub1<-n2[n2$NIBRS_Offe %in% select.crimes,]
  
  p1 <- ggplot(sub1, aes(x=x.jit, y=y.jit)) +
    geom_point( col='red',
                alpha=opacity1,
                cex=0.5, #Size of the markers
                pch=16) +
    #theme_classic()+
    theme_void()+
    geom_polygon(data=shp.nhv2, aes(x=long, y=lat, group=group), fill=NA, colour='black', alpha=0) 
  return(p1)
}
