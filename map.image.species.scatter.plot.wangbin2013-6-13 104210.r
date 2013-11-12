###########################################################
# Nonggang map, Species scatter plot, Branch plot, Image
###########################################################
#..................
setwd("F:/DataW/lg-data/composition")
dir()


###############
djt<-read.csv("elev5m2013.csv")
head(djt)
djt<-as.list(djt[,c(2,3,4)])
library(akima)
##
x.min<-min(djt$x)
x.max<-max(djt$x)
##
y.min<-min(djt$y)
y.max<-max(djt$y)
##
djt$z<-djt[[3]]
z.min<-round(min(djt$z))
z.max<-max(djt$z)
##
djt.smooth <- with(djt, interp(x, y, z, xo=seq(x.min+0.2,x.max-0.2, length=(x.max-x.min)/5),
                    yo=seq(y.min+0.2,y.max-0.2, length=(y.max-y.min)/5),linear=T))
##
zz<-djt.smooth$z
dim(zz)
xx <- (1:nrow(zz))*5
yy <- (1:ncol(zz))*5 

##############################################################
######## 
dat00 <- read.csv('lgdat2013.csv')
dat00 <- dat00[is.na(dat00$bra),]
hys00 <- read.csv('hysIV2013.csv')
nam00 <- as.character(hys00$Species)
# ................................................
#
PointCont <- function(){
    for(i in 1:3){
    dat02 <- dat01[dat.dbh==i,]
    points(dat02$x, dat02$y, 
          pch=list(8, 3, 1)[[i]], cex=c(0.2, 0.5, 1)[i], 
          col= grey(c(3/10,1.5/10,0/10), alpha=c(0.3,0.7,1)
                    )[i]
           )    }       } 
#
#````````````````````````````````````````````````````` 
nam.1 <- c('黄梨木', '细叶谷木', 
           '山榄叶柿', '蚬木',  '闭花木', '肥牛树',
           '日本五月茶', '劲直刺桐', '对叶榕'
           )
windowsFonts(Times = windowsFont("Times New Roman"))
   for(i in 1:length(nam.1)){ 
     dat01 <- dat00[dat00$sp==nam.1[i],]
     dat.dbh <- cut(dat01$dbh,c(-Inf,5,20,Inf))
     levels(dat.dbh) <- 1:3
     bb00 <- substitute(paste(x,'  ', italic(y)), 
                        list(x=as.character(nam.1[i]), 
                        y=as.character(hys00$sp.ld[i]))
                        )
 tiff(paste(nam.1[i],'.tiff'),
      family= "Times", pointsize=8,
      width = 56, height = 43, units = "mm",
      res=600,compression = "lzw"
      )
     par(mex=0.31,mar=c(0.5, 0.5, 4.0, 0.5))
     contour(xx, yy,zz, levels = seq(round(z.min), z.max, by = 20),
             add = F, col = "peru",lwd=0.6,labcex=0.1,
             yaxt="n", xaxt="n")
     PointCont() 
     mtext(bb00, side=3, line = .5,  at = 260, cex = 1.0 ,font =2 
           ) 
 dev.off()
                }
#
#############################################################
###
windowsFonts(Times = windowsFont("Times New Roman"))
#
tiff('MapPlotImageNongGang.tiff',
     family="Times", pointsize=8,
     width = 80, height = 55, units = "mm",
     res=600,compression = "lzw")
par(fig=c(0, 0.95 ,0 , 1), mex=0.4,mar=c(5.5, 5.3, 1.5,0))
image(xx, yy, zz, col = gray(35:95/100), axes = F
      , xlab="米  (m)", ylab="米  (m)"
      , cex.lab=1
      )
contour(xx, yy,zz, levels = seq(round(z.min,-1), z.max, by = 10),
        add = T, col = 1,lwd=0.6,labcex=0.65)
contour(xx, yy,zz, levels = 320,
        add = T, col = 1,lwd=1.5,lty=2, labcex=0.65)
contour(xx, yy,zz, levels = 240,
        add = T, col = 1,lwd=1.5,lty=2,labcex=0.65)
axis(1, cex.axis=0.8, at = seq(3, 500, length.out = 11), labels= seq(0, 500, by = 50))
axis(2, cex.axis=0.8, at = seq(3, 300, length.out = 7), labels = seq(0, 300, by = 50))
box()
op1 <- par( fig=c(.89, 1 ,0 ,1), new=T, mex=0.3, mar=c(7,6,1,1))
plot(c(1,1), 1:2, type='n', axes=F, xlab='', ylab='')
arrows(1, 1.6, 1, 1.8, angle=18, code=2, length = 0.10, lwd = 1)
text(1, 1.85, "N", cex=1)
dev.off()
#
####################################################### 
#










