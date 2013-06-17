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

############## 
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
          pch=list(8, 3, 1)[[i]], cex=c(0.4, 0.7, 1.2)[i], 
          col= grey(c(3/10,1.5/10,0/10), alpha=c(0.3,0.7,1))[i]
           )    }       } 
#
#````````````````````````````````````````````````````` 

   for(i in 1:30){ 
     dat01 <- dat00[dat00$sp==nam00[i],]
     dat.dbh <- cut(dat01$dbh,c(-Inf,5,20,Inf ))
     levels(dat.dbh) <- 1:3
     bb00 <- substitute(paste(x,'  ',italic(y)), 
                        list(x=as.character(nam00[i]), 
                        y=as.character(hys00$sp.ld[i]))
                        )
      tiff(paste(nam00[i],'.tiff'), width = 2100, height = 1600,res=600,compression = "lzw")
    # x11(5,4)
     par(mex=0.45,mar=c(3,3,3,0.5))
     contour(xx, yy,zz, levels = seq(round(z.min), z.max, by = 20),
             add = F, col = "peru",lwd=1,labcex=0.1, cex.axis=0.9, yaxt="n")
     axis(side=2,at=seq(0,300,100), cex.axis=0.9)
     PointCont() 
     mtext(bb00, side=3, line = .5,  at = 260, cex = 1.0 ,font =2 ) 
     dev.off()
              }

#############################################################

######################
x11(5,3.5)
par(fig=c(0, 0.95 ,0 , 1), mex=0.45,mar=c(5.1, 5.3, 1,0))
image(x, y, z, col = gray(35:95/100), axes = F
      , xlab="米  (m)", ylab="米  (m)"
      , cex.lab=0.8
      )
contour(xx, yy,zz, levels = seq(round(z.min,-1), z.max, by = 10),
        add = T, col = 1,lwd=1.2,labcex=0.65)
axis(1, cex.axis=0.8, at = seq(3, 500, length.out = 11), labels= seq(0, 500, by = 50))
axis(2, cex.axis=0.8, at = seq(3, 300, length.out = 7), labels = seq(0, 300, by = 50))
box()
op1 <- par( fig=c(.89, 1 ,0 ,1), new=T, mex=0.3, mar=c(7,6,1,1))
plot(c(1,1), 1:2, type='n', axes=F, xlab='', ylab='')
arrows(1, 1.6, 1, 1.8, angle=18, code=2, length = 0.15, lwd = 1.5)
text(1, 1.85, "N", cex=1.1)
 
####################################################### 





setwd("F:/lg-data/composition")
data <- read.csv("lgdat2013.csv")
head(data) 
da0 <- subset(data, !is.na(bra))
#da0 <- subset(data, sp!= '00枯立木'& is.na(bra))


da0$dbh[da0$dbh<1] <- 1
summary(da0$dbh)

head(da0)
x11(3.5, 2.8)

op0 <- par(mex=0.5,mar=c(3,3,1,1))

with(subset(da0,dbh>0 & dbh<5), 
     plot(x, y
          , ylim=c(0,300), xlim=c(0,500)
          ,ylab='',xlab=''
          , cex.axis=0.7
          ,col=1
          , pch='+', cex=0.32)
)

contour(xx, yy,zz, levels = seq(round(z.min), z.max, by = 20),
        add = T, col = "peru",lwd=0.5,labcex=0.1)
 
x11(3.5, 2.8)

op0 <- par(mex=0.5,mar=c(3,3,1,1))

with(subset(da0,dbh>=5 & dbh<20), 
     plot(x, y
          , ylim=c(0,300), xlim=c(0,500)
          ,ylab='',xlab=''
          , cex.axis=0.7
          ,col=grey(3/10)
          ,lwd=1
          , pch=3, cex=0.3)
) 

contour(xx, yy,zz, levels = seq(round(z.min), z.max, by = 15),
        add = T, col = "peru",lwd=1,labcex=0.1)

with(subset(da0,dbh>=20), 
     points(x,y,ylim=c(0,300),xlim=c(0,500),pch=1,lwd=2,cex=1,col=1)
) 

par(op0)

##################################################################











