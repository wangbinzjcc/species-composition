###############
djt<-read.csv("E.csv")
djt<-as.list(djt[,c(2,3,7)])
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
djt.smooth <- with(djt, interp(x, y, z, xo=seq(x.min+0.2,x.max-0.2, length=(x.max-x.min)*20),
                    yo=seq(y.min+0.2,y.max-0.2, length=(y.max-y.min)*20),linear=T))
##
zz<-djt.smooth$z
xx <- (1:nrow(zz))
yy <- (1:ncol(zz))
###############

#
png("弄岗样地--等高线图.png",width = 880, height = 680)

image(xx, yy,zz, col = terrain.colors(100), axes = F,xlab="EW",ylab="SN")
contour(xx, yy,zz, levels = seq(z.min, z.max, by = (z.max-z.min)/10),
        add = TRUE, col = "peru")
axis(1, at = seq(0, 500, by = 50),)
axis(2, at = seq(0, 300, by = 50),)
box()
title(main = "弄岗样地--凹凸度", cex.main = 1.5,font.main = 4)

#
dev.off()
################
