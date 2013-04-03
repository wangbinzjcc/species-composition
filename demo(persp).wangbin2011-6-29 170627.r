
###############
djt<-read.csv("??ƺ?߳?.csv")
djt<-as.list(djt[,c(1,2,4)])
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
djt.smooth <- with(djt, interp(x, y, z, xo=seq(x.min+0.2,x.max-0.2, length=(x.max-x.min)*10),
                    yo=seq(y.min+0.2,y.max-0.2, length=(y.max-y.min)*10),linear=T))
##
z<-djt.smooth$z
x <- (1:nrow(z))/10
y <- (1:ncol(z))/10

#########
###................................................................
require(datasets)
require(grDevices)
require(graphics)
 z0 <- min(z) - 5
 z <- rbind(z0, cbind(z0, z, z0), z0)
 x <- c(min(x) - 1e-10, x, max(x) + 1e-10)
 y <- c(min(y) - 1e-10, y, max(y) + 1e-10)
 fill <- matrix("green3", nrow = nrow(z)-1, ncol = ncol(z)-1)
 fill[ , i2 <- c(1,ncol(fill))] <- "gray"
 fill[i1 <- c(1,nrow(fill)) , ] <- "gray"
## `image like' colors on top :
 fcol <- fill
 zi <- djt.smooth$z[ -1,-1] + djt.smooth$z[ -1,-ncol(djt.smooth$z)] +
           djt.smooth$z[-nrow(djt.smooth$z),-1] + djt.smooth$z[-nrow(djt.smooth$z),-ncol(djt.smooth$z)] 
 fcol[-i1,-i2] <- terrain.colors(20)[cut(zi,
                            stats::quantile(zi, seq(0,1, length.out = 21)),
                            include.lowest = TRUE)]
###``````````````````````````````````````````````````````````````````
png("??ƺ????--????ͼ1.png",width = 680, height = 680)
 persp(x, y, z, theta = 38, phi =18,expand = 0.1, col = fcol, scale = FALSE,
       ltheta = -120, shade = 0.4, border = NA, box = T,ticktype = "detailed", 
 xlab = "׮?ţ?x??", ylab = "׮?ţ?y??", zlab ="????(m)",cex.axis=1, cex.lab=1.6)

title(main = "??ƺ????--????ͼ",cex.main = 1.8,font.main = 4)
#
dev.off()

 