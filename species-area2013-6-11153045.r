#########################
#  Species-area  2013-6-11 15:32:37
#########################

setwd("F:/DataW/lg-data/composition")

dir()
## 随机取样 ~~~~~
data0 <- read.csv("lgdat2013.csv")
head(data0) 
data0 <- subset(data0, sp!='00枯立木' & is.na(data0$bra))
##############
if(any(data0$x==0)){data0$x[data0$x==0] <- 0.01}
if(any(data0$y==0)){data0$y[data0$y==0] <- 0.01}
##
#
q1 <- 500/((1:2000)/10)
q2 <- 300/((1:2000)/10)
side00 <- c((1:2000)/10)[q1%%1==0 & q2%%1==0]
# [1]   0.1   0.2   0.4   0.5   0.8   1.0   2.0   2.5
# [9]   4.0   5.0  10.0  12.5  20.0  25.0  50.0 100.0
#
x11()
#
side0=20
#
data1 <- data0
data1 <- data1[data1$dbh>20,]
sp1 <- data1$sp
sx1 <- ceiling(data1$x/side0)
sy1 <- ceiling(data1$y/side0)
ag01 <- aggregate(sp1 ~ sx1 + sy1,FUN=function(xx){length(xx)})
re01 <- reshape(ag01,v.names='sp1',idvar='sx1',timevar='sy1',direction='wide')
image(x=1:dim(re01)[1], y=1:(dim(re01)[2]-1),
      z=as.matrix(re01[, -1]), col = grey((10:0)/15),
      xlab='', ylab='')

#################

#
side0=20
#
data2 <- data0
sp2 <- data2$sp
sx2 <- ceiling(data2$x/side0)
sy2 <- ceiling(data2$y/side0)
ag02 <- aggregate(sp2~ sx2 + sy2,FUN=function(xx){length(unique(xx))})
re02 <- reshape(ag02,v.names='sp2',idvar='sx2',timevar='sy2',direction='wide')
#
x11(5,4.5)
image(x=1:dim(re02)[1], y=1:(dim(re02)[2]-1),
      z=as.matrix(re02[, -1]), col = grey((25:0)/25),
      xlab='', ylab='')

#
x11(5,4.5)
image(x=1:dim(re02)[1], y=1:(dim(re02)[2]-1),
      z=as.matrix(re02[, -1]), col = grey((0:25)/25),
      xlab='', ylab='')

#
#
#
#################
  
SampleRanSqu <- function(data0=data0, side.x=seq(0, 500, by=10),
                         n.rep=10, plotdim=c(500, 300)){
  digits0 <- '%1.f'
  dat0x <- sprintf(digits0, data0$x)
  dat0y <- sprintf(digits0, data0$y)

  sid0.x <- rep(side.x, times=n.rep)
  sid0.y <- rep(side.x/5*3, times=n.rep)
 
  start.x <- sapply(sid0.x,
           function(i)runif(n=1, min=0, max=plotdim[1]-i)
                    )  
  start.y <- sapply(sid0.y,
           function(i)runif(n=1, min=0, max=plotdim[2]-i)
                   ) 
  
  logi0.xy <- lapply(1 : length(sid0.x), function(i){ 
                 seq.x0  <- seq(start.x[i], start.x[i]+sid0.x[i], by=1)
                 seq.y0  <- seq(start.y[i], start.y[i]+sid0.y[i], by=1)
                 logi.x0 <- !is.na(match(dat0x, sprintf(digits0, seq.x0))) 
                 logi.y0 <- !is.na(match(dat0y, sprintf(digits0, seq.y0)))
                 logi.x0 & logi.y0                  }
                     ) 
  
  sp.rich <- sapply(1:length(sid0.x), function(i){
                  length(unique(data0$sp[logi0.xy[[i]]])) 
                                                 })
  sp.indi <- sapply(1:length(sid0.x), function(i){
                  length(data0$sp[logi0.xy[[i]]]) 
                                                 })
  return(list(area=(sid0.x/100)*(sid0.y/100), x=start.x, y=start.y, sp.rich=sp.rich,
              sp.indi=sp.indi) )
}




######################33##############################################


SampleRanSqu0 <- function(data0=data0, side.x=seq(0, 500, by=10),
                         n.rep=10, plotdim=c(500, 300)){
  
  sid0.x <- rep(side.x, times=n.rep)
  sid0.y <- rep(side.x/5*3, times=n.rep)
 
  start.x <- sapply(sid0.x, function(i)runif(n=1, min=0, max=plotdim[1]-i))  
  start.y <- sapply(sid0.y, function(i)runif(n=1, min=0, max=plotdim[2]-i)) 
 
  logi0.xy <- lapply(1 : length(start.x), function(i){ 
          data0$x >= start.x[i] & data0$x <= start.x[i]+sid0.x[i] &
          data0$y >= start.y[i] & data0$y <= start.y[i]+sid0.y[i]
                                                    }
                    )  
  
  sp.rich <- sapply(1:length(start.x), function(i){
        length(unique(data0$sp[logi0.xy[[i]]]))   }
                    )
  
  sp.abun <- sapply(1:length(start.x), function(i){
        length(data0$sp[logi0.xy[[i]]])           }
                    )
  
  return(list(area=(sid0.x/100)*(sid0.y/100), x=start.x, y=start.y, sp.rich=sp.rich,
              sp.abun=sp.abun) )
                                                      }
 
#####################################

system.time(
  resu.all <- SampleRanSqu(data0=data0, side.x=seq(1, 500, by=5), n.rep=50, plotdim=c(500, 300))
)
  system.time(
resu.all0 <- SampleRanSqu0(data0=data0, side.x=seq(1, 500, by=5), n.rep=50, plotdim=c(500, 300))
)

###################################################################
## dput(resu.all,'species-area-result.all.2013-6-12 121424')
## resu.all  <- dget('species-area-result.all.2013-6-12 121424')
###########################################################################
area00 <- resu.all$area 
sp.rich00 <- resu.all$sp.rich 

tiff('AreaSpeciesRich0.tiff',
     width = 3000, height = 2800,res=600,compression = "lzw")
op0 <- par(mex=0.45,mar=c(5.1,5.0,2.3,1))
plot(area00 
     , sp.rich00
     , pch='*'
     , cex=1
     , col=gray(1/80
     , alpha=0.05
                )
     , xlab=expression('面积 Area(' * hm^2 * ')')
     , ylab="物种数 Number of species"
     , cex.lab=1
     , cex.axis=1
)
grid()
#
height <- 24
radius <- 0.8 
x0 <- seq(-1,1,length=50)*radius  
y0 <- height/radius*sqrt(radius^2-x0^2)  
lines(3.5 + x0, 168 + y0, col=1)
lines(3.5 + x0, 168 - y0, col=1)
#
arrows(4.5, 155, 7, 135, angle=23, code=2, length=0.1)
par(op0)
#1
op1 <- par( fig=c(.51 ,.94 ,.16 ,.60) , new=TRUE, mex=0.3, mar=c(7,6,1,1))
unique(area00)
rich01 <- sp.rich00[area00>=3 & area00<=4]
hist(rich01, breaks = 40, xlab = '',  # '物种数 Number of species', 
     ylab = '频数 Frequency' , main = '',
     cex.lab = 0.73,
     cex.axis = 0.7
) 
par(op1) 

dev.off()

################################

summary(resu.all)
x00 <- resu.all$x

rich01 <- sp.rich00[area00>=3 & area00<=4]
x01 <- x00[area00>=3 & area00<=4]
area01 <- area00[area00>=3 & area00<=4]

t01 <- tapply(rich01,area01,mean)
n01 <- names(t01)
  w01 <-lapply(n01,function(xx){which(area01==xx & x01>=t01[xx])})
 hist(x01[unlist(w01)])
 hist(x01[-unlist(w01)])



################################# 

##################################
area00 <- resu.all$area 
sp.abun00 <- resu.all$sp.abun
 
############
tiff('AreaSpeciesIndividual.tiff',
     width = 3000, height = 2800,res=600,compression = "lzw")
op0 <- par(mex=0.45,mar=c(5.1,5.0,2.3,1))
plot(area00, sp.abun00
     , pch='*'
     , cex=0.8
     , col=gray(1/80
        # , alpha=0.15
                )
     , xlab=expression('面积 Area(' * hm^2 * ')')
     , ylab="个体数 Number of individuals"
     , cex.lab=1
     , cex.axis=1
)
abline(lm(sp.abun00 ~ area00), col=gray(5/8), lwd=2)
grid()
#
height <- 3800
radius <- 0.58 
x0 <- seq(-1,1,length=50)*radius  
y0 <- height/radius*sqrt(radius^2-x0^2)  
lines(3.55 + x0, 15800 + y0, col=1)
lines(3.55 + x0, 15800 - y0, col=1)
arrows( 3.65,21000, 4.6,35000,angle=30,code=2,length=0.10)
par(op0)
#
op1 <- par( fig=c(.13 , .57, .56 , .95) , new=TRUE, mex=0.3, mar=c(5,6,1,1))
sp.abun01 <- sp.abun00[area00>=3 & area00<=4]
hist(sp.abun01, breaks = 40, xlab = '',  # "个体数 Number of individuals", 
     ylab = '频数 Frequency' , main = '',
     cex.lab = 0.73,
     cex.axis = 0.7
) 
par(op1)  
dev.off()


##################################################




