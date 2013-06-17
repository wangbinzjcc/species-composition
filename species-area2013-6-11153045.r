#########################
#  Species-area  2013-6-11 15:32:37
#########################

setwd("F:/DataW/lg-data/composition")

dir()
## 随机取样 ~~~~~
data0 <- read.csv("lgdat2013.csv")
head(data0) 
data0 <- subset(data0, sp!='00枯立木' & is.na(data0$bra))

  
SampleRanSqu <- function(data0=data0, side.x=seq(0, 500, by=50),
                         n.rep=3, plotdim=c(500, 300)){
  digits0 <- '%1.f'
  dat0x <- sprintf(digits0, data0$x)
  dat0y <- sprintf(digits0, data0$y)
  
  sid0.x <- rep(side.x, times=n.rep)
  sid0.y <- rep(side.x/5*3, times=n.rep)
 
  start.x <- sapply(plotdim[1] - sid0.x,
              function(i)runif(n=1, min=0, max=i)
                    )  
  start.y <- sapply(plotdim[2] - sid0.y,
              function(i)runif(n=1, min=0, max=i)
                   ) 
  logi.xy <- lapply(1 : length(sid0.x), function(i){ 
                 seq.x0 <- seq(start.x[i], start.x[i]+sid0.x[i], by=1)
                 seq.y0 <- seq(start.y[i], start.y[i]+sid0.y[i], by=1)
                logi.x0 <- !is.na(match(dat0x, sprintf(digits0, seq.x0))) 
                logi.y0 <- !is.na(match(dat0y, sprintf(digits0, seq.y0)))
                logi.x0 & logi.y0                }) 
  
  sp.rich <- sapply(1:length(sid0.x), function(i){
                  length(unique(data0$sp[logi.xy[[i]]])) 
                                                 })
  sp.indi <- sapply(1:length(sid0.x), function(i){
                  length(data0$sp[logi.xy[[i]]]) 
                                                 })
  return(list(area=(sid0.x/100)*(sid0.y/100), x=start.x, y=start.y, sp.rich=sp.rich,
              sp.indi=sp.indi) )
}

resu.all <- SampleRanSqu(data0=data0, side.x=seq(1, 500, by=2), n.rep=100, plotdim=c(500, 300))
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


################################# 

##################################
area00 <- resu.all$area 
sp.indi00 <- resu.all$sp.indi
 
############
tiff('AreaSpeciesIndividual.tiff',
     width = 3000, height = 2800,res=600,compression = "lzw")
op0 <- par(mex=0.45,mar=c(5.1,5.0,2.3,1))
plot(area00, sp.indi00
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
abline(lm(sp.indi00 ~ area00), col=gray(5/8), lwd=2)
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
sp.indi01 <- sp.indi00[area00>=3 & area00<=4]
hist(sp.indi01, breaks = 40, xlab = '',  # "个体数 Number of individuals", 
     ylab = '频数 Frequency' , main = '',
     cex.lab = 0.73,
     cex.axis = 0.7
) 
par(op1)  
dev.off()


##################################################




