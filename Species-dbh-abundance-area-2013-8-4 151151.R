###############################################################
#  species  dbh  abundance rich area  2013-6-12 15:51:31          #
###############################################################
setwd("F:/DataW/lg-data/composition")
dir()
dat <- read.csv('lgdat2013.csv')
head(dat)
dat01 <- subset(dat, sp!='00枯立木')
dat00 <- subset(dat01,is.na(bra))

##
head(dat00)
levels(dat00$sp) 

dim(dat00)
unique(dat00$sp)
############################################################
 

#
DoPoint <- function(xm01, den00, TEXT=1){
  Tex0 <- ifelse(TEXT==1
              , sprintf('(%s, ln%s)', xm01, round(exp(den00[xm01])))
              , sprintf('(%s, %s%%)', xm01, round(den00[xm01],2))
                )
  #
  points(xm01,den00[xm01], col=2)  
  segments(xm01, -1,xm01,den00[xm01], lty=2, col=1)
  segments(-6, den00[xm01], xm01, den00[xm01], lty=2, col=1)
  x0 <- ifelse(TEXT==1, 25, 35)
  y0 <- ifelse(TEXT==1, 0.4, -0.1) 
  text(xm01+x0,den00[xm01]+y0, Tex0, cex=0.9  )    
                                        }
####

# species-abundance

tab00 <- sort(table(dat00$sp), decreasing = T)
sum(tab00)
ke.hys <- read.csv("distru.ke.shu.hys2013-2-19 170601.csv")
head(ke.hys)

m.order <- match(names(tab00), ke.hys$种名)
ke00 <- ke.hys[m.order, ]
tab01 <- cbind(tab00,ke00)
head(tab01)
sum(tab01$tab00)
dir()
write.csv(tab01,'species abundance01.csv')

 t.1 <- length(which(tab00>15))+1
 t.10 <- length(which(tab00>150))+1
############
#x11(3,2.8)
windowsFonts(Times = windowsFont("Times New Roman"))
tiff('lnAbundanceSpecies00.tiff',
     family="Times", pointsize=8,
     width = 80, height = 70, units = "mm",
     res=600,compression = "lzw")
par(mex=0.45,mar=c(5.1, 5.1,1,1)) 
plot(log(tab00)
     , type='p'
     , cex=0.7
     , lwd=0.5
     , cex.lab=1
     , cex.axis=0.9
     ,ylab="多度(取对数) Ln(abundance)"
     ,xlab="物种多度序列 Species rank in abundance"
)
DoPoint(xm01=t.1, den00=log(tab00), TEXT=1)
DoPoint(xm01=t.10, den00=log(tab00), TEXT=1)
dev.off()
#############################################
#
sum(length(tab00))
223+1-t.1
223+1-t.10 
(223+1-t.1)/223
(t.1 - t.10)/223
65+83+75
############################################################
### 
tab01 <- cumsum(tab00)*100/sum(tab00)
t5 <- length(which(tab01<50))+1
t9 <- length(which(tab01<90))+1
###########
#################
#x11(3,2.8)
tiff('CumulativeAbundanceSpecies.tiff',
     family="Times", pointsize=8,
     width = 80, height = 70, units = "mm",
     res=600,compression = "lzw")
par(mex=0.45,mar=c(5.1, 7.2, 1, 1))
plot(tab01
     , type='p'
     , cex=0.7
     , lwd=0.5
     , cex.lab=1
     , cex.axis=0.9
     ,ylab="累计百分比多度 \n Cumulative percentage of abundance(%)"
     ,xlab="物种多度序列 Species rank in abundance"
     )
#grid()
DoPoint(xm01=t5, den00=tab01, TEXT=2)
DoPoint(xm01=t9, den00=tab01, TEXT=2)
dev.off()


##############################################################################
#  物种名、科、多度、分枝、平均胸高断面积、频度、重要值。   生境类型
#
#.......................................................................
IV.Calc <- function( datas ,side = 20){
  if(any(datas$x==0 | datas$y==0)){
    datas[datas$x==0,'x'] <- 0.005 ; datas[datas$y==0,'y']<- 0.005
                                  }
  # abundance of the trees without branches  
  dat.ubr <- datas[is.na(datas$bra),]
  sp.abu <- table(dat.ubr$sp)
  rel.den <- sp.abu*100/sum(sp.abu, na.rm = T)
  
  # abundance of the branches  
  dat.bra <- datas[!is.na(datas$bra),]
  bra.abu <- table(dat.bra$sp)
  
  # Relative dominance  
  sp.dbh2 <- tapply(datas$dbh ,datas$sp ,function(x){sum(pi*x^2/4)})
  rel.dom <- sp.dbh2*100/sum(sp.dbh2, na.rm = T)
  
  # Means of dbh^2 of each tree
  mean.dbh2 <- tapply(datas$dbh ,datas$sp ,function(x){mean(pi*x^2/4)})
  
  # Relative frequency：
  sp.fre <-tapply( paste(ceiling(datas$x/side) ,ceiling(datas$y/side))
                   ,datas$sp ,function(x)sum(table(x)>0) )
  rel.fre <- sp.fre*100/sum(sp.fre, na.rm = T)
  
  # Result：
  results <- data.frame(sp.abu=as.numeric(sp.abu)
                        , bra.abu=as.numeric(bra.abu)
                        , mean.dbh2=mean.dbh2
                        , rel.fre=rel.fre  
                        , IV=as.numeric(rel.den+rel.dom+rel.fre)/3
                        )
  results <- results[order(-results$IV),]
  # Output:
  return(round(results,3))
                                                           }
#``````````````````````````````````````````````````````````````````````````````
#
re00 <- IV.Calc(datas=dat00, side=20)
#
dir()
hys00 <- read.csv('spe_cod_Hys-2012-10-26 110946.csv') 
#
mat00 <- match(rownames(re00),hys00$种名)
re00$ke <- hys00$科拉丁名[mat00]
re00$zhong <- hys00$种拉丁名[mat00]
head(re00)
#
#################################################################
dir()
ca.spe <- read.csv('species .csv')
iv.spe <- read.csv('IV.RESUL201323204839.csv')
head(iv.spe)
ma00 <- match(iv.spe$X, ca.spe$X)
iv.spe$CCA1 <- ca.spe$CCA1[ma00]
iv.spe$CCA2 <- ca.spe$CCA2[ma00]
#
write.csv(iv.spe, "IV.RESUL201323224033.csv")
###############################################################
#
# Species-Area curve
#
setwd("F:/DataW/lg-data/composition")

dir()
  ## 随机取样 ~~~~~
  data <- read.csv("lgdat2013.csv")
head(data) 
data <- subset(data, sp!='00枯立木' & is.na(data$bra))
 
##   Single sampling ~~~~
SampleRanSqu <- function(data=data, side.x=seq(1, 500, by=5)
                         , n.rep=20, plotdim=c(500, 300)){
  side.y <- side.x/5*3
  
  xlo <- sapply(plotdim[1] - side.x
                , function(i)runif(n.rep, min=0, max=i)
                )
  
  ylo <- sapply(plotdim[2] - side.y
                , function(i)runif(n.rep, min=0, max=i) 
                )
  attach(data)
  Logi_A <- function(j.re){
    lapply(1:length(side.x), function(i.si){
      aaa<-  x >= xlo[j.re, i.si] & x < xlo[j.re, i.si]+side.x[i.si] & 
        y >= ylo[j.re, i.si] & y < ylo[j.re, i.si]+side.y[i.si]
                                           }) 
                           }
  logi.b <- lapply(1:n.rep, Logi_A)
  No_Spp <-function(j.logi)sapply(j.logi, 
                                  function(i)length(unique(sp[i]))
                                  )
  
  No_Sp2 <-function(j.logi)sapply(j.logi, 
                                  function(i)length(sp[i])
                                  )
  no.spp <- sapply(logi.b, No_Spp) 
  no.sp2 <- sapply(logi.b, No_Sp2)
  detach(data)   
  return(list(area=side.x^2/5*3/1e4, x=t(xlo), y=t(ylo), spp=no.spp, sp2=no.sp2) )
                                                           }

resu.all <- SampleRanSqu(data=data, side.x=seq(1, 500, by=2), n.rep=100)
str(resu.all)

area <- rep(resu.all$area, dim(resu.all$x)[2])
no.spp<- stack(as.data.frame(resu.all$spp))
ggxy <- as.data.frame(cbind(area, no.spp[1]))

length(ggxy$area)
x11(3.15, 3)
op0 <- par(mex=0.45,mar=c(5.1,5.0,1,1))
plot(ggxy$area
     , ggxy$values
     , pch='*'
     , cex=0.25
     , col=1#gray(1/8, alpha=0.6)
     , xlab=expression('面积 Area(' * hm^2 * ')')
     , ylab="物种数 Number of species"
     , cex.lab=0.7
     , cex.axis=0.7
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
op1 <- par( fig=c(.45 ,.95 ,.17 ,.67) , new=TRUE, mex=0.3, mar=c(7,6,1,1))

la0 <- stack(as.data.frame(resu.all[['spp']][(218/2):(238/2),]))
hist(la0[,1], breaks = 40, xlab = '',  # '物种数 Number of species', 
     ylab = '频数 Frequency' , main = '',
     cex.lab = 0.63,
     cex.axis = 0.6
    ) 

par(op1) 
#################################
# 218^2/5*3
# 238^2/5*3
# 2.69 3.34
# 260^2/5*3
# 300^2/5*3
# #################################
area <- rep(resu.all$area, dim(resu.all$x)[2])
no.spp <- stack(as.data.frame(resu.all$sp2))
ggxy <- as.data.frame(cbind(area, no.spp))
############
x11(3, 3)
op0 <- par(mex=0.45,mar=c(5.1,5.0,1.5,1))
plot(ggxy$area, ggxy$values
     , pch='*'
     , cex=0.28
     , xlab=expression('面积 Area(' * hm^2 * ')')
     , ylab="个体数 Number of individuals"
     , cex.lab=0.7
     , cex.axis=0.7
)
abline(lm(ggxy$values~ggxy$area), col=gray(5/8), lwd=1.7)
grid()
#
height <- 3800
radius <- 0.58 
x0 <- seq(-1,1,length=50)*radius  
y0 <- height/radius*sqrt(radius^2-x0^2)  
lines(3.55 + x0, 15800 + y0, col=1)
lines(3.55 + x0, 15800 - y0, col=1)
arrows( 3.65,21000, 4.6,35000,angle=30,code=2,length=0.10)
#
par(op0)
op1 <- par( fig=c(.15 , .57, .55 , .96) , new=TRUE, mex=0.3, mar=c(5,6,1,1))
la0 <- stack(as.data.frame(resu.all[['sp2']][(218/2):(238/2),]))
hist(la0[,1], breaks = 40, xlab = '',  # "个体数 Number of individuals", 
     ylab = '频数 Frequency' , main = '',
     cex.lab = 0.63,
     cex.axis = 0.6
) 
par(op1)  
###############################################################################
#  Distribution patterns ~~~~~~~~~~~~
#

setwd("F:/DataW/lg-data/composition")
data <- read.csv("lgdat2013.csv")
head(data)  
 da0 <- subset(data, sp!= '00枯立木'& is.na(bra))
 
da0$dbh[da0$dbh<1] <- 1
summary(da0$dbh)
head(da0)  
#
windowsFonts(Times = windowsFont("Times New Roman"))
tiff('MapDbhPlot10-20.tiff',
     family="Times",pointsize=8,
     width = 80, height = 59, units = "mm",
     res=600,compression = "lzw")
op0 <- par(mex=0.45,mar=c(3,3,0.5,0.5))

with(subset(da0,dbh>=10 & dbh<20), 
     plot(x, y, 
          ylim=c(0,300), xlim=c(0,500),
          ylab='',xlab='', 
          cex.axis=1,
          col=grey(3/20
                   , alpha=0.7
                   ),
          lwd=0.65,yaxt='n',
          pch=3, cex=0.65)
    )  
axis(2, cex.axis=1, at = seq(0, 300, length.out = 4)
     #, labels = seq(0, 300, by = 50)
     )
dev.off()
#
#
tiff('MapDbhPlot20-30-inf.tiff',
     family="Times",pointsize=8,
     width = 80, height = 59, units = "mm",
     res=600,compression = "lzw")
   op0 <- par(mex=0.45,mar=c(3,3,0.5,0.5))
with(subset(da0,dbh>=20 & dbh<30), 
     plot(x, y,
           ylim=c(0,300), xlim=c(0,500),
           ylab='',xlab='',
           cex.axis=1, yaxt='n',
           col=grey(0/10, alpha=0.6),
           lwd=0.9, pch=1, cex=0.7
          )
     ) 
axis(2, cex.axis=1, at = seq(0, 300, length.out = 4) )
with(subset(da0,dbh>=30), 
     points(x,y,ylim=c(0,300),xlim=c(0,500),pch=2,lwd=0.9,cex=1,col=grey(0/10, alpha=0.8))
     ) 
dev.off()
# 
########################## 
#####################################################################
#
windowsFonts(Times = windowsFont("Times New Roman"))
tiff('DbhClassAbun.tiff',
     family="Times",pointsize=8,
     width = 80, height = 70, units = "mm",
     res=600,compression = "lzw")
  par(mex=0.45,mar=c(5.1, 5.0, 2.3, 1))
  yy <- table(cut(da0$dbh,0:max(da0$dbh)+1, right=F))
  plot(yy
     , type='h'
     , lwd=0.7
     , cex.lab=1
     , axes=F
     , xlab='径级 DBH class (cm)', ylab='多度 Abundance')
  axis(1, seq(0,max(da0$dbh), by =round(max(da0$dbh)/10)), cex.axis=0.9)
  axis(2, seq(0,25000, by =2500), cex.axis=0.9)
  box()
  grid() 
  text(50,16000,labels = '所有个体 \n All individuals',cex=1)
dev.off()
###################################

tiff('CumulativeDbhClassAbun.tiff',
     family="Times",pointsize=8,
     width = 80, height = 70, units = "mm",
     res=600,compression = "lzw")
par(mex=0.45,mar=c(5.1, 7.5, 2.3, 1))
yy0 <- cumsum(yy)*100/sum(yy)
plot(yy0, type='n', axes=F,  
     , xlab='径级 DBH Class (cm)'
     , ylab="累计百分比多度 \n Cumulative percentage of abundance(%)"
)
grid()
axis(1, seq(0,max(da0$dbh), by =round(max(da0$dbh)/10)), cex.axis=0.9)
axis(2, seq(0,100, by =10), cex.axis=0.9)
box()
points(yy0
     , type='o'
     , cex=0.7
     , lty=3
     , lwd=0.7
     , cex.lab=1
  )
text(50,78,labels = '所有个体 \n All individuals',cex=1)
dev.off()
#
###########################################
#
yy0[74]-yy0[9]
cumsum(yy)[74]-cumsum(yy)[9]

sum(yy)-67538
#######################################################################
setwd("F:/DataW/lg-data/composition")
data <- read.csv("lgdat2013.csv")
head(data)  
da0 <- subset(data, sp!= '00枯立木'& is.na(bra))
#
da0$dbh[da0$dbh<1] <- 1
dir()
iv00 <- read.csv('hysIV2013.csv')
head(iv00)
nam00 <- iv00$Species
nam.1 <- c('黄梨木', '细叶谷木',
           '山榄叶柿', '蚬木',  '闭花木', '肥牛树', 
          '日本五月茶', '劲直刺桐', '对叶榕'
           )
xy00 <- data.frame(x=c(18, 42, 12, 
                       50, 30, 35,
                       24, 45,16),
                   y=c(134,360,290,
                       230,1550,285,
                       550,14,410) 
                  )
###
#
  for(i in 1:length(nam.1)){
    i=3
      da0=data[data$sp==nam.1[i], ]
      yy <- table(cut(da0$dbh,1:max(da0$dbh)+1))
tiff(paste('DbhSpeciesPlot',nam.1[i],'.tiff'),
     family="Times",pointsize=8,
     width = 56, height = 51, units = "mm",
     res=600,compression = "lzw"
     )
par(mex=0.3,mar=c(4, 4, 1, 1))
   plot(yy, lwd=0.7, axes=F, xlab='', ylab='', xlim=c(0,25))
      axis(1, seq(0,max(da0$dbh), by =round(max(da0$dbh)/10)), cex.axis=1)
      axis(2, seq(0,max(yy), by =round(max(yy)/10)), cex.axis=1)
      bb <- substitute(expression(X, italic(Y))
                 , list(X = paste(nam.1[i], '\n'), Y = as.character(iv00$sp.ld[i])) 
                 ) 
      mode(bb) <- 'expression'
      text(xy00[i,],labels = bb,cex=0.9)
 dev.off()
                         }

#######################################################################
  


##############################################################
setwd("F:/DataW/lg-data/composition")
data <- read.csv("lgdat2013.csv")
da0 <- subset(data, !is.na(bra))  # get brach data
da0$dbh[da0$dbh<1] <- 1
summary(da0$dbh); head(da0) 
#
windowsFonts(Times = windowsFont("Times New Roman"))
tiff('BrachMapPlot-0-5.tiff',family="Times",
     pointsize=8,
     width = 80, height = 59, units = "mm",
     res=600,compression = "lzw")
op0 <- par(mex=0.5,mar=c(3,3,1,1))
with(subset(da0,dbh>0 & dbh<5), 
     plot(x, y
          , ylim=c(0,300), xlim=c(0,500)
          ,ylab='',xlab=''
          , cex.axis=1
          ,col=grey(10/80
                      ,alpha=0.5
          )
          , pch='*', cex=0.65, yaxt='n'
     )
)
axis(side=2,at=seq(0,300,100), cex.axis=1)
dev.off()
#

#
tiff('BrachMapPlot-5-20-inf.tiff',family="Times",
     pointsize=8,
     width = 80, height = 59, units = "mm",
     res=600,compression = "lzw")
op0 <- par(mex=0.5,mar=c(3,3,1,1))
with(subset(da0,dbh>=5 & dbh<20), 
     plot(x, y
          , ylim=c(0,300), xlim=c(0,500)
          ,ylab='', xlab=''
          , cex.axis=1, yaxt='n',
          ,col=grey(2/10, alpha=0.5)
          ,lwd=0.7, pch=3, cex=0.6
     )
) 
axis(side=2,at=seq(0,300,100), cex.axis=1)
with(subset(da0,dbh>=20), 
     points(x,y,ylim=c(0,300), xlim=c(0,500),
            pch=1,lwd=1,cex=1,col=1)
) 
par(op0)
dev.off()
##################################################################
#####################################################################
#
yy <- table(cut(da0$dbh,0:max(da0$dbh)+1, right=F))
windowsFonts(Times = windowsFont("Times New Roman"))
tiff('BranchDbhClassAbun.tiff',
     family="Times",
     pointsize=8,
     width = 80, height = 70, units = "mm",
     res=600,compression = "lzw")
par(mex=0.45,mar=c(5.1,5.0,1,1))
plot(yy
     , type='h'
     , lwd=0.7
     , cex.lab=1
     , axes=F
     , xlab='径级 DBH class (cm)', ylab='多度 Abundance')
axis(1, seq(0,max(da0$dbh), by =round(max(da0$dbh)/10)), cex.axis=0.9)
axis(2, seq(0,25000, by =2500), cex.axis=0.9)
text(40.5,10000,labels = '所有分枝和萌枝 \n All ramifications and sprouts',cex=0.9)
box()
grid() 
dev.off()
###################################



