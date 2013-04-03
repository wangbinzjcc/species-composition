################################################################
# organize the original data of LG  wangbin  2013-2-1 22:22:20
################################################################

setwd("F:/lg-data/composition")
dir()
lgdat00 <- read.csv("DT.LGdata002.csv")
#
l.n00 <-as.character(lgdat00$no.)
#
gre00 <- grep('MC', l.n00)
n00 <-l.n00[gre00]
n01 <- gsub('MC','00',n00)
n02 <- gsub('NG','mc',n01)
sub00 <- substr(n02,10,11)
uni00 <- unique(sub00)
whi00 <- which(!is.na(match(sub00, uni00[-c(1:3,20:21,23)])))
n02[whi00] <- gsub('00','0',n02[whi00])
#
l.n00[gre00] <- n02
lgdat00$no. <- l.n00
# 
s00 <- substr(lgdat00$no.,11,12)
lg00$bra <- s00
###################################################################
#
HY <- read.csv('spe_cod_Hys-2012-10-26 110946.csv')
# 
DU <- unique(lgdat00$sp)
HU <- unique(HY$种名) 
setdiff(HU,DU)
setdiff(DU,HU)
############################################################ 
bh.u <- unique(substr(lgdat00$no.,3,6))

ex <- expand.grid(x=c('01','02','03','04','05','06','07','08','09',10:50), 
                  y=c('01','02','03','04','05','06','07','08','09',10:30))
ex.p <- paste(ex$x,ex$y, sep='')

setdiff(ex.p, bh.u)
setdiff(bh.u, ex.p)

bh.1 <- as.numeric(substr(bh.u,1,2))
bh.2 <- as.numeric(substr(bh.u,3,4))
plot(bh.1, bh.2)

#########################################
# add the missing quadrats data  

nu.00 <- c("0917", "0818", "0918", "0819", "0919", "0820", "0920", "0821")

nu.01 <- c('0916', '0817', '1018', '0719', '1019', '0720', '0921', '0822')
#

lgi.00 <- match(substr(lgdat00$no., 3, 6), nu.01)
lgdat01 <- lgdat00[!is.na(lgi.00), ]

lg01.new <- lgdat01$no.
for(i in 1:8){
lg01.new <- gsub(pattern = paste('NG', nu.01[i],sep=''), 
            replacement = paste('na',nu.00[i],sep=''),
            x=lg01.new)
lg01.new <- gsub(pattern = paste('mc', nu.01[i],sep=''), 
                 replacement = paste('na',nu.00[i],sep=''),
                 x=lg01.new)
             }
lgdat01$no. <- lg01.new 
#
lgdat02 <- rbind(lgdat00,lgdat01)
##############################################################
###  add Coordinates of the Branchs~~~
for(i in 1:30){
  xn <- which(is.na(lgdat02$x)) 
  lgdat02$x[xn] <- lgdat02$x[xn-1]
  yn <- which(is.na(lgdat02$y)) 
  lgdat02$y[yn] <- lgdat02$y[yn-1]
              }

###  Transform coordinates from small scale to the large one ~~~

lgdat02$x <- with(lgdat02, as.numeric(substr(no.,3,4))*10 - 10 + as.numeric(x) + as.numeric(quadrats==2 | quadrats==3)*5  )
lgdat02$y <- with(lgdat02, as.numeric(substr(no.,5,6))*10 - 10 + as.numeric(y) + as.numeric(quadrats==3 | quadrats==4)*5  )
lgdat02$bra <- substr(lgdat02$no.,11,12)

write.csv(lgdat02, 'LGdataWB201322144445.csv')


###################################################################
###################################################################












