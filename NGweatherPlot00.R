
setwd('F:\\DataW\\lg-data')
dir()
weather00 <- read.csv("弄岗气象1971-2000.csv")
head(weather00)
summary(weather00)
#
x11()

op0 <- par(mar=c(5,11,1,5), mex=0.45)
per00 <- weather00$平均降水量 
plot(per00,type='h', lwd=8,Jxaxt="n",
     tck=0.02, xlab='月份 Month', ylab='降雨量 Percipitation')
axis(1, at=1:12, labels=1:12, tck=0.02)
par(op0)

op1 <- par(new=T, mex=0.45, mar=c(5,11,1,5))
win00 <- weather00$平均风速.m.s
plot(win00, type='o',pch=24, bg=grey(7/8),  cex=1.5,
     axes=F,xlab='',ylab='')
axis(4,tck=0.02)
mtext('风速 Wind speed', side=4, line=2.8)
legend(9.9, 1.43,
       c('降雨量\nPercipitation   \n', '温度\nTemperaturel   \n', '风速\nWind speed   \n'),
       pch = c( 15, 21, 24), ncol=1, pt.cex =c(1.5, 1.5, 1.3),cex=0.8)
par(op1)

op2 <- par(new=T, mex=0.45, mar=c(5,11,1,5))
tem00 <- weather00$平均温度 
plot(tem00,type='o', pch=21, bg=grey(7/8), cex=1.6,
     xlab='',bty='c',axes=F, tck=0.02, ylab=' ')
op3 <- par(new=T, mex=0.45, mar=c(5,5,1,5))
plot(tem00, type='n', xlab='', ylab='温度 Temperature', axes=F)
axis(2, tck=0.02)
par(op2)
par(op3)

 ###################



  