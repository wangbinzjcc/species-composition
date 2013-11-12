
setwd('F:\\DataW\\lg-data')
dir()
weather00 <- read.csv("弄岗气象1971-2000.csv")
head(weather00)
summary(weather00)
#
# x11(5,3.6)
windowsFonts(Times = windowsFont("Times New Roman"))
 #
tiff('WeatherNongGang00-wb.tiff', family="Times",
     pointsize=8,
     width = 80, height = 60, units = "mm",
     res=600,compression = "lzw")

op00 <- par(mar=c(6,12,0.5,5.5), mex=0.36)
per00 <- weather00$平均降水量 
summary(per00)
plot(per00,type='n', axes=F, xlab='月份 Month', ylab='')

legend(1, 238,
       c(' 降雨量\nPercipitation   \n', ' 温 度\nTemperaturel   \n', ' 风 速\nWind speed   \n'),
       bty='n', pch = c(15, 21, 24), col=1, pt.bg=c(1, grey(7/8), grey(7/8)),
       ncol=3, pt.cex =c(1.3, 1.3, 1.1), cex=0.8)
box()
axis(1, at=1:12, labels=1:12, tck=0.02, mgp=c(0,0,0), cex.axis=0.9)
par(op00)

op2 <- par(new=T, mex=0.36, mar=c(6,12,5,5.5))
per00 <- weather00$平均降水量 
summary(per00)
plot(per00, type='h', lwd=6, xaxt="n", bty='u',
     xlab='', ylab='', bty='c', axes=F, tck=0.02,)
par(op2)

#
op0 <- par(new=T, mex=0.36, mar=c(6,12,10,5.5))
tem00 <- weather00$平均温度 
summary(tem00)
plot(tem00,type='o', pch=21, bg=grey(7/8), cex=1.3,axes=F, 
     xlab='', ylab='',
     tck=0.02, cex.lab=1)
axis(2,tck=0.02, cex.axis=0.9, mgp=c(0,0.1,0))
mtext(expression(温度*' '*Temperature*' ('^o*'C)'), side=2, line=2.3, cex.lab=1)
par(op0)

op1 <- par(new=T, mex=0.36, mar=c(6,12,10,5.5))
win00 <- weather00$平均风速.m.s
plot(win00, type='o',pch=24, bg=grey(7/8),  cex=1.1, 
     axes=F,xlab='',ylab='')
axis(4,tck=0.03, cex.axis=0.9, mgp=c(0, -0.1, 0))
mtext('风速 Wind speed (m/s)', side=4, line=2.6, cex.lab=1)
par(op1)

op3 <- par(new=T, mex=0.36, mar=c(5,5.6,1,5.5))
plot(per00 , type='n', xlab='', ylab='降雨量 Percipitation (mm)', axes=F, cex.lab=1)
axis(2,at=seq(20,200, by=30), labels=seq(20,200, by=30), tck=0.02
     , mgp=c(0,0,0.3 ), cex.axis=0.9)
par(op3)
#
dev.off()
 ###################

 






 