### Path to the Git repository
setwd("~/Documents/DATA/Codes/seaflow-sfl")

inst <- "751" # or "751"

fr <- read.csv(paste0("flow_rate_calibration/",inst,"-streampressure.csv"))
fr$fr <- 60*(fr$weight - fr$tare)/fr$time #calculate flow rate (ml/min)


### linear regression
reg <- lm(fr ~ poly(measured.pressure, 1, raw=T), data=log(fr[,c("fr", "measured.pressure")],10))
summary(reg)

save(reg, file=paste0("flow_rate_calibration/lm_",inst))

png(paste0("flow_rate_calibration/",inst,"-flowrate.png"),width=12, height=12, unit='in', res=100)

par(mfrow=c(1,1), cex=1.4)
  plot(fr$measured.pressure , fr$fr, bg=adjustcolor('red3',0.4), pch=21, cex=2, xlab='pressure (psi)', ylab="Flow Rate (ml/min)", log='xy', main=paste("#",inst))
  par(new=T)
  plot(log10(fr$measured.pressure), log10(fr$fr), yaxt='n',xaxt='n',xlab=NA, ylab=NA,pch=NA, bty='n')
  lines(x=log10(fr$measured.pressure),predict(reg, newdata=data.frame(measured.pressure=log10(fr$measured.pressure)),interval='predict')[,"fit"], col='red3',lwd=2 )
  lines(x=log10(fr$measured.pressure),predict(reg, newdata=data.frame(measured.pressure=log10(fr$measured.pressure)),interval='predict')[,"lwr"], col='grey',lwd=2 )
  lines(x=log10(fr$measured.pressure),predict(reg, newdata=data.frame(measured.pressure=log10(fr$measured.pressure)),interval='predict')[,"upr"], col='grey',lwd=2 )
  legend("bottomright", legend=bquote(paste("FR=",.(round(10^reg$coefficients[1],3)),"(psi"^{.(round(reg$coefficients[2],3))},")")), bty='n',cex=2)
  abline(v=log10(12),col='green')
  abline(h=predict(reg, newdata=data.frame(measured.pressure=log10(12)),interval='predict')[,"fit"], col='green')
  legend('top',paste(round(10^predict(reg, newdata=data.frame(measured.pressure=log10(12)),interval='predict'),1)), bty='n', text.font=c(1,2,1))

dev.off()
