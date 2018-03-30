library(lmodel2)


#Path to the Git repository
setwd("~/Documents/DATA/Codes/seaflow-virtualcore")

fr <- read.csv("flow_rate_calibration/FLOW_RATEcalibration.csv")
fr$fr <- 60*(fr$weight - fr$tare)/fr$time #calculate flow rate (ml/min)


# linear regression type II
reg <- lmodel2(fr ~ measured.pressure, data=log10(fr[,c("fr", "measured.pressure")]))
df <- data.frame(expo=reg$regression.results$Slope[1],expo_97.5=reg$confidence.intervals[1,5],expo_2.5=reg$confidence.intervals[1,4],coeff=10^reg$regression.results$Intercept[1], coeff_97.5=10^reg$confidence.intervals[1,3],coeff_2.5=10^reg$confidence.intervals[1,2])


png("flow_rate_calibration/FlowRate-calibration.png",width=12, height=12, unit='in', res=100)

par(mfrow=c(1,1), cex=1.4)
  plot(fr$measured.pressure , fr$fr, bg=adjustcolor('red3',0.4), pch=21, cex=2, xlab='pressure (psi)', ylab="Flow Rate (ml/min)", log='xy')
  par(new=T)
  plot(log10(fr$measured.pressure), log10(fr$fr), yaxt='n',xaxt='n',xlab=NA, ylab=NA,pch=NA, bty='n')
  abline(b=reg$regression.results$Slope[1], a=reg$regression.results$Intercept[1], col=2,lwd=2) # 50%
  abline(b=reg$confidence.intervals[1,4], a=reg$confidence.intervals[1,2], col='grey',lwd=2) # 2.5%
  abline(b=reg$confidence.intervals[1,5], a=reg$confidence.intervals[1,3], col='grey',lwd=2) # 97.5%
  legend("topleft", legend=bquote(paste("FR=",.(round(10^reg$regression.results$Intercept[1],3)),"(psi"^{.(round(reg$regression.results$Slope[1],3))},")")), bty='n',cex=2)
  legend("bottomright", legend=bquote(paste("12 psi =",.(round(10^reg$regression.results$Intercept[1]*12^reg$regression.results$Slope[1],1)),
                                                  "(",.(round(10^reg$confidence.intervals[1,2]*12^reg$confidence.intervals[1,4],1)),
                                                  "-",.(round(10^reg$confidence.intervals[1,3]*12^reg$confidence.intervals[1,5],1)),")")), bty='n',cex=2)
dev.off()
