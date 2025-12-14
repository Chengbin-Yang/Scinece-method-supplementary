#### Approximate Minimum Detectable Effect Size (assuming a known single shift in a single time series model, based on CDHP 2015)

setwd("F:\\Desktop\\科研项目\\1.负责科研项目\\Climate Policy DID Science-厦大_北师大\\Science")

T <- 22 #time series length
Tsi <- matrix(NA, ncol=1, nrow=T)
for (i in 1:T){
  Tsi[i] <- i*(T-i)/T
}

c01 <- abs(qnorm(0.01/2))
c001 <- abs(qnorm(0.001/2))

lam <- seq(0, 5, 0.5)
sigma <- 0.04 #approx s.e. of regression

p01 <- (c01/sqrt(Tsi))*sigma
p001 <- (c001/sqrt(Tsi))*sigma

ticks <- c(seq(0.01, 0.15, 0.01))
ticksx <- seq(0, 22, 1)

pdf("approx_power_v1.pdf", width=10, height=9)

plot(p01, col="#e41a1c", lwd=2,type="l", ylim=c(0, 0.16), yaxt='n', xaxt="n", cex.axis=0.8, xlab="Length of Break (years, with a maximum sample of T=22)", ylab="Minimum Effect Size (dlog Emission for se=0.04)", main="Approximate Minimum Detectable Effect Size for a 'Known' Break")
lines(p001, col="#377eb8", lwd=2)

abline(h=ticks, col="gray85", lwd=0.4)

lines(p01, col="#e41a1c", lwd=5)
lines(p001, col="#377eb8", lwd=5)

abline(h=0, lty=1, col="gray55")
legend("bottomleft", 
       legend = c("p=0.01", "p=0.001"), 
       col = c("#e41a1c", "#377eb8"),
       lty = c(1, 1),
       lwd = c(2,2),  
       bty = "n", 
       pt.cex = 2, 
       cex = 1.2, 
       text.col = "black", 
       horiz = F , 
       inset = c(0.01, 0.03))

axis(2, labels=ticks, at=ticks, cex.axis=0.8)
axis(1, labels=ticksx, at=ticksx, cex.axis=0.8)

dev.off()






