



 
curve(5 * log(x + 1) / log(101), from=0, to=100, xlab="Espezifizitatea", ylab="Sentikortasuna", 
      main="ROC kurba", col="blue", lwd=3, axes=F, xlim=c(0, 100), ylim=c(0, 5))


Axis(side=1, at=c(0, 20, 40, 60, 80, 100), labels=c("0", "0.2", "0.4", "0.6", "0.8", "1"), cex.axis=0.8)
Axis(side=2, at=0:5, labels=c("0", "0.2", "0.4", "0.6", "0.8", "1"))
segments(0, 0, 100, 5, lwd=3)
segments(0, 0, 0, 5, lwd=3, col="grey")
segments(0, 5, 100, 5, lwd=3, col="grey")
text(20, 4, col="grey", labels=" perfektua")
text(40, 3, col="blue", labels=" ona")
text(70, 2, col="black", labels="txarra")

