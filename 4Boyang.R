x = read.csv("GOOGLEANALYTICS.csv")



plot(x$REV, x$DURATION)

reg = lm(x$DURATION~x$REV)
abline(reg)

reg




z = subset(x, x$REV > 0 & x$ACQ == "direct")
plot(z$REV, z$DURATION)



reg2 = lm(z$DURATION~z$REV)

abline(reg2)

reg2

help(lm)

