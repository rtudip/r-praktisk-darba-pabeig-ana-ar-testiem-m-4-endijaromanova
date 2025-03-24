kordat <- read.table(file = "variants1.txt", row.names = 1, header = TRUE, sep = "\t", dec = ",", strip.white = TRUE)
kordat[, 9:ncol(kordat)] <- lapply(kordat[, 9:ncol(kordat)], as.factor)
sink("results.txt")
summary(kordat[, 9:ncol(kordat)])
sl.by.b <- split(kordat$Slope, kordat$b)
print(sl.by.b)
kordat$Average <- rowMeans(kordat[, c("Slope", "Intercept", "adj.r.squared")], na.rm = TRUE)
sd <- tapply(kordat$Slope, kordat$f, sd, na.rm = TRUE)
print(sd)
if (any(kordat$adj.r.squared > 0)){
  prockordat <- kordat[kordat$adj.r.squared > 0.7, ]
} else {
  prockordat <- kordat[kordat$adj.r.squared > -0.3, ]}
prockordat$Slope <- 1 - (1 / prockordat$Slope)
print(prockordat)
sink()

svg("scatter.svg")
plot(kordat$MAD, kordat$Average)
dev.off()
svg("boxplot.svg")
boxplot(kordat$Intercept ~ kordat$f)
dev.off()