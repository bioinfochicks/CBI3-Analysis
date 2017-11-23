#calculate averages, medians, rolling mean, rolling median

newd$PhaseMean <- rowMeans(select(d,starts_with("Phase.Sweep")), na.rm = TRUE)
newd$AntiphaseMean <- rowMeans(select(d,starts_with("Antiphase.Sweep")), na.rm = TRUE)
newd$GrandMean <- rowMeans(select(d,contains("Grand")), na.rm = TRUE)

newd$PhaseMedian <- rowMedians(as.matrix(select(d,starts_with("Phase.Sweep"))), na.rm = TRUE)
newd$AntiphaseMedian <- rowMedians(as.matrix(select(d,starts_with("Antiphase.Sweep"))), na.rm = TRUE)
newd$GrandMedian <- rowMedians(as.matrix(select(d,contains("Grand"))), na.rm = TRUE)

newd$PhaseRollingMean10 <- rollapply(select(d,starts_with("Phase.Sweep")),10, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean10 <- rollapply(select(d,starts_with("Antiphase.Sweep")),10, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean10 <- rollapply(select(d,starts_with("Grand")),10, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian10 <- rollapply(select(d,starts_with("Phase.Sweep")),11, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian10 <- rollapply(select(d,starts_with("Antiphase.Sweep")),11, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian10 <- rollapply(select(d,starts_with("Grand")),11, median, by.column=FALSE, fill=NA)

newd$PhaseRollingMean20 <- rollapply(select(d,starts_with("Phase.Sweep")),20, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean20 <- rollapply(select(d,starts_with("Antiphase.Sweep")),20, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean20 <- rollapply(select(d,starts_with("Grand")),20, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian20 <- rollapply(select(d,starts_with("Phase.Sweep")),21, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian20 <- rollapply(select(d,starts_with("Antiphase.Sweep")),21, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian20 <- rollapply(select(d,starts_with("Grand")),21, median, by.column=FALSE, fill=NA)

newd$PhaseRollingMean50 <- rollapply(select(d,starts_with("Phase.Sweep")),50, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean50 <- rollapply(select(d,starts_with("Antiphase.Sweep")),50, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean50 <- rollapply(select(d,starts_with("Grand")),50, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian50 <- rollapply(select(d,starts_with("Phase.Sweep")),51, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian50 <- rollapply(select(d,starts_with("Antiphase.Sweep")),51, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian50 <- rollapply(select(d,starts_with("Grand")),51, median, by.column=FALSE, fill=NA)


#Basic plot of raw data, smoothed raw data mean, medians (normal and rolling)

pdf("PERG_raw.pdf", 7,5)
ggplot(mdata, aes(mdata$Time, mdata$value, group=mdata$variable)) +
  geom_line(col="cornflowerblue") +
  xlab("Time (mS)") + 
  ylab("nV")
dev.off()

pdf("PERG_means.pdf", 7,5)
plotPERG(newd, newd$Time, newd$PhaseMean, newd$AntiphaseMean, newd$GrandMean)
dev.off()

pdf("PERG_medians.pdf", 7,5)
plotPERG(newd, newd$Time, newd$PhaseMedian, newd$AntiphaseMedian, newd$GrandMedian)
dev.off()

pdf("PERG_smooth_gam.pdf", 7,5)

ggplot(mdata, aes(mdata$Time, mdata$value)) +
  geom_smooth() +
  geom_vline(xintercept = 50, col = "grey44", linetype="dashed") +
  geom_text(aes(x=60,y=4500, label = "P50")) +
  geom_vline(xintercept = 35, col = "grey44", linetype="dashed") +
  geom_text(aes(x=27,y=4500, label = "N35")) +
  geom_vline(xintercept = 95, col = "grey44", linetype="dashed") +
  geom_text(aes(x=105,y=4500, label = "N95")) +
  xlab("Time (mS)") + 
  ylab("nV")
dev.off()

#### Rolling means/medians - do this in a for loop with different intervals 10, 20, 50


pdf("PERG_rolling_mean10.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMean10,
         newd$AntiphaseRollingMean10,
         newd$GrandRollingMean10)
dev.off()

pdf("PERG_rolling_median10.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMedian10,
         newd$AntiphaseRollingMedian10,
         newd$GrandRollingMedian10)
dev.off()

pdf("PERG_rolling_mean20.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMean20,
         newd$AntiphaseRollingMean20,
         newd$GrandRollingMean20)
dev.off()

pdf("PERG_rolling_median20.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMedian20,
         newd$AntiphaseRollingMedian20,
         newd$GrandRollingMedian20)
dev.off()

pdf("PERG_rolling_mean50.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMean50,
         newd$AntiphaseRollingMean50,
         newd$GrandRollingMean50)
dev.off()

pdf("PERG_rolling_median50.pdf", 7,5)
plotPERG(newd, newd$Time, 
         newd$PhaseRollingMedian50,
         newd$AntiphaseRollingMedian50,
         newd$GrandRollingMedian50)
dev.off()

mnewd <- melt(newd, id= newd$Time)

##########Final plot of grand averages for all - need to make long
ggplot(newd, aes(x=newd$Time, y = newd$GrandMean)) +
  geom_line(aes()) +
  geom_line(aes(x=newd$Time, y = newd$GrandMedian)) +
  geom_line(aes(x=newd$Time, y = newd$GrandRollingMean20)) +
  geom_line(aes(x=newd$Time, y = newd$GrandRollingMedian20 )) +
  geom_vline(xintercept = 50, col = "grey44", linetype="dashed") +
  geom_text(aes(x=60,y=4500, label = "P50")) +
  geom_vline(xintercept = 35, col = "grey44", linetype="dashed") +
  geom_text(aes(x=27,y=4500, label = "N35")) +
  geom_vline(xintercept = 95, col = "grey44", linetype="dashed") +
  geom_text(aes(x=105,y=4500, label = "N95")) +
  xlab("Time (mS)") + 
  ylab("nV") +
  scale_colour_discrete(labels = c("Grand Average", "Grand Median", 
                                   "Rolling Mean", "Rolling Median"),
                        name = "Method")

#calculate averages, medians, rolling mean, rolling median

newd$PhaseMean <- rowMeans(select(d,starts_with("Phase.Sweep")), na.rm = TRUE)
newd$AntiphaseMean <- rowMeans(select(d,starts_with("Antiphase.Sweep")), na.rm = TRUE)
newd$GrandMean <- rowMeans(select(d,contains("Grand")), na.rm = TRUE)

newd$PhaseMedian <- rowMedians(as.matrix(select(d,starts_with("Phase.Sweep"))), na.rm = TRUE)
newd$AntiphaseMedian <- rowMedians(as.matrix(select(d,starts_with("Antiphase.Sweep"))), na.rm = TRUE)
newd$GrandMedian <- rowMedians(as.matrix(select(d,contains("Grand"))), na.rm = TRUE)

newd$PhaseRollingMean10 <- rollapply(select(d,starts_with("Phase.Sweep")),10, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean10 <- rollapply(select(d,starts_with("Antiphase.Sweep")),10, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean10 <- rollapply(select(d,starts_with("Grand")),10, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian10 <- rollapply(select(d,starts_with("Phase.Sweep")),11, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian10 <- rollapply(select(d,starts_with("Antiphase.Sweep")),11, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian10 <- rollapply(select(d,starts_with("Grand")),11, median, by.column=FALSE, fill=NA)

newd$PhaseRollingMean20 <- rollapply(select(d,starts_with("Phase.Sweep")),20, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean20 <- rollapply(select(d,starts_with("Antiphase.Sweep")),20, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean20 <- rollapply(select(d,starts_with("Grand")),20, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian20 <- rollapply(select(d,starts_with("Phase.Sweep")),21, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian20 <- rollapply(select(d,starts_with("Antiphase.Sweep")),21, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian20 <- rollapply(select(d,starts_with("Grand")),21, median, by.column=FALSE, fill=NA)

newd$PhaseRollingMean50 <- rollapply(select(d,starts_with("Phase.Sweep")),50, mean, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMean50 <- rollapply(select(d,starts_with("Antiphase.Sweep")),50, mean, by.column=FALSE, fill=NA)
newd$GrandRollingMean50 <- rollapply(select(d,starts_with("Grand")),50, mean, by.column=FALSE, fill=NA)

newd$PhaseRollingMedian50 <- rollapply(select(d,starts_with("Phase.Sweep")),51, median, by.column=FALSE, fill=NA)
newd$AntiphaseRollingMedian50 <- rollapply(select(d,starts_with("Antiphase.Sweep")),51, median, by.column=FALSE, fill=NA)
newd$GrandRollingMedian50 <- rollapply(select(d,starts_with("Grand")),51, median, by.column=FALSE, fill=NA)

