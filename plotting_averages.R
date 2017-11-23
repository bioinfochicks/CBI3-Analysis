library(dplyr)
library(ggplot2)
library(matrixStats)
library(zoo)
library(signal)
library(rmarkdown)

#d <- read.csv("Hicks_PERG - WorkingCopy.csv", header=T)

#mdata <- melt(d, id="Time")

######### functions

plotPERG <- function(dataset, time, sub1, sub2, grand) { 
  list(
    p <- ggplot(data=dataset , aes(x=time, y = sub1)) +
    geom_line(col="grey") +
    geom_line(data=dataset, aes(x=time, y=sub2), col="grey") +
    geom_line(data=dataset, aes(x=time, y=grand), col="red") +
    geom_vline(xintercept = 50, col = "grey44", linetype="dashed") +
    geom_text(aes(x=60,y=4500, label = "P50")) +
    geom_vline(xintercept = 35, col = "grey44", linetype="dashed") +
    geom_text(aes(x=27,y=4500, label = "N35")) +
    geom_vline(xintercept = 95, col = "grey44", linetype="dashed") +
    geom_text(aes(x=105,y=4500, label = "N95")) +
    xlab("Time (mS)") + 
    ylab("nV")
  )
  return(p)
}

######### Processing stages
#plot of raw data
#
#the signal-to-noise ratio (SNR) is improved by using a Butterworth filter (bandpass) (2-65Hz). Use the signal package

#The average repetitive form of the signal is represented by reorganising the signal (which is 
#column of numbers) into an array so that each 251ms response has a separate column. Put into correct format

#Following this, the row vector of each PERG element was computed which will represent the 
#average of the array, giving you the ‘clean’ PERG recovered signal with a SNR improvement of Square Root of N (100). Calculate average

# these artefacts were uncovered by calculating the variance of each row 
#(in a clean PERG signal, we would expect this to be normally distributed) displayed in a histogram. Calc variance of each row, to remove unusually large variances
#Where an artefact is present in a row, the variance will be unusually large. Using the standard deviation of the 
#signal, the rows containing artefacts were removed and the ‘clean’ PERG signal was recomputed.
# Remove high variance using standard deviation, then repeat the average

## bandpass filter (using signal)

## remove passes with large variance (may have been done manually) to clean the signal

############################################################
#Basic plot of two subaverage and grand, as output by program
ggplot(d, aes(Time, Antiphase.Subaverage)) +
  geom_line() +
  geom_line(aes(Time, Phase.Subaverage)) +
  geom_line(aes(Time, GrandAverage), col="red")+
  geom_vline(50)
  xlab("Time (mS)") + 
  ylab("nV")
  

#head(d[,2:590]) To get individual columns
newd <- data.frame(d$Time) 
colnames(newd) <- "Time"

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

## designing the bandpass filter range is 2-65Hz. Unsure how to specify the filter
low_filt <- butter(3,1/500, "pass")
high_filt <- butter(3,1/500, "pass")

## applying the bandpass filter - signal package - does this need to be in ' 
butter(3,1/500, 65/500, 'bandpass')
filtered_d <- filtfilt(low_filt, high_filt, data)

## calculating the variance and removing passes which exceed the threshold