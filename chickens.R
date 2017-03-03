source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
setwd("C:/Users/Julian Bautista/Documents/Portfolio/Chickens")

#setting up the data
chix <- read.table("https://raw.githubusercontent.com/julbautista/Chickens/master/chicken_data.txt", header = FALSE, skip = 1)
colnames(chix) <- c("freq", "N1", "Mean_SvC", "SE_SvC", "N2", "Mean_TvC", "SE_TvC")

#plotting sham treatment vs frequency
ggplot(chix, aes(freq, Mean_SvC)) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(0.8,1.2) + 
  labs(x = "Frequency", y = "Sham Treatment",
       title = "Sham Treatment:Control Ratio Plotted across Frequencies") +
  geom_errorbar(ymin = chix$Mean_SvC - chix$SE_SvC,
                ymax = chix$Mean_SvC + chix$SE_SvC, width = 8,
                colour = jbpal$blue) + 
  geom_point(colour = jbpal$blue) +
  geom_hline(yintercept = max(chix$Mean_SvC + chix$SE_SvC) + 0.003, linetype = "dotted") +
  geom_hline(yintercept = min(chix$Mean_SvC - chix$SE_SvC) - 0.003, linetype = "dotted") +
  geom_hline(yintercept = 1, linetype = "dashed", colour = jbpal$blue) +
  geom_text(aes(y = max(chix$Mean_SvC + chix$SE_SvC) + 0.02, x = median(chix$freq)), label = "Maximum Sham Effect", colour = jbpal$brown, size = 2.5, alpha = 0.15) +
  geom_text(aes(y = min(chix$Mean_SvC - chix$SE_SvC) - 0.02, x = median(chix$freq)), label = "Minimum Sham Effect", colour = jbpal$brown, size = 2.5, alpha = 0.15)

#regress treatment vs frequency
summary(lm(chix$Mean_TvC~chix$freq))

#plot treatment vs frequency
ggplot(chix, aes(freq, Mean_TvC)) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(0.8,1.5) + 
  labs(x = "Frequency", y = "Sham Treatment",
       title = "Treatment:Control Ratio Plotted across Frequencies") +
  geom_point() + geom_smooth(method = "lm") +
  geom_text(aes(100,1.4), label = "Slope = -0.00012 \n Adjusted R-Squared = 0.015", colour = jbpal$brown, alpha = 0.15)


#fitting a multilevel model estimating treatment effect
y   <- chix$Mean_TvC
se  <- chix$SE_TvC
N   <- length(y)

fit <- stan(file = 'chickens.stan', 
            data = c("y", "se", "N"), 
            iter = 1000, chains = 4)

#plotting the multilevel model
ext <- extract(fit)
est <- colMeans(ext$theta)

ggplot(chix, aes(freq, est)) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(0.7,1.5) + 
  labs(x = "Frequency", y = "Sham Treatment",
       title = "Multilevel Estimate of Treatment:Control Ratio") +
  geom_errorbar(ymin = est - chix$SE_SvC,
                ymax = est + chix$SE_SvC, width = 8, colour = jbpal$blue) +
  geom_smooth(method = "lm", fill = NA, alpha = 0.1, size = 0.5, colour = jbpal$green) +
  geom_point(colour = jbpal$blue) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3)
  

#plotting simple treatment model
ggplot(chix, aes(freq, Mean_TvC)) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(0.7,1.5) + 
  labs(x = "Frequency", y = "Treatment",
       title = "Treatment:Control Ratio") +
  geom_errorbar(ymin = chix$Mean_TvC - chix$SE_SvC,
                ymax = chix$Mean_TvC + chix$SE_SvC, width = 8, colour = jbpal$blue) +
  geom_smooth(method = "lm", fill = NA, alpha = 0.1, size = 0.5, colour = jbpal$green) +
  geom_point(colour = jbpal$blue) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3)


#plotting the two estimate types next to one another.
ggplot(chix, aes(chix$freq, (chix$Mean_TvC - (chix$Mean_TvC) / mean(chix$Mean_SvC)))) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(-0.5,0.5) + 
  labs(x = "Frequency", y = "Treatment",
       title = "Difference between Two Methods") + 
  geom_hline(yintercept = 0, colour = jbpal$green) +
  geom_point(colour = jbpal$blue, shape = 19)


#plotting mean treatment effect
ggplot(chix, aes(chix$freq, chix$Mean_TvC)) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(-0.5,1.5) + 
  labs(x = "Frequency", y = "Treatment",
       title = "Treatment Mean with Error Bars") +
  geom_errorbar(ymin = chix$Mean_TvC - chix$SE_TvC,
                ymax = chix$Mean_TvC + chix$SE_TvC, width = 8, colour = jbpal$blue) +
  geom_point(colour = jbpal$blue) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3)

#plotting ratio of means
ggplot(chix, aes(chix$freq, (chix$Mean_TvC) / mean(chix$Mean_SvC))) + jbplot +
  theme(panel.grid.major = element_blank(), axis.ticks = element_line(colour = "#BCBCBC")) +
  ylim(-0.5,1.5) + 
  labs(x = "Frequency", y = "Treatment",
       title = "Treatment:Sham Ratio with Error Bars") +
  geom_errorbar(ymin = (chix$Mean_TvC) / mean(chix$Mean_SvC) - ((chix$SE_TvC) / mean(chix$SE_SvC)),
                ymax = (chix$Mean_TvC) / mean(chix$Mean_SvC) + ((chix$SE_TvC) / mean(chix$SE_SvC)), 
                width = 8, colour = jbpal$blue) +
  geom_point(colour = jbpal$blue) +
  geom_hline(yintercept = 1, linetype = "dashed", alpha = 0.3)

#plot of options i and ii means
plot(x = NULL, y = NULL,
     main = "Treatment Mean with Treatment:Sham Ratio",
     xlab = "Frequency",
     ylab = "Treatment Effect",
     ylim = range(c(-0.5, 1.5)),
     xlim = range(chix$freq)
)
abline(h = 1, lty = 2)
points(chix$freq, chix$Mean_TvC, pch = 16)
points(chix$freq, (chix$Mean_TvC) / mean(chix$Mean_SvC), pch = 18, col = "orange")

#plot of option i with errors
plot(x = NULL, y = NULL,
     main = "Treatment Mean with Error Bars",
     xlab = "Frequency",
     ylab = "Treatment Effect",
     ylim = range(c(-0.5, 1.5)),
     xlim = range(chix$freq)
)
abline(h = 1, lty = 2)
arrows(chix$freq, 
       chix$Mean_TvC - chix$SE_TvC, chix$freq,
       chix$Mean_TvC + chix$SE_TvC,
       length = 0,
       angle = 90,
       col = "blue"
)
points(chix$freq, chix$Mean_TvC, pch = 16)

#plot of option ii with errors
plot(x = NULL, y = NULL,
     main = "Treatment:Sham Ratio with Error Bars",
     xlab = "Frequency",
     ylab = "Treatment Effect",
     ylim = range(c(-0.5, 1.5)),
     xlim = range(chix$freq)
)
abline(h = 1, lty = 2)
arrows(chix$freq, 
       ((chix$Mean_TvC) / mean(chix$Mean_SvC)) - ((chix$SE_TvC) / mean(chix$SE_SvC)),
       chix$freq,
       ((chix$Mean_TvC) / mean(chix$Mean_SvC)) + ((chix$SE_TvC) / mean(chix$SE_SvC)),
       length = 0,
       angle = 90,
       col = "blue"
)
points(chix$freq, (chix$Mean_TvC) / mean(chix$Mean_SvC), pch = 18, col = "orange")

