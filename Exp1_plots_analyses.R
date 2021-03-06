# Haptic perception of gyroscopic forces

library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(cowplot)
library(lme4)

setwd("/yourfilepath/")

df1 <- readRDS("weight.rds")

# the weight.rda dataframe (tibble) has 6 variables in "long" format
# all IVs are within subjects
# -----IVs-----
# Participant: participant number
# Arc:         wielding mode (Full = forehand 90 degrees, Half = forehand-backhand 45 degrees)
# Mass:        how much weight was added? (0 g, 50 g, or 100 g)
# Spin:        direction/rate of spin (-1 = 7000 rpm CCW, 0 = no spin, 1 = 7000 rpm
# Abspin:      Absolute value of the spin variable
# -----DV------
# Judgment:    Participants rating of objects weight, in comparison to a standed object. Arbitrary Units.

#Get means of each group separated by arc
means <- df1 %>%
  group_by(Mass, Spin, Arc) %>%
  dplyr::summarise(Judgment = mean(Judgment))
sds   <- df1 %>%
  group_by(Mass, Spin, Arc) %>%
  dplyr::summarise(SD = sd(Judgment))
df2 <- bind_cols(means,sds[4])
within_error <- summarySEwithin(x, measurevar="Judgment", withinvars=c("Spin","Mass","Arc"),
                                idvar="Participant")
SEs <- as.tibble(within_error)
df3 <- inner_join(SEs[,c(1:4,6:8)],df2, by = c("Mass", "Spin", "Arc"))

# get means of each group, not separated by arc
means <- df1 %>%
  group_by(Mass, Spin) %>%
  dplyr::summarise(Judgment = mean(Judgment))
sds   <- df1 %>%
  group_by(Mass, Spin) %>%
  dplyr::summarise(SD = sd(Judgment))
within_error <- summarySEwithin(x, measurevar="Judgment", withinvars=c("Spin","Mass"),
                                idvar="Participant")
df4 <- bind_cols(means,sds[3])
SEs <- as.tibble(within_error)
df5 <- inner_join(SEs[,c(1:2,4:7)],df4, by = c("Mass", "Spin"))

# Plot means and standard error for all 9 conditions of unidirectional wielding mode

p1 <- ggplot(subset(df3,Arc=="Full"), aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

# Plot means and standard error for all 9 conditions of bidirectional wielding mode

p2 <- ggplot(subset(df3,Arc=="Half"), aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

# Plot means and standard error for all 9 conditions of both wielding modes

p3 <- ggplot(SEs, aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.justification=c(2,-.5), legend.position=c(1,0)) +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

# Arrange plots

row2 <- plot_grid(p1, p2, labels = c("B", "C"), nrow=1)
plot_grid(p3, row2, ncol=1, labels=c('A',''))

# Mixed effects ANOVA with Spin, Mass, and Spin x Arc fixed effects and Participant as random effect.

mod1 <- aov(Judgment ~ Spin + Mass + Spin:Arc + Error(Participant), data=df1)

summary(mod1)

