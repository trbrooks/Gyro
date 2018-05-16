library(readxl)
library(dplyr)
library(tidyr)
library(tibble)
library(ggplot2)
library(cowplot)
library(lme4)


getwd()
setwd("/Users/trbrooks/desktop/diss stuff/Exp1")

files <- dir()[grep(".xlsx",dir())] 

for (i in 1:length(files)) {
  if(i==1){x<-NULL}
  temp <- read_excel(files[i]) %>%
  .[complete.cases(.),]
  x<-rbind(x,temp)
}

x$Mass <- as.factor(x$Mass)
x$Judgment <- as.numeric(x$Judgment)
x$Spin <- recode(x$Spin, `-7k`=-1, `0`=0, `7k`=1) %>%
  as.factor
x$Abspin<-recode(x$Spin, `-1`=1, `0`=0, `1`=1) %>%
  as.factor

library(Rmisc)

library(ggplot2)
head(x)
df1  <- x %>% 
  as.tibble %>%
  mutate_each(funs(factor), Spin)

as.numeric(df1$Spin)

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

p1 <- ggplot(subset(df3,Arc=="Full"), aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

p2 <- ggplot(subset(df3,Arc=="Half"), aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.position="none") +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

p3 <- ggplot(SEs, aes(x=Mass,y=Judgment, group=Spin, color=Spin)) +
  geom_line() +
  geom_point() +
  theme(legend.justification=c(2,-.5), legend.position=c(1,0)) +
  geom_errorbar(aes(ymin=Judgment-se, ymax=Judgment+se, color=Spin), width=.05)

row2 <- plot_grid(p1, p2, labels = c("B", "C"), nrow=1)
plot_grid(p3, row2, ncol=1, labels=c('A',''))

mod1 <- aov(Judgment ~ Spin + Mass + Spin:Arc + Error(Participant), data=df1)
mod2 <- lm(Judgment ~ Mass + Spin + (1|Participant), data=df1)
mod3 <- lm(Judgment ~ Mass + Spin + Spin:Arc + (1|Participant), data=df1)

summary(mod1)

df1 %>%
  group_by(Mass) %>%
  dplyr::summarise(Judgment = sd(Judgment))


