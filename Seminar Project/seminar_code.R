# ---------------
# ----  Packages

install.packages("readr")
install.packages("ggplot2")
install.packages("rdd")
install.packages("rddtools")
install.packages("rdrobust")
install.packages("magrittr")
install.packages("tidyverse")
install.packages("broom")
install.packages("readxl")
install.packages("sandwich")
install.packages("VGAM")
install.packages("rddensity")
install.packages("ldlocrand")

library(tidyverse)
library(broom)
library(readr)
library(ggplot2)
library(rdd)
library(rddtools)
library(rdrobust)
library(magrittr)
library(readxl)
library(sandwich)
library(tidyverse)
library(VGAM)
library(ldlocrand)

# -----------------------
# ----  Datasets

setwd("C:/Users/User/Desktop/Seminar/Code")

#Full dataSet
Data_viral_on_top <- read_csv("viral50_on_top200.csv")
Data_top_on_viral <- read_csv("top50_on_viral100.csv")

Data <- Data_viral_on_top #insert Dataset
Data <- Data_top_on_viral #insert Dataset

# -------------------
# Histogram 
ggplot(Data, aes(x=rank, fill = ifelse(rank > 50,'red','green'), color = ifelse(rank > 50,'red','green'))) +
  geom_histogram(position="dodge", binwidth = 1) +
  geom_vline(aes(xintercept=50.5), linetype="dashed") +
  scale_color_manual(values=c('#87d8b0','#989898'), labels=c("Treatment", "Control"),name=("")) +
  scale_fill_manual(values=c('#87d8b0','#989898'), labels=c("Treatment", "Control"),name=("")) +
  theme_minimal()+theme(legend.position="bottom")

# ---  Simple discontinuity model (no covariates)

RD_data <- rdd_data(data = Data, y = within_a_month, x = -rank, cutpoint = -50)

linear <- rdd_reg_lm(rdd_object = RD_data, slope = "separate", order=1)
quad <- rdd_reg_lm(rdd_object = RD_data, slope = "separate", order=2)
probit <- rdd_gen_reg(RD_data, fun= glm, family=binomial(link='probit'))
probit_quad <- rdd_gen_reg(RD_data, fun= glm, order=2, family=binomial(link='probit'))

# -----------------------------
# + both covariates
covar = data.frame(Data$collaboration, Data$appeared_before)

RD_data <- rdd_data(data = Data, y = within_a_month, x = -rank, cutpoint = -50, covar = covar)
linear_cov <- rdd_reg_lm(RD_data, slope = "separate", order=1,
                         covariates=c("Data.collaboration", "Data.appeared_before"))
quad_cov <- rdd_reg_lm(RD_data, slope = "separate", order=2,
                       covariates=c("Data.collaboration", "Data.appeared_before"))
probit_cov <- rdd_gen_reg(RD_data, fun= glm, order=1,
                          covariates=c("Data.collaboration", "Data.appeared_before"), 
                          family=binomial(link='probit'))
probit_cov_quad <- rdd_gen_reg(RD_data, fun= glm, order=2,
                        covariates=c("Data.collaboration", "Data.appeared_before"), 
                        family=binomial(link='probit'))
# ------------------------------------

## Plots

#fit 
fitted <- linear

fit <- as.matrix(fitted(fitted))
Data_plot <- cbind(Data,fit)
df <- data.frame(Data_plot)

#overlay of data points 
vector <- sequence(100)
df_modify <- aggregate(Data$within_a_month, list(Data$rank), mean)
data_overlay <- cbind.data.frame(df_modify$x, vector)

df <- Data
ggplot(df, aes(df$rank, df$within_a_month, color = as.factor(df$below50_1)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_point(data = data_overlay, aes(vector, df_modify$x, color = 'red'), shape=19, alpha=0.75)+
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_lin(data = fortify(df), aes(x = as.numeric(df$rank), y = df$fit), se=F)+
  xlab("Rank in the Viral100 Chart") +
  ylab("Probability of Being in the\n Top200 Chart Within 30 Days" ) +
  scale_color_manual(values=c('#000000','#57504d', '#87d8b0'), labels=c("Treatment", "Control", "Local Average"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

ggplot(Data, aes(Data$rank, Data$within_a_month, color = as.factor(Data$below50_0)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_point(data = data_overlay, aes(vector, `df_modify$x`, color = 'red'), shape=19, alpha=0.75)+
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_smooth(data = fortify(fit), aes(x = as.numeric(Data$rank), y = .fitted), se=F)+
  xlab("Rank in the Global Top Chart") +
  ylab("Probability of Being in the\n Global Viral Chart Within 30 Days" ) +
  scale_color_manual(values=c('#000000','#57504d', '#87d8b0'), labels=c("Treatment", "Control", "Local Average"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

ggplot(Data, aes(Data$rank, Data$within_a_month, color = as.factor(Data$below50_0)))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_point(data = data_overlay, aes(vector, `df_modify$x`, color = 'red'), shape=19, alpha=0.75)+
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_smooth(data = fortify(fit), aes(x = as.numeric(Data$rank), y = .fitted), se=F)+
  xlab("Rank in the Global Viral Chart") +
  ylab("Probability of Being in the\n Global Top Chart Within 30 Days" ) +
  scale_color_manual(values=c('#000000','#57504d', '#87d8b0'), labels=c("Treatment", "Control", "Local Average"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

# -------------------------
#plot Probit

#Viral on Top

vector <- sequence(100)
df_modify <- aggregate(Data$within_a_month, list(Data$rank), mean)
data_overlay <- cbind.data.frame(df_modify$x, vector)

dfFromViral <- read.csv("viral50_on_top200.csv")
probitLinearViral <- glm(data = dfFromViral, within_a_month ~ 1 + below50_1+ I(rank-50) + below50_1:I(rank-50), family = binomial(link = "probit"))
probitQuadViral <- glm(data = dfFromViral, within_a_month ~ 1 + below50_1 + I(rank-50) + below50_1:I(rank-50) + I((rank-50)^2) + below50_1:I((rank-50)^2), family = binomial(link = "probit"))
tempFrameViral <- data.frame(rank = rep(seq(from = 1, to = 100, length.out = 100)))
tempFrameViral[, "I(rank-50)"] <- tempFrameViral$rank - 50
tempFrameViral[, "below50_1"] <- as.numeric(tempFrameViral$rank<=50)
tempFrameViral[, "below50_1:I(rank-50)"] <- tempFrameViral$`I(rank-50)`*tempFrameViral$below50_1
tempFrameViral[, "I((rank-50)^2)"] <- (tempFrameViral$`I(rank-50)`)^2
tempFrameViral[, "below50_1:I((rank-50)^2)"] <- (tempFrameViral$below50_1)*tempFrameViral$`I((rank-50)^2)`
tempFrameViral[, c("pQuad", "seQuad")] <- predict(probitQuadViral, tempFrameViral, type = "response", se.fit=TRUE)[-3]
tempFrameViral[, c("pLinear", "seLinear")] <- predict(probitLinearViral, tempFrameViral, type = "response", se.fit=TRUE)[-3]

ggplot(tempFrameViral, aes(rank, pLinear, color = ifelse(rank >= 51, 'red', 'green')))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_point(data = data_overlay, aes(vector, `df_modify$x`, color = 'yellow'), shape=19, alpha=0.75)+
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_smooth(data = tempFrameViral, aes(rank, pLinear), se=F)+
  xlab("Rank in the Global Viral Chart") +
  ylab("Probability of Being in the\n Global Top Chart Within 30 Days" ) +
  scale_color_manual(values=c('#000000','#57504d', '#87d8b0'), 
                     labels=c("Treatment", "Control", "Local Average"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#Top on Viral

dfFromTop <- read.csv("top50_on_viral100.csv")
probitLinearTop <- glm(data = dfFromTop, within_a_month ~ 1 + below50_1 + I(rank-50) + below50_1:I(rank-50), family = binomial(link = "probit"))
probitQuadTop <- glm(data = dfFromTop, within_a_month ~ 1 + below50_1 + I(rank-50) + below50_1:I(rank-50) + I((rank-50)^2) + below50_1:1:I((rank-50)^2), family = binomial(link = "probit"))
tempFrameTop <- data.frame(rank = rep(seq(from = 1, to = 200, length.out = 200)))
tempFrameTop[, "I(rank-50)"] <- tempFrameTop$rank - 50
tempFrameTop[, "below50_1"] <- as.numeric(tempFrameTop$rank<=50)
tempFrameTop[, "below50_1:I(rank-50)"] <- tempFrameTop$`I(rank-50)`*tempFrameTop$below50_1
tempFrameTop[, "I((rank-50)^2)"] <- (tempFrameTop$`I(rank-50)`)^2
tempFrameTop[, "below50_1:I((rank-50)^2)"] <- (tempFrameTop$below50_1)*tempFrameTop$`I((rank-50)^2)`
tempFrameTop[, c("pQuad", "seQuad")] <- predict(probitQuadTop, tempFrameTop, type = "response", se.fit=TRUE)[-3]
tempFrameTop[, c("pLinear", "seLinear")] <- predict(probitLinearTop, tempFrameTop, type = "response", se.fit=TRUE)[-3]

vector <- sequence(200)
df_modify <- aggregate(Data$within_a_month, list(Data$rank), mean)
data_overlay <- cbind.data.frame(df_modify$x, vector)

ggplot(tempFrameTop, aes(rank, pQuad, color = ifelse(rank >= 51, 'red', 'green')))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_point(data = data_overlay, aes(vector, `df_modify$x`, color = 'yellow'), shape=19, alpha=0.75)+
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_smooth(data = tempFrameTop, aes(rank, pQuad), se=F)+
  xlab("Rank in the Top200 Chart") +
  ylab("Probability of Being in the\n Viral100 Chart Within 30 Days" ) +
  scale_color_manual(values=c('#000000','#57504d', '#87d8b0'), 
                     labels=c("Treatment", "Control", "Local Average"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")


# --------------------------
# Tobit

newDf <- read.csv("viral50_on_top200.csv")
names(newDf)[names(newDf) == '?..rank'] <- 'rank'
newDf[is.na(newDf$best_rank),]$best_rank <- 201
m <- vglm(best_rank ~ below50_1 + I(rank-50) + below50_1:I(rank-50), tobit(Upper = 201),
                  data = newDf, maxit = 100)
mQuad <- vglm(best_rank ~ below50_1 + I(rank-50) + I((rank-50)^2) + below50_1:I(rank-50) + 
                        below50_1:I((rank-50)^2), 
                      tobit(Upper = 201),data = newDf, maxit = 100)

mCov <- vglm(best_rank ~ appeared_before + collaboration + below50_1 + I(rank-50) + 
                       below50_1:I(rank-50), tobit(Upper = 201),data = newDf, maxit = 100)

mQuadCov <- vglm(best_rank ~ appeared_before + collaboration + below50_1 + I(rank-50) 
                         + I((rank-50)^2) + below50_1:I(rank-50) + below50_1:I((rank-50)^2), 
                         tobit(Upper = 201),data = newDf, maxit = 100)

dfFromTop <- read.csv("top50_on_viral100 (2).csv")
names(dfFromTop)[names(dfFromTop) == '?..rank'] <- 'rank'
dfFromTop[is.na(dfFromTop$best_rank),]$best_rank <- 101
mFromTop <- vglm(best_rank ~ below50_1 + I(rank-50) + below50_1:I(rank-50), 
                         tobit(Upper = 101),data = dfFromTop, maxit = 100)
mFromTopQuad <- vglm(best_rank ~ below50_1 + I(rank-50) + I((rank-50)^2) + below50_1:I(rank-50) + below50_1:I((rank-50)^2), tobit(Upper = 101),data = dfFromTop, maxit = 100)
mFromTopCov <- vglm(best_rank ~ appeared_before + collaboration + below50_1 + I(rank-50) + below50_1:I(rank-50), tobit(Upper = 101),data = dfFromTop, maxit = 100)
round(mFromTopCov@coefficients, digits = 3)=
mFromTopQuadCov <- vglm(best_rank ~ appeared_before + collaboration + below50_1 + I(rank-50) + I((rank-50)^2) + below50_1:I(rank-50) + below50_1:I((rank-50)^2), tobit(Upper = 101),data = dfFromTop, maxit = 100)

# --------------------------
# Tobit plot
ggplot(tempFrameTop, aes(tempFrameTop$rank, tempFrameTop$LinearFitted, color = ifelse(tempFrameTop$rank < 51,'red','green')))+
  # Add the line using the fortified fit data, plotting the x vs. the fitted values
  geom_vline(xintercept=50, linetype="dashed", alpha = 0.5)+
  geom_line(data = tempFrameTop, aes(x = as.numeric(tempFrameTop$rank), y = tempFrameTop$LinearFitted), se=F, size=1.2)+
  xlab("Rank in the Top200 Chart") +
  ylab("Latent Best Rank\n in Viral100 Chart Within 30 Days" ) +
  scale_color_manual(values=c('#57504d','#000000'), labels=c("Treatment", "Control"),name=(""))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom", legend.text = element_text(size=12))


# --------------------------
#Non parametric

##Local linear regression
kViralLinear <- RDestimate(formula = within_a_month ~ rank, data = newDf, cutpoint = 50.5, se.type = "HC", verbose = TRUE, frame = TRUE, model = TRUE )
kViralLinearCov <- RDestimate(formula = within_a_month ~ rank | collaboration + appeared_before, data = newDf, cutpoint = 50.5, se.type = "HC", verbose = TRUE, frame = TRUE, model = TRUE )
kTopLinear <- RDestimate(formula = within_a_month ~ rank, data = dfFromTop, cutpoint = 50.5, se.type = "HC", verbose = TRUE, frame = TRUE, model = TRUE )
kTopLinearCov <- RDestimate(formula = within_a_month ~ rank | collaboration + appeared_before, data = dfFromTop, cutpoint = 50.5, se.type = "HC", verbose = TRUE, frame = TRUE, model = TRUE )

##Local Randomization design
rViral <- rdrandinf(Y = newDf$within_a_month, R = newDf$rank, cutoff = 50.5, covariates = newDf$collaboration, wmasspoints = TRUE)
rTop <- rdrandinf(Y = dfFromTop$within_a_month, R = dfFromTop$rank, cutoff = 50.5, covariates = dfFromTop$collaboration, wmasspoints = TRUE)

#------------------------- 
# McCrary tests
#Run section twice: once for viral_on_top, once for top_on_viral

#Perform McCrary test for a range of placebo cutoff points and put them all in the same dataframe
McCrary_df = NULL
#20:80 for viral_on_top data, 20:150 for top_on_viral data
for (i in 20:80){
  RD_data <- rdd_data(data = Data, y = within_a_month, x = rank, cutpoint = i)
  x <- dens_test(RD_data, plot = FALSE)
  McCrary_df = rbind(McCrary_df, data.frame(i, x$p.value))
}

#plot P-values
ggplot(McCrary_df, aes(i))+
  xlab("Cutoff Point") +
  ylab("P-value of the McCrary Test" ) +
  scale_y_continuous(trans='log10') +
  geom_line(aes(y = x.p.value, color = "P-value"), size = 1) +
  geom_hline(yintercept = 0.05, color ='#000000', size = 0.5) +
  geom_vline(xintercept = 51, color = '#000000', size = 1, linetype = 'dotted') +
  scale_color_manual(values='#87d8b0', labels="P-value",name=("")) +
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#-----------------------
#Frandsen tests
#Run section twice: once for viral_on_top, once for top_on_viral

#Perform Frandsen test for a range of placebo cutoff points and put them all in the same dataframe
Frandsen_df = NULL
#20:80 for viral_on_top data, 20:150 for top_on_viral data
for (i in 20:80){
  RD_data <- rdd_data(data = Data, y = within_a_month, x = rank, cutpoint = i)
  x <- rddisttest::rddisttest(Data$rank, i)
  Frandsen_df = rbind(Frandsen_df, data.frame(i, x))
}

#Plot P-values
ggplot(Frandsen_df, aes(i))+
  xlab("Cutoff Point") +
  ylab("P-value of the Frandsen Test" ) +
  scale_y_continuous(trans='log10') +
  geom_line(aes(y = x, color = "P-value"), size = 1) +
  geom_hline(yintercept = 0.05, color ='#000000', size = 0.5) +
  geom_vline(xintercept = 51, color = '#000000', size = 1, linetype = 'dotted') +
  scale_color_manual(values='#87d8b0', labels="P-value",name=("")) +
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#---------------------
#Bandwidth selection robustness

#!!!for Viral on top, run rdd_reg_lm with bw=50, for top on viral, run with bw=150!!!
RD_data <- rdd_data(data = Data, y = within_a_month, x = -rank, cutpoint = -50)

linear <- rdd_reg_lm(rdd_object = RD_data, slope = "separate", order=1,bw=150)
quad <- rdd_reg_lm(rdd_object = RD_data, slope = "separate", order=2,bw=150)
probit_lin <- rdd_gen_reg(RD_data, fun= glm, family=binomial(link='probit'),bw=150)
probit_quad <- rdd_gen_reg(RD_data, fun= glm, order=2, family=binomial(link='probit'),bw=150)

covar = data.frame(Data$collaboration, Data$appeared_before)

RD_data <- rdd_data(data = Data, y = within_a_month, x = -rank, cutpoint = -50, covar = covar)
linear_cov<- rdd_reg_lm(RD_data, slope = "separate", order=1, bw=150,
                        covariates=c("Data.collaboration", "Data.appeared_before"))
quad_cov <- rdd_reg_lm(RD_data, slope = "separate", order=2, bw=150,
                       covariates=c("Data.collaboration", "Data.appeared_before"))
probit_cov_linear <- rdd_gen_reg(RD_data, fun= glm, bw=150,
                                 covariates=c("Data.collaboration", "Data.appeared_before"), 
                                 family=binomial(link='probit'))
probit_cov_quad <- rdd_gen_reg(RD_data, fun= glm, order=2,bw=150,
                               covariates=c("Data.collaboration", "Data.appeared_before"), 
                               family=binomial(link='probit'))

#Get bandwidth sensitivty data
#Change first argument to the data you want to plot: linear, quad, linear_cov or quad_cov
bw_data <- plotSensi(linear_cov, from = 1, to = 150, by = 1, output = 'data', order = 1, vcov. = vcovHC(linear_cov, type = "HC"),plot=FALSE)

#plot for bandwidth graph
ggplot(bw_data, aes(bw))+
  xlab("Bandwidth") +
  ylab("Estimated Treatment Effect" ) +
  geom_line(aes(y = LATE, color = "Estimated Treatment Effect"), size = 1) +
  geom_line(aes(y = CI_low, color = "95% Confidence Interval"), size = 1, linetype = 'dashed') +
  geom_line(aes(y = CI_high, color = "95% Confidence Interval"), size = 1, linetype = 'dashed') +
  geom_hline(yintercept = 0, color ='#000000', size = 0.5) +
  #bit of a complex specification but otherwise it kept getting the colors wrong in the legend
  scale_color_manual(values=c('#57504d','#87d8b0'), labels=c("95% Confidence Interval", "Estimated Treatment Effect"),name=(""), 
                     guide = guide_legend(reverse = T))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")

#---------------------
#Placebo's robustness

#!!!Load top_on_viral data and run lines 111 to 126!!!
placebo_data_linear <- plotPlacebo(linear, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_quad <- plotPlacebo(quad, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_linear_cov <- plotPlacebo(linear_cov, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_quad_cov <- plotPlacebo(quad_cov, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)

placebo_data_probit_linear <- plotPlacebo(probit_lin, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_probit_quad <- plotPlacebo(probit_quad, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_probit_linear_cov <- plotPlacebo(probit_cov_linear, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)
placebo_data_probit_quad_cov <- plotPlacebo(probit_cov_quad, from = 0.15, to = 0.85, by = 1, vcov. = vcovHC, output = 'data', plot = FALSE)

#Plot the placebo data
#Change argument based on what data you want to plot
graph_placebo_data <- placebo_data_probit_linear
#run ggplot
ggplot(graph_placebo_data, aes(-cutpoint))+
  xlab("Cutoff Point") +
  ylab("Estimated Treatment Effect" ) +
  geom_line(aes(y = LATE, color = "Estimated Treatment Effect", group = position), size = 1) +
  geom_line(aes(y = CI_low, color = "95% Confidence Interval", group = position), size = 1, linetype = 'dashed') +
  geom_line(aes(y = CI_high, color = "95% Confidence Interval", group = position), size = 1, linetype = 'dashed') +
  geom_hline(yintercept = 0, color ='#000000', size = 0.5) +
  geom_vline(xintercept = 50, color = '#000000', size = 1, linetype = 'dotted') +
  scale_color_manual(values=c('#57504d','#87d8b0'), labels=c("95% Confidence Interval", "Estimated Treatment Effect"),name=(""), 
                     guide = guide_legend(reverse = T))+
  theme_set(theme_bw() + theme(legend.key=element_blank())) +
  theme(legend.position = "bottom")