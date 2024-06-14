# We first load necessary libraries and functions
source('helper_functions.r')
library(psych)
library(tidyverse)
library(patchwork)
library(knitr)
library(kableExtra)

### START Open data file ###
# Insert code below that loads your dataset into a variable called 'rawdata'
rawdata = read.csv("dataset17_premade.csv")

### END Open data file ###

data = rawdata #copy raw datafile
data$condition = recode(rawdata$condition, "1"=0, "2"=1) #recode condition to 0 and 1
data$correct = data$correct



### START Explore through visual inspection ###
# Insert code below to check your dataset by looking at the raw data

data %>%
  group_by(ID, condition) %>%
  summarise(n=n()) %>%
  ungroup() %>%
  mutate(ID = as.character(ID),
         condition = as.character(condition)) %>%
  add_row("ID"="Total", "condition"="Total", "n"=sum(.$n)) %>%
  kable() %>%
  kable_styling()

# accuracy per condition
data %>%
  group_by(condition) %>%
  summarise(sum = sum(correct),
            n = n(),
            freq = sum(correct)/n())

#boxplot of response times - outlier for condition 1 will be removed
ggplot(data, aes(x=as.factor(condition), y=rt)) +
  geom_boxplot()

#remove one observation with rt > 20s, all other observations are < 4s
data_clean = filter(data, rt < 10000)

data_clean %>%
  ggplot(aes(x=as.factor(condition), y=rt, fill=as.factor(correct))) +
  geom_boxplot()

data_clean %>%
  ggplot(aes(x=rt, colour=as.factor(condition))) +
  geom_density()

#Histogram of reaction times
data_clean %>%
  ggplot(aes(x=rt)) +
  geom_histogram()

#Histogram of reaction times per correct reactions
data_clean %>%
  ggplot(aes(x=rt, fill=as.factor(correct))) +
  geom_histogram()

#Histogram of reaction times per condition
data_clean %>%
  ggplot(aes(x=rt, fill=as.factor(condition))) +
  geom_histogram() +
  theme_minimal() +
  labs(y=element_blank(), x="Reaction times", fill="Condition") +
  scale_fill_discrete(labels=c("0"="LIPT", "1"="HITP")) +
  theme(text = element_text(size=15))

# descriptive table
data_clean %>%
  group_by(condition) %>%
  summarise("mean" = mean(rt),
            "sd" = sd(rt),
            "median" = median(rt)) %>%
  kable() %>%
  kable_styling()

### START Inspect data: paired t-test over RT data by condition
# Insert code below to check whether RTs differ by condition. Note: you should do this by aggregating data
# by subject, using the median (due to skewed distribution of response times!)
data_aggr = data_clean %>%
  group_by(ID, condition) %>%
  summarise(rt_aggr = median(rt))
shapiro.test(data_aggr$rt_aggr) #shapiro wilk test on reaction times
#paired wilcoxon test on reaction times
wilcox.test(rt_aggr~condition, data=data_aggr, paired=T, alternative="greater", conf.int=T)

### START Fit model for each participant x condition
# Now we get down to business. The fit_data() function takes one input: a dataframe. It returns a named list of length 5,
# for the 5 parameters we are estimating. It is your job to think of a good data structure to use for subsequent analysis.
# Make sure you pass only a subset of your dataframe! Insert code below.
# "s"		SD of drift rates
# This reflects variability in drift rates. However, as this parameter does not have an easily interpretable cognitive mapping, this parameter does not significantly differ between conditions in the provided datasets.
# "A"		upper limit of starting point
# This reflects the starting point of the evidence accumulation process. It reflects bias or expectations for one choice.
# "ter"	non-decision time
# This reflects the time necessary for processes that are not related to evidence integration. For example, the time it takes for activation of the motor cortex to result in the hand pressing the response button, or the time it takes for visual information to get from the stimulus to the visual cortex.
# "b"		threshold
# The distance from 0 to the threshold. It reflects cautiousness: lower thresholds lead to faster responses but a higher error rate.
# "v1"	drift rate
# The quality of the evidence or difficulty of the problem. Higher drift rates lead to faster and more accurate responses.
df_fit = matrix(ncol=7, nrow=0) #helper matrix for model parameters to fill estimates in
colnames(df_fit) = c("ID", "condition", "s", "A", "ter", "b", "v1") #colnames for parameters
df_fit = as.data.frame(df_fit) #turn into dataframe

#iteration to 
for(id in unique(data_clean$ID)){ #iterate over subject
  for(cond in unique(data_clean$condition)) { #iterate over condition

    parameters = fit_data(subset(data_clean, condition==cond & ID==id)) #estimate parameters
    
    #save estimators in a dataframe row
    helper = data.frame("ID" = id,
                        "condition" = cond,
                        "s" = parameters["s"], #intertrial variability drift
                        "A" = parameters["A"], #upper threshold
                        "ter" = parameters["ter"], #non-decision time
                        "b" = parameters["b"], #bias / starting point
                        "v1" = parameters["v1"]) #drift rate
  
    df_fit = rbind(df_fit, helper) #add row to dataframe created above
  }
}
glimpse(df_fit)
### END Fit model for each participant x condition

### Check normality of differences
df_diffs = df_fit %>% #calculate differences between conditions per subject
  group_by(ID) %>%
  summarise(diff_A = diff(A),
            diff_v = diff(v1),
            diff_b = diff(b),
            diff_ter = diff(ter),
            diff_s = diff(s))

#shapiro wilk tests for normality
shapiro.test(df_diffs$diff_A) #for threshold
shapiro.test(df_diffs$diff_v) #drift
shapiro.test(df_diffs$diff_b) #bias
shapiro.test(df_diffs$diff_ter) #non-reaction time
shapiro.test(df_diffs$diff_s) #drift variability



### START Check which parameter(s) differ between conditions
# Insert code below to check whether parameters differ by condition.
t.test(s~condition, data=df_fit, paired=T) #drift variability
t.test(A~condition, data=df_fit, paired=T, alternative="g") #h2
t.test(ter~condition, data=df_fit, paired=T) #non-reaction time
t.test(b~condition, data=df_fit, paired=T) #H4
wilcox.test(v1~condition, data=df_fit, paired=T, alternative="g", conf.int=T) #h3
wilcox.test(v1~condition, data=df_fit, paired=T, alternative="l", conf.int=T) #h3 exploration

#exploration of ter and s
t.test(ter~condition, data=df_fit, paired=T) #t-test for ter
t.test(s~condition, data=df_fit, paired=T) #t-test for condition

### END Check which parameter(s) differ between conditions

#descriptive parameters
df_fit %>% #mean and sd for each parameter and condition
  pivot_longer(cols=c(A, b, v1, ter, s), names_to="parameter") %>%
  group_by(condition, parameter) %>%
  summarise(mean = mean(value),
            sd = sd(value)) %>%
  kable() %>%
  kable_styling()

#plot of individual effects
df_diffs = df_fit %>%
  group_by(ID) %>%
  summarise(diff_s = diff(s),
            diff_A = diff(A),
            diff_ter = diff(ter),
            diff_b = diff(b),
            diff_v1 = diff(v1))

# Plots for parameter distribution over subjects
# plotting function for all parameters
ind_line_plot = function(variable, varname, y_ul=NA, data=df_fit) {
  data %>%
    mutate(ID=as.factor(ID), condition=as.factor(condition)) %>%
    ggplot(aes(x=condition, colour=ID)) +
    geom_line(aes(y=!!enquo(variable), group=ID)) +
    ylim(c(0, y_ul)) +
    theme_minimal() +
    scale_colour_discrete(guide="none") +
    scale_x_discrete(labels=c("0"="LITP", "1"="HITP"),
                     expand = c(0.1,0)) +
    labs(x="Condition", y=varname) +
    theme(text = element_text(size=15))
}

#create plot using plotting function
v_plot = ind_line_plot(v1, "Drift rate", 1.2) + #drift rate
  labs(title="A.")
v_plot
A_plot = ind_line_plot(A, "Threshold") + #threshold
  labs(title="B.")
A_plot
b_plot = ind_line_plot(b, "Bias rate") + #bias
  labs(title="D.")
b_plot
ter_plot = ind_line_plot(ter, "Non-decision time") #non-decision reaction time
ter_plot
s_plot = ind_line_plot(s, "Drift variability") +
  labs(title="C.")
s_plot

v_plot + A_plot + s_plot + b_plot + plot_layout(axis_titles="collect", guides="collect")


#plot parameters via errorbars and mean
est_plot = function(variable, axis_name, data=df_fit) {
  data %>%
    group_by(condition) %>%
    summarise("mean"=mean(!!enquo(variable)),
              "sd"=sd(!!enquo(variable)),
              "n"=n()) %>%
    mutate(l_mean = mean - qt(0.975,df=n-1)*sd/sqrt(n),#calculate lower 95% CI
           u_mean = mean + qt(0.975,df=n-1)*sd/sqrt(n)) %>% #calculate upper 95% CI
    ggplot(aes(x=as.factor(condition))) +
    geom_point(aes(y=mean), size=7) +
    geom_errorbar(aes(ymin=l_mean,ymax=u_mean), width=.2, linewidth=1.2) +
    coord_flip() +
    theme_minimal() +
    labs(x=element_blank(), y=axis_name) +
    scale_x_discrete(labels=c('LIPT', 'HITP')) +
    theme(text = element_text(size=20))
}
est_RT = est_plot(rt, "Reaction time (ms)", data=data_clean) +
  labs(title="A.") +
  scale_y_continuous(limits=c(520, 640), breaks=seq(520, 640, 20))
est_RT
est_v1 = est_plot(v1, "Drift rate") + labs(title="B.") +
  scale_y_continuous(limits=c(.6, 1.1), breaks=seq(.6, 1.1, .1))
est_v1
est_A = est_plot(A, "Threshold") + labs(title="C.") +
  scale_y_continuous(limits=c(290, 370), breaks=seq(290, 370, 20))
est_A
est_b = est_plot(b, "Bias") + labs(title="D.") +
  scale_y_continuous(limits=c(360, 420), breaks=seq(360, 420, 20))
est_b
est_ter = est_plot(ter, "Non-decision time") + labs(title="E.") +
  scale_y_continuous(limits=c(230, 330), breaks=seq(230, 330, 20))
est_ter
est_s = est_plot(s, "Drift variability") + labs(title="F.") +
  scale_y_continuous(limits=c(.2, .3), breaks=seq(.2, .3, .025))
est_s

(est_RT + est_v1) / (est_A + est_b) / (est_ter + est_s)

# Effect sizes
cohen.d(rt_aggr~condition, data=data_aggr) #rt
cohen.d(A~condition, data=df_fit) #threshold
cohen.d(v1~condition, data=df_fit) #threshold
cohen.d(b~condition, data=df_fit) #threshold
cohen.d(ter~condition, data=df_fit) #threshold
cohen.d(s~condition, data=df_fit) #threshold
