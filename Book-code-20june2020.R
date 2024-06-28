# Book-code-20june2020.R
# Directory: /home/baileraj/Book-StatsBehindTheHeadlines
# 

# setwd("/home/baileraj/Book-StatsBehindTheHeadlines")
#getwd()

########## C H A P T E R     2 ......................................................

# read the UN population projection data ...
library(xlsx)
lower95 <- read.xlsx(file="UN_PPP2019_Output_PopTot.xlsx",sheetIndex = 1, startRow = 17)
lower80 <- read.xlsx(file="UN_PPP2019_Output_PopTot.xlsx",sheetIndex = 2, startRow = 17)
medPPP <-  read.xlsx(file="UN_PPP2019_Output_PopTot.xlsx",sheetIndex = 3, startRow = 17)
upper80 <- read.xlsx(file="UN_PPP2019_Output_PopTot.xlslsx",sheetIndex = 4, startRow = 17)
upper95 <- read.xlsx(file="UN_PPP2019_Output_PopTot.xlsx",sheetIndex = 5, startRow = 17)

# restrict to only the World
library(dplyr)
World.L.95 <- lower95 %>% 
  filter(Region..subregion..country.or.area == "WORLD") %>% 
  select(-c(Index, Notes, NA., NA..1, Type))
World.L.95

World.L.80 <- lower80 %>% 
  filter(Region..subregion..country.or.area == "WORLD") %>% 
  select(-c(Index, Notes, NA., NA..1, Type))
World.med <- medPPP %>% 
  filter(Region..subregion..country.or.area == "WORLD") %>% 
  select(-c(Index, Notes, NA., NA..1, Type))
World.U.80 <- upper80 %>% 
  filter(Region..subregion..country.or.area == "WORLD") %>% 
  select(-c(Index, Notes, NA., NA..1, Type))
World.U.95 <- upper95 %>% 
  filter(Region..subregion..country.or.area == "WORLD") %>% 
  select(-c(Index, Notes, NA., NA..1, Type))

World.wide <- rbind(
  World.L.95,
  World.L.80,
  World.med,
  World.U.80,
  World.U.95
)

# final steps for creating wide version of world projection data .................
library(stringr)
World.wide2 <- World.wide %>% 
  mutate(E = substring(Variant,first=1,last=8),
         estimate = str_remove_all(E, "[[:space:]]")) %>% 
  select(-c(Region..subregion..country.or.area, Variant, E))
View(World.wide2)  

# convert to long format .......................................................
library(tidyr)
WW.long <- World.wide2 %>% 
  gather(key=cyear, value=as.numeric(pop), -estimate) %>% 
  mutate(year = as.numeric(substring(cyear,2,5))) %>% 
  select(-cyear)
WW.long

# convert to wide format with rows=different years and cols=estimates ..........
WW.wide <- WW.long %>% 
  mutate(pop=as.numeric(pop)) %>% 
  spread(key=estimate, value=pop) 

# graph construction
library(ggthemes)
library(ggplot2)
World.pop.plot <- ggplot(data=WW.wide, aes(x=year,y=MedianP) ) +
#  geom_ribbon(aes(x=year, ymin=Lower95, ymax=Upper95), alpha=0.5,fill="lightgrey") +
  geom_ribbon(aes(x=year, ymin=Lower80, ymax=Upper80), fill="lightgrey") +
  geom_line(size=1.5) +
  # geom_line(aes(x=year, y=Lower95), alpha=1, lty=3) +
  # geom_line(aes(x=year, y=Lower80), alpha=1, lty=2) +
  # geom_line(aes(x=year, y=Upper95), alpha=1, lty=3) +
  # geom_line(aes(x=year, y=Upper80), alpha=1, lty=2) +
  theme_minimal() +
  labs(y="", x="", 
       caption = "Data from UN Population Projections\n(https://population.un.org/wpp/Download/Probabilistic/Population/)") +
  geom_text(label="Predicted Population\n(BILLIONs)",
            x=2020, y=12000000, hjust=0, vjust=1, size=3) +
  geom_text(label="Year",
            x=2100, y=7800000, hjust=1, vjust=0, size=3) +
  scale_y_continuous(breaks = c(8000000,10000000,12000000),
                     labels = c("8","10","12"),
                     limits = c(7750000,12000000)) 
  
World.pop.plot
ggsave(filename = "World-pop-plot.jpg", plot=World.pop.plot,
       device = "jpeg",
       dpi=600,
       height=3,
       width=4)

########## C H A P T E R     3 ......................................................

sample(x=c(0,1,2,3,4,5), size=10, replace=TRUE)

sample(x=c(0,1,2,3,4,5), size=10, replace=TRUE, prob=c(.2,.3,.2))

nsymptoms <- 0:5
probsymptoms <- c(.15,.26,.18,.17,.14,.10)

sum(probsymptoms)
nbar <- sum(nsymptoms*probsymptoms)
nsd <- sqrt( sum((nsymptoms - nbar)^2 * probsymptoms))

c(nbar,nsd)

set.seed(45057)
test_sample <- sample(x=c(0,1,2,3,4,5), size=6595, replace=TRUE, prob=probsymptoms)
mean(test_sample)
sd(test_sample)
median(test_sample)
quantile(test_sample)

# mean = 2.19 and SD = 1.57
table(test_sample)
table(test_sample)/6595
table(test_sample)/sum(table(test_sample))

# hypothetical
my.df <- data.frame(score=0:5, 
                    freq1 = c(5,10,20,20,10,5), 
                    freq2 = c(20,10,5,5,10,20),
                    freq3 = c(0,0,35,35,0,0))

apply(my.df, 2, sum)

apply(my.df, 2, )

# summaries for ...
# ... sample 1
with(my.df, mean(rep(score,freq1)))
with(my.df, sd(rep(score,freq1)))
with(my.df, quantile(rep(score,freq1)))
# ... sample 2
with(my.df, mean(rep(score,freq2)))
with(my.df, sd(rep(score,freq2)))
with(my.df, quantile(rep(score,freq2)))
# ... sample 3
with(my.df, mean(rep(score,freq3)))
with(my.df, sd(rep(score,freq3)))
with(my.df, quantile(rep(score,freq3)))


library(dplyr)
my.df %>% mutate(avg1 = sum(score*freq1)/sum(freq1),
                 avg2 = sum(score*freq2)/sum(freq2),
                 avg3 = sum(score*freq3)/sum(freq3))

library(tidyr)
my.df2 <- my.df %>% 
  gather(key = "sample", value="count", freq1, freq2, freq3) %>% 
  mutate(sampleNum = substr(sample,start=5,stop=5),
         sampleName = paste0("Sample ",sampleNum)) %>% 
  select(-sample)

library(ggplot2)
ggplot(my.df2, aes(x=score, y=count)) +
  geom_col() +
  facet_grid(. ~ sampleNum) +
  coord_flip()
  
compare_dist_plot <- ggplot(my.df2, aes(x=score, y=count)) +
  geom_col() +
  facet_grid(. ~ sampleName, ) +
  scale_x_continuous(name="Score", breaks=0:5, labels=0:5) +
  coord_flip() +
  theme_minimal()

ggsave(filename = "Compare-dist.jpg", plot=compare_dist_plot,
       device = "jpeg",
       dpi=600,
       height=3,
       width=4)

# ------------------------------------------------
# waffle plots

allsamp <- c(1125, 2082, 2000, 817, 571)
intonly <- c(73,172,198,98,70)
extonly <- c(122,287,310,97,69)
bothsamp <- c(122, 283, 290, 202, 172)
timesocmed <- c("None","0-30 min","30 min to 3 h","3-6 h", "6+ h")
names(allsamp) <- timesocmed
names(intonly) <- timesocmed
names(extonly) <- timesocmed
names(bothsamp) <- timesocmed

library(waffle)
library(colorbrewer)
waffle(intonly, rows=20)

all_waffle_plot <- waffle(allsamp, rows=50, size=.5, colors=c(rep("grey",4),"black"))
both_waffle_plot <- waffle(bothsamp, rows=50, size=.5, colors=c(rep("grey",4),"black"))

ggsave(filename = "all-waffle.jpg", plot=all_waffle_plot,
       device = "jpeg",
       dpi=600,
       height=6,
       width=4)

ggsave(filename = "both-waffle.jpg", plot=both_waffle_plot,
       device = "jpeg",
       dpi=600,
       height=3,
       width=4)


########## C H A P T E R     7  ......................................................
# R package “deSolve” option
install.packages("deSolve")  # required installation step with first use
library(deSolve)

#
#  Healthy -> Infected -> Recovered


# Define simple model and 
virus.model <- function(Time,State,Pars) {
  with(as.list(c(State,Pars)),{
    dHealthy   = -kInf*Healthy
    dInfected  =  kInf*Healthy - kRec*Infected
    dRecovered =  kRec*Infected
    return(list(c(dHealthy, dInfected, dRecovered)))
  })}


Time <- c(seq(from=0, to=50, length=50))
State <- c(Healthy=198, Infected=2, Recovered=0)


# CASE 1:   kInf = 0.5, kRec = 0.05 .................................

Pars <- c(kInf=0.5, kRec= 0.05)

Rout <- ode(func = virus.model, y = State, parms = Pars, times = Time)

Rout.df <- as.data.frame(Rout)
head(Rout.df)

library(tidyr)   # .................. gather columns for plotting 
Rout.Long.df <- Rout.df %>% 
  gather(key="Status",value="Number",-time)
head(Rout.Long.df)

library(ggplot2) # .................. plotting
ggplot(Rout.Long.df, aes(x=time, y=Number, color=Status)) +
  geom_line() +
  theme_bw()

ggplot(Rout.Long.df, aes(x=time, y=Number, fill=Status)) +
  geom_area() +
  theme_bw()

Rout.Long.df %>% 
  group_by(Status) %>% 
  summarize(MaxNum = max(Number))

# TO DO:  
# 1. move legend to plot body
# 2. mark point of max # infected on area graph?
# 3. add point of hospital capacity to line graph




# CASE 2:   kInf = 0.10, kRec = 0.05 .................................

Pars <- c(kInf=0.1, kRec= 0.05)

Rout2 <- ode(func = virus.model, y = State, parms = Pars, times = Time)

Rout2.df <- as.data.frame(Rout2)
head(Rout2.df)

library(tidyr)   # .................. gather columns for plotting 
Rout2.Long.df <- Rout2.df %>% 
  gather(key="Status",value="Number",-time)
head(Rout2.Long.df)

library(ggplot2) # .................. plotting
ggplot(Rout2.Long.df, aes(x=time, y=Number, color=Status)) +
  geom_line() +
  theme_bw()

ggplot(Rout2.Long.df, aes(x=time, y=Number, fill=Status)) +
  geom_area(alpha=.2) +
  theme_bw()

Rout2.Long.df %>% 
  group_by(Status) %>% 
  summarize(MaxNum = max(Number))


# citation for desolve
citation("deSolve")

# Karline Soetaert, Thomas Petzoldt, R. Woodrow Setzer (2010). Solving
# Differential Equations in R: Package deSolve. Journal of Statistical
# Software, 33(9), 1--25. URL http://www.jstatsoft.org/v33/i09/ DOI
# 10.18637/jss.v033.i09


