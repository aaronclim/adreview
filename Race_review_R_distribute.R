'''update R'''

install.packages("haven")
library(haven)
install.packages("ggpubr")
library(ggpubr)
install.packages("gridExtra")
library(gridExtra)
han<-read_sav('path/Race_review_dist.sav')
bhan <- read_sav('path/Indirect_studies_dist.sav')
library(ggplot2)

'''note. [Race] is 0-100%. [Race2] is 0-1.0.'''
ggplot(han, aes(x = White, size=TotalN))+geom_histogram();
whiteplot<-ggplot(han, aes(x = White, size=TotalN))+geom_histogram(color="black", fill="white");
whiteplot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
white_mean <- mean(han$White, na.rm=TRUE)
white_median <- median(han$White, na.rm=TRUE)

'''scatterplot x year'''

white_splot<-ggplot(han, aes(x = PublicationYear, y = White, size=TotalN))+labs(x="Publication Year", y="% White")+
xlim(1994, 2022)+geom_point(shape=1)+theme(legend.position="none")
white_splot2<-white_splot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background = element_blank(), axis.line = element_line(colour = "black"))
white_splot2

whiteNH_splot<-ggplot(han, aes(x = PublicationYear, y = White_NH, size=TotalN))+labs(x="Publication Year", y="% Non-Hispanic White")+
  xlim(1995, 2022)+geom_point(shape=1)+theme(legend.position="none");
whiteNH_splot2<-whiteNH_splot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
whiteNH_splot2

black_splot<-ggplot(han, aes(x = PublicationYear, y = BlackAfricanAmerican, size=TotalN))+labs(x="Publication Year", y="% Black/African-American")+
  xlim(1995, 2022)+geom_point(shape=1)+theme(legend.position="none");
black_splot2<-black_splot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
black_splot2

HisLa_splot<-ggplot(han, aes(x = PublicationYear, y = HispanicorLatino, size=TotalN))+labs(x="Publication Year", y="% Hispanic/Latino")+
  xlim(1995, 2022)+geom_point(shape=1);
HisLa_splot2<-HisLa_splot+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                panel.background = element_blank(), axis.line = element_line(colour = "black"))
HisLa_splot2


comb_splot<-grid.arrange(white_splot2, whiteNH_splot2, black_splot2, HisLa_splot2,
             ncol=2, nrow=2,
             layout_matrix=rbind(c(1,2), c(3,4)))

'''combined scatterplot graph with shared legend'''

install.packages("cowplot")
library(cowplot)

prow2<- plot_grid(
  white_splot2+theme(legend.position="none"),
  whiteNH_splot2+theme(legend.position="none"),
  black_splot2+theme(legend.position="none"),
  HisLa_splot2+theme(legend.position="none"),
  align='vh',
  labels=c("A","B","C","D"),
  hjust=-1,
  nrow=2,
  label_x=-.01)
prow2

HisLa_splot2+theme(legend.position="bottom")

legend <- get_legend(
  HisLa_splot2+
    guides(key=guide_legend(nrow=1))+
    theme(legend.position="bottom"))
plot_grid(prow2, legend, ncol=1, rel_heights=c(1,.1))



'''histograms by race/ethnicity'''

'''white'''
install.packages("tidyverse")
library(tidyverse)
df_stats <-
  han %>%
  summarize(
    mean = mean(White, na.rm=TRUE),
    median = median(White, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)


whiteplot<-ggplot(han, aes(x = White, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats, aes(xintercept=value, color=key), show.legend=FALSE)
whiteplot2<- whiteplot + scale_x_continuous(name="% White", limits=c(0,100)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,150)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
whiteplot2

'''Non-Hispanic White'''
df_stats4 <-
  han %>%
  summarize(
    mean = mean(White_NH, na.rm=TRUE),
    median = median(White_NH, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

whiteNHplot<-ggplot(han, aes(x = White_NH, size=TotalN))+geom_histogram(color="black", fill="white")

whiteNHplot<-ggplot(han, aes(x = White_NH, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats4, aes(xintercept=value, color=key), show.legend=FALSE)
whiteNHplot2<- whiteNHplot + scale_x_continuous(name="% Non-Hispanic White", limits=c(0,100)) +  scale_y_continuous(expand = c(0,0), limits=c(0,150)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
whiteNHplot2

'''black'''

library(tidyverse)
df_stats2 <-
  han %>%
  summarize(
    mean = mean(BlackAfricanAmerican, na.rm=TRUE),
    median = median(BlackAfricanAmerican, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)
blackplot<-ggplot(han, aes(x = BlackAfricanAmerican, size=TotalN))+geom_histogram(color="black", fill="white")

blackplot<-ggplot(han, aes(x = BlackAfricanAmerican, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats2, aes(xintercept=value, color=key), show.legend=FALSE)
blackplot2<-blackplot + scale_x_continuous(name="% Black/African-American")+
  scale_y_continuous(expand = c(0,0), limits=c(0,150))+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
blackplot2

grid.arrange(whiteplot, blackplot,
             ncol=2, nrow=1,
             layout_matrix=rbind(c(1,1), c(2,2)))

'''hispanic/latino'''

library(tidyverse)
df_stats3 <-
  han %>%
  summarize(
    mean = mean(HispanicorLatino, na.rm=TRUE),
    median = median(HispanicorLatino, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

HisLaplot<-ggplot(han, aes(x = HispanicorLatino, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats3, aes(xintercept=value, color=key), show.legend=TRUE)
HisLaplot2<-HisLaplot + scale_x_continuous(name="% Hispanic/Latino", limits=c(0,100)) +
scale_y_continuous(expand = c(0,0), limits=c(0,150)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                               panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))

HisLaplot2
'''Asian'''

library(tidyverse)
df_stats5 <-
  han %>%
  summarize(
    mean = mean(Asian, na.rm=TRUE),
    median = median(Asian, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

asianplot<-ggplot(han, aes(x = Asian, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats5, aes(xintercept=value, color=key), show.legend=FALSE)
asianplot2<-asianplot + scale_x_continuous(name="% Asian", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,200)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''American Indian/Alaska Native'''

library(tidyverse)
df_stats6 <-
  han %>%
  summarize(
    mean = mean(AmericanIndianAlaskaNative, na.rm=TRUE),
    median = median(AmericanIndianAlaskaNative, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

AmiAlaplot<-ggplot(han, aes(x = AmericanIndianAlaskaNative, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats6, aes(xintercept=value, color=key), show.legend=FALSE)
AmiAlaplot2<-AmiAlaplot + scale_x_continuous(name="% American Indian/Alaska Native", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,200)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''Native Hawaiian/Other PI'''

library(tidyverse)
df_stats7 <-
  han %>%
  summarize(
    mean = mean(NativeHawaiianOtherPacificIslander, na.rm=TRUE),
    median = median(NativeHawaiianOtherPacificIslander, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

NaPIplot<-ggplot(han, aes(x = NativeHawaiianOtherPacificIslander, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats7, aes(xintercept=value, color=key), show.legend=FALSE)
NaPIplot2<-NaPIplot + scale_x_continuous(name="% Native Hawaiian/Pacific Islander", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,200)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                          panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''Multiracial'''

library(tidyverse)
df_stats8 <-
  han %>%
  summarize(
    mean = mean(Multiracial, na.rm=TRUE),
    median = median(Multiracial, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

MRplot<-ggplot(han, aes(x = Multiracial, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats8, aes(xintercept=value, color=key), show.legend=FALSE)
MRplot2<-MRplot + scale_x_continuous(name="% Multiracial", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0), limits=c(0,200)) +
 theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
grid.arrange(whiteplot2, whiteNHplot2, blackplot2, HisLaplot2,
             ncol=2, nrow=2,
             layout_matrix=rbind(c(1,2), c(3,4)))

'''when attempting to plot all other races, the mean/medians are essentially 0 and make
somewhat skewed graphs. Therefore, may make most sense to just present these 4 above'''

grid.arrange(whiteplot2, whiteNHplot2, blackplot2, HisLaplot2,
             asianplot2, AmiAlaplot2, NaPIplot2, MRplot2,
             ncol=4, nrow=2,
             layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8)))



'''Shared legend'''
'''Combined figure of histogram for race %'''

install.packages("cowplot")
library(cowplot)
HisLaplot<-ggplot(han, aes(x = HispanicorLatino, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats3, aes(xintercept=value, color=key), show.legend=TRUE)

prow<- plot_grid(
  whiteplot2+theme(legend.position="none"),
  whiteNHplot2+theme(legend.position="none"),
  blackplot2+theme(legend.position="none"),
  HisLaplot2+theme(legend.position="none"),
  align='vh',
  labels=c("A","B","C","D"),
  hjust=-1,
  nrow=2)
prow


HisLaplot2+theme(legend.position="bottom")

legend <- get_legend(
  HisLaplot2+
    guides(key=guide_legend(nrow=1))+
    theme(legend.position="bottom"))
plot_grid(prow, legend, ncol=1, rel_heights=c(1,.1))

'''Big study dataset graphs start here'''
'''histograms by race/ethnicity'''

'''white'''
install.packages("tidyverse")
library(tidyverse)
df_stats9 <-
  bhan %>%
  summarize(
    mean = mean(White, na.rm=TRUE),
    median = median(White, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bwhiteplot<-ggplot(bhan, aes(x = White, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats9, aes(xintercept=value, color=key), show.legend=FALSE)
bwhiteplot2<- bwhiteplot + scale_x_continuous(name="% White", limits=c(0,100)) +
  scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
bwhiteplot2

'''Non-Hispanic White'''
df_stats10 <-
  bhan %>%
  summarize(
    mean = mean(White_NH, na.rm=TRUE),
    median = median(White_NH, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bwhiteNHplot<-ggplot(bhan, aes(x = White_NH, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats10, aes(xintercept=value, color=key), show.legend=FALSE)
bwhiteNHplot2<- bwhiteNHplot + scale_x_continuous(name="% Non-Hispanic White", limits=c(0,100)) +  scale_y_continuous(expand = c(0,0)) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
bwhiteNHplot2

'''black'''

library(tidyverse)
df_stats11 <-
  bhan %>%
  summarize(
    mean = mean(BlackAfricanAmerican, na.rm=TRUE),
    median = median(BlackAfricanAmerican, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)
bblackplot<-ggplot(bhan, aes(x = BlackAfricanAmerican, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats11, aes(xintercept=value, color=key), show.legend=FALSE)
bblackplot2<-bblackplot + scale_x_continuous(name="% Black/African-American", limits=c(0,100))+
  scale_y_continuous(expand = c(0,0))+ theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
bblackplot2


'''hispanic/latino'''

library(tidyverse)
df_stats12 <-
  bhan %>%
  summarize(
    mean = mean(HispanicorLatino, na.rm=TRUE),
    median = median(HispanicorLatino, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bHisLaplot<-ggplot(bhan, aes(x = HispanicorLatino, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats12, aes(xintercept=value, color=key), show.legend=TRUE)
bHisLaplot2<-bHisLaplot + scale_x_continuous(name="% Hispanic/Latino", limits=c(0,100)) +
  scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                           panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))

bHisLaplot2

'''Asian'''

library(tidyverse)
df_stats13 <-
  bhan %>%
  summarize(
    mean = mean(Asian, na.rm=TRUE),
    median = median(Asian, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

basianplot<-ggplot(bhan, aes(x = Asian, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats13, aes(xintercept=value, color=key), show.legend=FALSE)
basianplot2<-basianplot + scale_x_continuous(name="% Asian", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''American Indian/Alaska Native'''

library(tidyverse)
df_stats14 <-
  bhan %>%
  summarize(
    mean = mean(AmericanIndianAlaskaNative, na.rm=TRUE),
    median = median(AmericanIndianAlaskaNative, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bAmiAlaplot<-ggplot(bhan, aes(x = AmericanIndianAlaskaNative, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats14, aes(xintercept=value, color=key), show.legend=FALSE)
bAmiAlaplot2<-bAmiAlaplot + scale_x_continuous(name="% American Indian/Alaska Native", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''Native Hawaiian/Other PI'''

library(tidyverse)
df_stats15 <-
  bhan %>%
  summarize(
    mean = mean(NativeHawaiianOtherPacificIslander, na.rm=TRUE),
    median = median(NativeHawaiianOtherPacificIslander, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bNaPIplot<-ggplot(bhan, aes(x = NativeHawaiianOtherPacificIslander, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats15, aes(xintercept=value, color=key), show.legend=FALSE)
bNaPIplot2<-bNaPIplot + scale_x_continuous(name="% Native Hawaiian/Pacific Islander", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0)) + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                                                                            panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))
'''Multiracial'''

library(tidyverse)
df_stats16 <-
  bhan %>%
  summarize(
    mean = mean(Multiracial, na.rm=TRUE),
    median = median(Multiracial, na.rm=TRUE)
  ) %>%
  gather (key=key, value=value, mean:median)

bMRplot<-ggplot(bhan, aes(x = Multiracial, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats16, aes(xintercept=value, color=key), show.legend=FALSE)
bMRplot2<-MRplot + scale_x_continuous(name="% Multiracial", limits=c(0,1.0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")+theme(text=element_text(family="serif")))

grid.arrange(bwhiteplot2, bwhiteNHplot2, bblackplot2, bHisLaplot2,
             ncol=2, nrow=2,
             layout_matrix=rbind(c(1,2), c(3,4)))

'''when attempting to plot all other races, the mean/medians are essentially 0 and make
somewhat skewed graphs. Therefore, may make most sense to just present these 4 above'''

grid.arrange(bwhiteplot2, bwhiteNHplot2, bblackplot2, bHisLaplot2,
             basianplot2, bAmiAlaplot2, bNaPIplot2, bMRplot2,
             ncol=4, nrow=2,
             layout_matrix=rbind(c(1,2,3,4), c(5,6,7,8)))


'''shared legend'''
'''Combined figure of histogram for race %'''

install.packages("cowplot")
library(cowplot)
HisLaplot<-ggplot(han, aes(x = HispanicorLatino, size=TotalN))+geom_histogram(color="black", fill="white")+
  geom_vline(data=df_stats3, aes(xintercept=value, color=key), show.legend=TRUE)

prow<- plot_grid(
  bwhiteplot2+theme(legend.position="none"),
  bwhiteNHplot2+theme(legend.position="none"),
  bblackplot2+theme(legend.position="none"),
  bHisLaplot2+theme(legend.position="none"),
  align='vh',
  labels=c("A","B","C","D"),
  hjust=-1,
  nrow=2)
prow


bHisLaplot2+theme(legend.position="bottom")

legend <- get_legend(
  bHisLaplot2+
    guides(key=guide_legend(nrow=1))+
    theme(legend.position="bottom"))
plot_grid(prow, legend, ncol=1, rel_heights=c(1,.1))

