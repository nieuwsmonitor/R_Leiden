library(psych)
library(ggthemes)
library(tidyverse)
library(viridis)
library(sjPlot)
library(ggcorrplot)

library(ggh4x)

d = read_csv("data/nienke.csv")|>as_tibble()


d2 = d|>
  rename(type_respondent = Q5_combined,
         experienced = Q6,
         lelijk_mooi = Q21_2,
         zwak_sterk = Q22_2,
         waardeloos_waardevol = Q23_2,
         gewoon_uniek = Q24_2,
         nutteloos_nuttig = Q25_2,
         oninteressant_interessant = Q26_2,
         onnatuurlijk_natuurlijk=Q27_2,
         onbelangrijk_belangrijk=Q28_2)|>
  pivot_longer(lelijk_mooi:onbelangrijk_belangrijk, names_to = 'opinion', values_to = 'score')


d2|>
  group_by(opinion)|>
  summarize(m = mean(score),
            sd = sd(score))|>
  ggplot(aes(x=opinion, y=m, fill=opinion)) + 
  geom_bar(stat="identity", color="black", 
           position=position_dodge()) +
  geom_errorbar(aes(ymin=m-sd, ymax=m+sd), width=.2,
                position=position_dodge(.9))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=10))+
  theme(legend.position = 'none')

  


labels1 <- c("Weak", "Worthless","Not Natural", "Not Interesting","Unimportant","Useless","Ugly","Ordinary")
labels2 <- c("Strong", "Valuable","Natural","Interessing","Important","Useful","Beautiful","Unique")


d2 %>%
  ggplot( aes(x=opinion, y=score, fill=opinion)) +
  geom_boxplot() +
  coord_flip()+
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("I think pavement plants are...") +
  xlab("")+
  ylab("Attitude score (0-100)")+
  scale_x_discrete(labels = labels1)+
  scale_fill_brewer(palette="Spectral", breaks=rev, guide=guide_legend(reverse=FALSE), name=element_blank())+
  guides(y.sec = guide_axis_manual(labels = labels2)) 


