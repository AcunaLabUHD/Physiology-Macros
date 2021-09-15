install.packages("tidyverse")
install.packages("RColorBrewer")
library(tidyverse)
library(RColorBrewer)

dir()
setwd("")
temp = list.files(pattern="*.txt")
myfiles = lapply(temp, read_table2)
data <- do.call(rbind.data.frame, myfiles)

write_excel_csv2(data, "")

data2 <- read.delim("")
data3 <- data2 %>% filter( Total_events > 0 & !is.na(Amplitude))
data3$Amplitude <- as.numeric(as.character(data3$Amplitude)) 
data4 <- data3 %>% filter(X.Fall_time._.s. > 5 & X.Fall_time._.s. < 30)


#responding cells proportions per Condition 

responding_cells <-  data2 %>%  mutate(active = (Total_events > 0 ))
 
active_cells_plot <- responding_cells %>% ggplot() +
  geom_bar(position = "fill", aes(x = Genotype, fill= active )) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")

#events

events_plot <- data4 %>% group_by(Genotype) %>% 
  summarise(
    n = n(),
    events = mean(Total_events),
    sem_events = sd(Total_events)/sqrt(n),
    amp = mean(Amplitude),
    sem_amp = sd(Amplitude)/sqrt(n)
    ) %>%  
  ggplot(.,aes(x = Genotype, y = events)) +
  geom_col(width = 1,aes(x = Genotype, y = events, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = events - sem_events, ymax = events + sem_events)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genotype, y = Total_events, alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")

#amplitude 
amp_plot <- data4 %>% group_by(Genotype) %>% 
  summarise(
    n = n(),
    events = mean(Total_events),
    sem_events = sd(Total_events)/sqrt(n),
    amp = mean(Amplitude),
    sem_amp = sd(Amplitude)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genotype, y = amp)) +
  geom_col(width = 1,aes(x = Genotype, y = amp, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = amp - sem_amp, ymax = amp + sem_amp)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genotype, y = Amplitude, alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")


#rise time
rise_t_plot <- data4 %>% group_by(Genotype) %>% 
  summarise(
    n = n(),
    rise_t = mean(X.Rise_time._.s.),
    sem_rise_t = sd(X.Rise_time._.s.)/sqrt(n),
    fall_t = mean(X.Fall_time._.s.),
    sem_fall_t = sd(X.Fall_time._.s.)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genotype, y = rise_t)) +
  geom_col(width = 1,aes(x = Genotype, y = rise_t, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = rise_t - sem_rise_t, ymax = rise_t + sem_rise_t)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genotype, y = X.Rise_time._.s., alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15)+
  scale_fill_brewer(palette="Paired")

#fall time

fall_t_plot <- data4 %>% group_by(Genotype) %>% 
  summarise(
    n = n(),
    rise_t = mean(X.Rise_time._.s.),
    sem_rise_t = sd(X.Rise_time._.s.)/sqrt(n),
    fall_t = mean(X.Fall_time._.s.),
    sem_fall_t = sd(X.Fall_time._.s.)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genotype, y = fall_t)) +
  geom_col(width = 1,aes(x = Genotype, y = fall_t, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = fall_t - sem_fall_t, ymax = fall_t + sem_fall_t)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genotype, y = X.Fall_time._.s., alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15)+
  scale_fill_brewer(palette="Paired")


#events cum freq plot

events_cum_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = Total_events , colour = Genotype)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Total event number")

#amplitude cum freq plot

amp_cum_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = Amplitude , colour = Genotype)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Event Amplitud (f/f0)")

#rise cum freq plot

rise_t_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = X.Rise_time._.s. , colour = Genotype)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Rise time (s)")
#fall cum freq plot

fall_t_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = X.Fall_time._.s. , colour = Genotype)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Fall time (s)")


#global sync index
dir()
data3 <- read.delim2("Summary Syncronyzation index.txt")

syn_index_plot <- data3 %>% group_by(Genotype) %>%
  summarise(
    n = n(),
    syn_index = mean(as.numeric(Syncronization_index)),
    sem_syn_index = sd(Syncronization_index)/sqrt(n),
  ) %>%  
  ggplot(.,aes(x = Genotype, y = syn_index)) +
  geom_col(width = 1, colour = "black",aes(x = Genotype, y = syn_index, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = syn_index - sem_syn_index, ymax = syn_index + sem_syn_index)) +
  theme_classic(base_size = 15) +
  ggtitle("Syn Index") +
  ylab("Sync index ") +
  xlab("Genotype")+
  scale_fill_brewer(palette="Paired")

#export plots 

events_plot <- events_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Total events (#)") + theme(legend.position = "none")

amp_plot <- amp_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Event Amplitude (F/F0)") + theme(legend.position = "none")

active_cells_plot <- active_cells_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("% of Cells")

rise_t_plot <- rise_t_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Rise Time (s)") + theme(legend.position = "none")

fall_t_plot <-  fall_t_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Fall Time (s)") + theme(legend.position = "none")



ggsave("Events.pdf",events_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Amplitud.pdf",amp_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Syn_index.pdf",syn_index_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Active_cells_plot.pdf",active_cells_plot,
       units="in", width=4, height=4, dpi=300)

ggsave("rise_t_plot.pdf",rise_t_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("fall_t_plot.pdf",fall_t_plot,
       units="in", width=2, height=4, dpi=300)



#export cum plots

events_cum_freq_plot <- events_cum_freq_plot + labs(title =NULL) + 
  xlab("# Total events") + ylab("Cum. fraction") + theme(legend.position = "None")

amp_cum_freq_plot <- amp_cum_freq_plot + labs(title =NULL) + 
  xlab("Mean Event Amplitude (F/F0)") + ylab("Cum. fraction") + theme(legend.position = "None")

rise_t_freq_plot <- rise_t_freq_plot + labs(title =NULL) + 
  xlab("Mean Rise Time (s)") + ylab("Cum. fraction") + theme(legend.position = "None")

fall_t_freq_plot <- fall_t_freq_plot + labs(title =NULL) + 
  xlab("Mean FAll Time (s)") + ylab("Cum. fraction") + theme(legend.position = "None")



ggsave("Cum_Events.pdf",events_cum_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Amplitud.pdf",amp_cum_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Rise_T.pdf",rise_t_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Fall_T.pdf",fall_t_freq_plot,
       units="in", width=5, height=5, dpi=300)


#Grouped graphs Add column Genot_group--------------------------------------------------------------------------------------
WT <- c("2H", "11H")
HET <- c("7B","4D ")

data4 <- data4 %>%  mutate(Genot_group = case_when(
  Genotype %in% WT ~ ("a_WT"),
  Genotype %in% HET ~ ("b_HET")
  ))

data2 <- data2 %>%  mutate(Genot_group = case_when(
  Genotype %in% WT ~ ("a_WT"),
  Genotype %in% HET ~ ("b_HET")
))

active_cells_plot <- responding_cells %>% ggplot() +
  geom_bar(position = "fill", aes(x = Genot_group, fill= active )) + 
  scale_y_continuous(labels = scales::percent) +
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")

#events

events_plot <- data4 %>% group_by(Genot_group) %>% 
  summarise(
    n = n(),
    events = mean(Total_events),
    sem_events = sd(Total_events)/sqrt(n),
    amp = mean(Amplitude),
    sem_amp = sd(Amplitude)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genot_group, y = events)) +
  geom_col(width = 1,aes(x = Genot_group, y = events, fill = Genot_group)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genot_group, ymin = events - sem_events, ymax = events + sem_events)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genot_group, y = Total_events, alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")

#amplitude 
amp_plot <- data4 %>% group_by(Genot_group) %>% 
  summarise(
    n = n(),
    events = mean(Total_events),
    sem_events = sd(Total_events)/sqrt(n),
    amp = mean(Amplitude),
    sem_amp = sd(Amplitude)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genot_group, y = amp)) +
  geom_col(width = 1,aes(x = Genot_group, y = amp, fill = Genot_group)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genot_group, ymin = amp - sem_amp, ymax = amp + sem_amp)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genot_group, y = Amplitude, alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15) +
  scale_fill_brewer(palette="Paired")


#rise time
rise_t_plot <- data4 %>% group_by(Genot_group) %>% 
  summarise(
    n = n(),
    rise_t = mean(X.Rise_time._.s.),
    sem_rise_t = sd(X.Rise_time._.s.)/sqrt(n),
    fall_t = mean(X.Fall_time._.s.),
    sem_fall_t = sd(X.Fall_time._.s.)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genot_group, y = rise_t)) +
  geom_col(width = 1,aes(x = Genot_group, y = rise_t, fill = Genot_group)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genot_group, ymin = rise_t - sem_rise_t, ymax = rise_t + sem_rise_t)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genot_group, y = X.Rise_time._.s., alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15)+
  scale_fill_brewer(palette="Paired")

#fall time

fall_t_plot <- data4 %>% group_by(Genot_group) %>% 
  summarise(
    n = n(),
    rise_t = mean(X.Rise_time._.s.),
    sem_rise_t = sd(X.Rise_time._.s.)/sqrt(n),
    fall_t = mean(X.Fall_time._.s.),
    sem_fall_t = sd(X.Fall_time._.s.)/sqrt(n)
  ) %>%  
  ggplot(.,aes(x = Genot_group, y = fall_t)) +
  geom_col(width = 1,aes(x = Genot_group, y = fall_t, fill = Genot_group)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genot_group, ymin = fall_t - sem_fall_t, ymax = fall_t + sem_fall_t)) +
  geom_jitter(data = data4, width = 0.2, height = 0.0, aes(x = Genot_group, y = X.Fall_time._.s., alpha = 0.2, color = Cover ))+
  theme_classic(base_size = 15)+
  scale_fill_brewer(palette="Paired")


#events cum freq plot

events_cum_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = Total_events , colour = Genot_group)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Total event number")

#amplitude cum freq plot

amp_cum_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = Amplitude , colour = Genot_group)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Event Amplitud (f/f0)")

#rise cum freq plot

rise_t_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = X.Rise_time._.s. , colour = Genot_group)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Rise time (s)")
#fall cum freq plot

fall_t_freq_plot <- 
  ggplot() +
  stat_ecdf(data = data4, geom = "step", size = 2 , aes(x = X.Fall_time._.s. , colour = Genot_group)) +
  theme_classic(base_size = 20) +
  scale_colour_brewer(palette="Paired") + 
  ylab("Freq") +
  xlab("Fall time (s)")


#global sync index
dir()
data3 <- read.delim2("Summary Syncronyzation index.txt")

syn_index_plot <- data3 %>% group_by(Genotype) %>%
  summarise(
    n = n(),
    syn_index = mean(as.numeric(Syncronization_index)),
    sem_syn_index = sd(Syncronization_index)/sqrt(n),
  ) %>%  
  ggplot(.,aes(x = Genotype, y = syn_index)) +
  geom_col(width = 1, colour = "black",aes(x = Genotype, y = syn_index, fill = Genotype)) + 
  geom_errorbar(width = 0.0, size = 1,aes(x = Genotype, ymin = syn_index - sem_syn_index, ymax = syn_index + sem_syn_index)) +
  theme_classic(base_size = 15) +
  ggtitle("Syn Index") +
  ylab("Sync index ") +
  xlab("Genotype")+
  scale_fill_brewer(palette="Paired")

#export plots 

events_plot <- events_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Total events (#)") + theme(legend.position = "none")

amp_plot <- amp_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Event Amplitude (F/F0)") + theme(legend.position = "none")

active_cells_plot <- active_cells_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("% of Cells")

rise_t_plot <- rise_t_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Rise Time (s)") + theme(legend.position = "none")

fall_t_plot <-  fall_t_plot + labs(title =NULL) + 
  xlab("Genotype") + ylab("Mean Fall Time (s)") + theme(legend.position = "none")



ggsave("Events_grouped.pdf",events_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Amplitud_grouped.pdf",amp_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Syn_index_grouped.pdf",syn_index_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("Active_cells_plot_grouped.pdf",active_cells_plot,
       units="in", width=4, height=4, dpi=300)

ggsave("rise_t_plot_grouped.pdf",rise_t_plot,
       units="in", width=2, height=4, dpi=300)

ggsave("fall_t_plot_grouped.pdf",fall_t_plot,
       units="in", width=2, height=4, dpi=300)



#export cum plots

events_cum_freq_plot <- events_cum_freq_plot + labs(title =NULL) + 
  xlab("# Total events") + ylab("Cum. fraction") + theme(legend.position = "None")

amp_cum_freq_plot <- amp_cum_freq_plot + labs(title =NULL) + 
  xlab("Mean Event Amplitude (F/F0)") + ylab("Cum. fraction") + theme(legend.position = "None")

rise_t_freq_plot <- rise_t_freq_plot + labs(title =NULL) + 
  xlab("Mean Rise Time (s)") + ylab("Cum. fraction") + theme(legend.position = "None")

fall_t_freq_plot <- fall_t_freq_plot + labs(title =NULL) + 
  xlab("Mean FAll Time (s)") + ylab("Cum. fraction") + theme(legend.position = "None")



ggsave("Cum_Events_grouped.pdf",events_cum_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Amplitud_grouped.pdf",amp_cum_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Rise_T_grouped.pdf",rise_t_freq_plot,
       units="in", width=5, height=5, dpi=300)

ggsave("Cum_Fall_T_grouped.pdf",fall_t_freq_plot,
       units="in", width=5, height=5, dpi=300)

