# Behavior analysis and figures

library(ggplot2)
library(tidyverse)


f0df_long <- read.csv("f0_behavior_data_normalized.csv")
f1df_long <- read.csv("f1_behavior_data_normalized.csv")

# ANALYSIS 
#for(s in 1:length(sal)){
  for(i in 1:length(end)){
    for(t in 1:length(stim)){
      f0df_long$Chemical <- factor(f0df_long$Chemical, levels = c("Control", "Bifenthrin", "Cyfluthrin", "Cyhalothrin") )
      set.seed(123)
      df <- f0df_long %>% filter(Stimuli == stim[t], Salinity == sal[s], Endpoint == end[i], Osmo == sal[s]) %>%
        dunn_test(Response ~ Chemical, p.adjust.method = "bonferroni")
      new_df <- cbind.data.frame(end[i], sal[s], stim[t], osm[s], df)
      # write_csv(new_df, file = "f0_sig_12162023.csv", append = T)
    }
  }
}

#if (file.exists("f0_sig_12162023.csv")) {
#Delete file if it exists
#file.remove("f0_sig_12162023.csv")
}
#df_long


# significant results
sig_f0_behavior <- read.csv("f0_sig_12162023.csv", header = FALSE) %>% filter(V6 == "Control") 
sig_f0_behavior %>% filter(V2 == V4, V6 == "Control", V7 == "Bifenthrin", V1 != "Mm_n", V13 != "ns") %>% dplyr::select(V1, V2, V3, V7, V10, V12, V13)


sig_f1_behavior <- read.csv("f1_sig_12172023.csv", header = FALSE) %>% filter(V5 == "Control") 
sig_f1_behavior %>% filter(V6 == "Bifenthrin", V12 != "ns") %>% dplyr::select(V1, V2, V3, V6, V9, V11, V12)




### FIGURES

f0df_long <- f0df_long %>% filter(Salinity == Osmo, Chemical != "")
f0df_long$Stimuli <- factor(f0df_long$Stimuli, levels = c("Dark1", "Light1", "Dark2"))
f0df_long$Chemical <- factor(f0df_long$Chemical, levels = c("Control", "Bifenthrin", "Cyfluthrin", "Cyhalothrin"))
f0_v <- f0df_long %>% filter(Endpoint == "DM_n") %>% group_by(Stimuli, Trial.....1, Well, Salinity, Chemical) %>% summarise(avg = mean(tdm))
f0_v$Chemical2 <- f0_v$Chemical



ggplot(na.omit(f0_v), aes(Stimuli, avg, fill = Chemical)) +
  geom_point(aes(fill = Chemical2),colour="black",pch=21, size=1.5, position = position_dodge(width = 0.5), alpha = 0.6) + 
  stat_summary(geom = "pointrange", size = 0.2, fun = mean, position = position_dodge(width = 0.5), show.legend = F) +
  stat_summary(fun.data=mean_se, position=position_dodge(width=0.5),geom="linerange", show.legend = F)+
  facet_wrap("Salinity") + 
  scale_fill_manual(values = c("goldenrod3", "royalblue1", "slateblue1", "springgreen4")) +
  labs(x="Stimuli",
       y="F0 TDM (mm)",
       title = "", 
       fill="Chemical") +  
  guides(colr = FALSE)+
  theme_classic() +
  theme(axis.text.x= element_text(size=11, color = "black"), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x= element_text(size=11,  color = "black"), plot.title = element_text(hjust = 0.5), axis.text.y= element_text(size=rel(1.2), color = "black"), title = element_text(size = 12), strip.text = element_text(size = 10, colour = "black"))+
  theme(panel.background = element_rect(fill=NA, color=NA),
        plot.background = element_rect(fill=NA, color=NA))


ggsave("f0_tdm_points_01042024.png", width = 6, height = 3)



f1df_long <- f1df_long %>% filter(Salinity == Osmo, Chemical != "")
f1df_long$Stimuli <- factor(f1df_long$Stimuli, levels = c("Dark1", "Light1", "Dark2"))
f1df_long$Chemical <- factor(f1df_long$Chemical, levels = c("Control", "Bifenthrin", "Cyfluthrin", "Cyhalothrin"))
f1_v <- f1df_long %>% filter(Endpoint == "DM_n") %>% group_by(Stimuli, `Trial.....1`, Well, Salinity, Chemical) %>% summarise(avg = mean(tdm))
f1_v$Chemical2 <- f1_v$Chemical



ggplot(na.omit(f1_v), aes(Stimuli, avg, fill = Chemical)) +
  geom_point(aes(fill = Chemical2),colour="black",pch=21, size=1.5, position = position_dodge(width = 0.5), alpha = 0.6) + 
  stat_summary(geom = "pointrange", size = 0.2, fun = mean, position = position_dodge(width = 0.5), show.legend = F) +
  stat_summary(fun.data=mean_se, position=position_dodge(width=0.5),geom="linerange", show.legend = F)+
  facet_wrap("Salinity") + 
  scale_fill_manual(values = c("goldenrod3", "royalblue1", "slateblue1", "springgreen4")) +
  labs(x="Stimuli",
       y="F1 TDM (mm)",
       title = "", 
       fill="Chemical") +  
  guides(colr = FALSE)+
  theme_classic() +
  theme(axis.text.x= element_text(size=11, color = "black"), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x= element_text(size=11,  color = "black"), plot.title = element_text(hjust = 0.5), axis.text.y= element_text(size=rel(1.2), color = "black"), title = element_text(size = 12), strip.text = element_text(size = 10, colour = "black"))+
  theme(panel.background = element_rect(fill=NA, color=NA),
        plot.background = element_rect(fill=NA, color=NA))


ggsave("f1_tdm_points_01042024.png", width = 6, height = 3)




# HEAT MAPS


chem <- unique(f0df_long$Chemical)
sal <- unique(f0df_long$Salinity)
stim <- unique(f0df_long$Stimuli)
end <- unique(f0df_long$Endpoint)

if (file.exists("f0_zbin.csv")) {
  #Delete file if it exists
  file.remove("f0_zbin.csv")
}


for(s in 1:length(sal)){
  for(t in 1:length(stim)){
    for (e in 1:length(end)){
      df <- f0df_long %>% filter(Endpoint == end[e], Salinity == sal[s], Osmo == Salinity,  Stimuli == stim[t])
      dfz <- (df$Response - mean(df$Response, na.rm = T)) / sd(df$Response, na.rm = T)
      z_df <- cbind.data.frame(stim[t], df$Chemical, sal[s], dfz, end[e])
      write_csv(z_df, file = "f0_zbin.csv", append = T)
    }
  }
  
}

#df <- f0df_long %>% filter(Endpoint == "DM_n", Chemical == "Bifenthrin", Salinity == 6, Osmo == Salinity,  Stimuli == "Dark1")
#dfz <- (df$Response - mean(df$Response, na.rm = T)) / sd(df$Response, na.rm = T)


zn <- read.csv("f0_zbin.csv", header = F, col.names = c("Stimuli", "Chemical",  "Salinity", "Z_Score", "Endpoint"))

#filter(z_b_n, Concentration == 0)

if (file.exists("zmean.csv")) {
  #Delete file if it exists
  file.remove("zmean.csv")
}

for(s in 1:length(sal)){
  for(t in 1:length(stim)) {
    for(e in 1:length(end)){
      for(c in 1:length(chem)) {
        dfzm <- zn %>% filter(Chemical == chem[c], Endpoint == end[e], Salinity == sal[s], Stimuli == stim[t]) %>% pull(Z_Score) %>% na.omit() %>% mean
        z_mean_df <- cbind.data.frame(chem[c], end[e], sal[s], stim[t], dfzm)
        write_csv(z_mean_df, file = "zmean.csv", append = T)
      }
    }
  }
}


z_n_mean <- read.csv("zmean.csv", header = F, col.names = c("Chemical", "Endpoint", "Salinity", "Stimuli", "Z_Score"))

if (file.exists("zmean_norm.csv")) {
  #Delete file if it exists
  file.remove("zmean_norm.csv")
}

for(t in 1:length(stim)){
  for(s in 1:length(sal)){
    for(c in 1:length(chem)) {
      for(e in 1:length(end)){
        dfz_n_mean <- z_n_mean %>% filter(Chemical == chem[c], Salinity == sal[s], Stimuli == stim[t] , Endpoint == end[e])
        control <- z_n_mean %>% filter(Endpoint == end[e], Salinity == sal[s], Stimuli == stim[t], Chemical == "Control")
        dfz_n_mean <- dfz_n_mean$Z_Score - control$Z_Score
        z_mean_df <- cbind.data.frame(chem[c], end[e], sal[s], stim[t], dfz_n_mean)
        write_csv(z_mean_df, file = "zmean_norm.csv", append = T)
      }
    }
  }
}

z_n_mean_norm <- read.csv("zmean_norm.csv", header = F, col.names = c("Chemical", "Endpoint", "Salinity", "Stimuli", "Z_Score"))


z_n_mean_norm$Chemical <- factor(z_n_mean_norm$Chemical, levels = c("Control", "Bifenthrin", "Cyfluthrin", "Cyhalothrin"))
z_n_mean_norm$Stimuli <- factor(z_n_mean_norm$Stimuli, levels = c("Dark1", "Light1", "Dark2"))

z_n_mean_norm$Endpoint <- gsub("DM_n", "TDM", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("V_n", "Velocity", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("F_n", "Freezing", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("B_n", "Bursting", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("C_n", "Cruising", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("Thig_n", "Thigmotaxis", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("Mm_n", "Meander", z_n_mean_norm$Endpoint)
z_n_mean_norm$Salinity <- gsub("6", "6 PSU", z_n_mean_norm$Salinity)
z_n_mean_norm$Salinity <- gsub("10", "10 PSU", z_n_mean_norm$Salinity)

z_n_mean_norm$Salinity <- factor(z_n_mean_norm$Salinity, levels = c("6 PSU", "10 PSU"))

ggplot(na.omit(z_n_mean_norm) %>% dplyr::filter(Chemical != "Control", Endpoint != "Meander"), aes(Chemical, Stimuli, fill = Z_Score)) +
  geom_tile(colour = "gray") + 
  facet_grid(Salinity~Endpoint) + 
  labs(x="Chemical",
       y="F0 behavioral response",
       title = "", 
       fill="Z-Score") +  
  theme_classic() +
  scale_fill_gradient2(low = "mediumorchid4", mid = "white", high = "gold", midpoint = 0) +
  theme(axis.text.x= element_text(size=rel(1.2), angle=90, color = "black"), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x= element_text(size=rel(1.2), angle=90, color = "black"), plot.title = element_text(hjust = 0.5), axis.text.y= element_text(size=rel(1.2), color = "black"), title = element_text(size = 14), strip.text = element_text(size = 10, colour = "black"))


#ggsave("f0_behavior_heatmap_01012024.png", width = 8, height = 5)




chem <- unique(f1df_long$Chemical)
sal <- unique(f1df_long$Salinity)
stim <- unique(f1df_long$Stimuli)
end <- unique(f1df_long$Endpoint)

if (file.exists("f1_zbin.csv")) {
  #Delete file if it exists
  file.remove("f1_zbin.csv")
}


for(s in 1:length(sal)){
  for(t in 1:length(stim)){
    for (e in 1:length(end)){
      df <- f1df_long %>% filter(Endpoint == end[e], Salinity == sal[s], Osmo == Salinity,  Stimuli == stim[t])
      dfz <- (df$Response - mean(df$Response, na.rm = T)) / sd(df$Response, na.rm = T)
      z_df <- cbind.data.frame(stim[t], df$Chemical, sal[s], dfz, end[e])
      write_csv(z_df, file = "f1_zbin.csv", append = T)
    }
  }
  
}

#df <- f0df_long %>% filter(Endpoint == "DM_n", Chemical == "Bifenthrin", Salinity == 6, Osmo == Salinity,  Stimuli == "Dark1")
#dfz <- (df$Response - mean(df$Response, na.rm = T)) / sd(df$Response, na.rm = T)


zn <- read.csv("f1_zbin.csv", header = F, col.names = c("Stimuli", "Chemical",  "Salinity", "Z_Score", "Endpoint"))

#filter(z_b_n, Concentration == 0)

if (file.exists("zmean.csv")) {
  #Delete file if it exists
  file.remove("zmean.csv")
}

for(s in 1:length(sal)){
  for(t in 1:length(stim)) {
    for(e in 1:length(end)){
      for(c in 1:length(chem)) {
        dfzm <- zn %>% filter(Chemical == chem[c], Endpoint == end[e], Salinity == sal[s], Stimuli == stim[t]) %>% pull(Z_Score) %>% na.omit() %>% mean
        z_mean_df <- cbind.data.frame(chem[c], end[e], sal[s], stim[t], dfzm)
        write_csv(z_mean_df, file = "zmean.csv", append = T)
      }
    }
  }
}


z_n_mean <- read.csv("zmean.csv", header = F, col.names = c("Chemical", "Endpoint", "Salinity", "Stimuli", "Z_Score"))

if (file.exists("zmean_norm.csv")) {
  #Delete file if it exists
  file.remove("zmean_norm.csv")
}

for(t in 1:length(stim)){
  for(s in 1:length(sal)){
    for(c in 1:length(chem)) {
      for(e in 1:length(end)){
        dfz_n_mean <- z_n_mean %>% filter(Chemical == chem[c], Salinity == sal[s], Stimuli == stim[t] , Endpoint == end[e])
        control <- z_n_mean %>% filter(Endpoint == end[e], Salinity == sal[s], Stimuli == stim[t], Chemical == "Control")
        dfz_n_mean <- dfz_n_mean$Z_Score - control$Z_Score
        z_mean_df <- cbind.data.frame(chem[c], end[e], sal[s], stim[t], dfz_n_mean)
        write_csv(z_mean_df, file = "zmean_norm.csv", append = T)
      }
    }
  }
}

z_n_mean_norm <- read.csv("zmean_norm.csv", header = F, col.names = c("Chemical", "Endpoint", "Salinity", "Stimuli", "Z_Score"))


z_n_mean_norm$Chemical <- factor(z_n_mean_norm$Chemical, levels = c("Control", "Bifenthrin", "Cyfluthrin", "Cyhalothrin"))
z_n_mean_norm$Stimuli <- factor(z_n_mean_norm$Stimuli, levels = c("Dark1", "Light1", "Dark2"))

z_n_mean_norm$Endpoint <- gsub("DM_n", "TDM", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("V_n", "Velocity", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("F_n", "Freezing", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("B_n", "Bursting", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("C_n", "Cruising", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("Thig_n", "Thigmotaxis", z_n_mean_norm$Endpoint)
z_n_mean_norm$Endpoint <- gsub("Mm_n", "Meander", z_n_mean_norm$Endpoint)
z_n_mean_norm$Salinity <- gsub("6", "6 PSU", z_n_mean_norm$Salinity)
z_n_mean_norm$Salinity <- gsub("10", "10 PSU", z_n_mean_norm$Salinity)

z_n_mean_norm$Salinity <- factor(z_n_mean_norm$Salinity, levels = c("6 PSU", "10 PSU"))

ggplot(na.omit(z_n_mean_norm) %>% dplyr::filter(Chemical != "Control", Endpoint != "Meander"), aes(Chemical, Stimuli, fill = Z_Score)) +
  geom_tile(colour = "gray") + 
  facet_grid(Salinity~Endpoint) + 
  labs(x="Chemical",
       y="F1 behavioral response",
       title = "", 
       fill="Z-Score") +  
  theme_classic() +
  scale_fill_gradient2(low = "mediumorchid4", mid = "white", high = "gold", midpoint = 0) +
  theme(axis.text.x= element_text(size=rel(1.2), angle=90, color = "black"), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x= element_text(size=rel(1.2), angle=90, color = "black"), plot.title = element_text(hjust = 0.5), axis.text.y= element_text(size=rel(1.2), color = "black"), title = element_text(size = 14), strip.text = element_text(size = 10, colour = "black"))

ggsave("f1_behavior_heatmap_01012024.png", width = 8, height = 5)




## BAR GRAPH


x <- f0df_long %>% filter(Salinity == Osmo, Endpoint == "DM_n") %>% dplyr::select(Time, Chemical, Salinity, tdm, Trial.....1, Well, Stimuli) %>% mutate(Gen = "0") 

#x$tdm_n <- (f0df$tdm - summary(f0df$tdm)["Min."] )/ ( summary(f0df$tdm)["Max."] - summary(f0df$tdm)["Min."] )


y <- f1df_long %>% filter(Salinity == Osmo, Endpoint == "DM_n") %>% dplyr::select(Time, Chemical, Salinity, tdm, `Trial.....1`, Well, Stimuli) %>% mutate(Gen = "1")
#filter(Salinity == 6, Osmo == 6, Chemical == "Control", Stimuli == "Dark1") %>% pull(tdm_n)
df1 <- rbind(x, y)

df1$Stimuli <- factor(df1$Stimuli, levels = c("Dark1", "Light1", "Dark2"))


df1$Salinity <- gsub("6", "6 PSU", df1$Salinity)
df1$Salinity <- gsub("10", "10 PSU", df1$Salinity)
df1$Gen <- gsub("0", "F0", df1$Gen)
df1$Gen <- gsub("1", "F1", df1$Gen)

df1$Salinity <- factor(df1$Salinity, levels = c("6 PSU", "10 PSU"))

tn <- df1 %>% filter(Chemical == "Control") %>% group_by(Time) %>% summarise(tn = mean(tdm))

df1 <- left_join(df1, tn)
df1$tdm_n <- df1$tdm -  df1$tn


#df.1.2 %>% dplyr::select(Chemical)

df.1.2 <- df1 %>% group_by(Salinity, Chemical, Gen, Stimuli) %>% summarise(avg = mean(tdm))
df.1.3 <- df.1.2 %>% filter(Chemical == "Control") %>% rename(avg_c= avg)


#df.1.3 <- df1 %>% filter(Gen == "1") %>% group_by(Salinity, Chemical, Stimuli) %>% summarise(avg1 = mean(tdm))

df.1.4 <- left_join(df.1.2, df.1.3[,c(1,3:5)])
df.1.4$d_avg <- df.1.4$avg - df.1.4$avg_c

ggplot((df.1.4 %>% filter(Chemical != "Control")), aes(Stimuli, d_avg, fill = Chemical)) +
  #geom_point(aes(fill = Chemical),colour="black",pch=21, size=1.5, position = position_dodge(width = 0.5), alpha = 0.6) +
  stat_summary(geom = "bar",  linewidth = .4, color = "black", fun = mean, position = position_dodge(width = 0.7),  width = 0.5) +
  #stat_summary(fun.data=mean_se, position=position_dodge(width=0.5),geom="linerange", show.legend = F)+
  facet_grid(Salinity ~ Gen) + 
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4)+
  scale_fill_manual(values = c( "royalblue1", "slateblue1", "springgreen4")) +
  labs(x="Stimuli",
       y="Average change in TDM (mm) \nbetween control and pyrethroid",
       title = "", 
       fill="Chemical") +  
  guides(colr = FALSE)+
  theme_classic() +
  theme(axis.text.x= element_text(size=11, color = "black"), plot.title = element_text(hjust = 0.5)) +
  theme(axis.text.x= element_text(size=11,  color = "black"), plot.title = element_text(hjust = 0.5), axis.text.y= element_text(size=rel(1.2), color = "black"), title = element_text(size = 12), strip.text = element_text(size = 10, colour = "black"))


#ggsave("avg_change_tdm_01012024.png", height = 3, width = 6)

