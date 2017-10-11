# Plotting Stream Conservation Unit (SCU) Biodiversity Ranks

# Includes code for generating:
# 1. Scatter plots of SCU B-ranks, with seperate graphs for B-ranks 1-5 and 1-2.
# 2. Heatmap (alone) and heatmap with histograms along axes
# 3. Barplots for B-ranks (alone) and across scoring types and forest-wetland / impervious surface.

# Created By: David Bucklin
# Last updated: 2017-10-11


# Load libraries
library(ggplot2)
# library(readxl)         # for reading excel worksheets
# library(RColorBrewer)   # can be used to generate color ramps; note that it is loaded in section for heatmaps

# load data
setwd("..../r-plot/scu")
# d <- read_xls("SCUs.xls")
d <- read.csv("SCUs.csv")
# lowercase names
names(d) <- tolower(names(d))

# instead of loading data, generate a fake dataset
reps <- 1000  # number of records to generate
d <- data.frame(objectid = 1:reps, site_name = paste("site",1:reps,sep=""), biodiv_sig = paste("B", sample(1:5,size = reps, replace = TRUE), sep=""),
                lngid = 1:reps, forwet_mean = rnorm(reps,0.5, sd = 0.15), impsur_mean = rnorm(reps,0.15, sd = 0.05), vuln_mean = seq(0.01, 1, 0.01),
                score_2b = rnorm(reps, 0.6, 0.15), score_eqwts = rnorm(reps, 0.6, 0.1))

# SCU biodiversity score factor levels
bscores <- c("B1: Outstanding significance","B2: Very high significance",
             "B3: High significance","B4: Moderate significance","B5: General significance")
d$biodiv_sig <- factor(d$biodiv_sig, levels = c("B1","B2","B3","B4","B5"),
       labels = bscores)

# automated blue->red points, blue-yellow-red bkgd
color.fn<- colorRampPalette(c("blue1","firebrick"))
color.pts<- color.fn(length(unique(d$biodiv_sig)))

# background colors and alpha
color.bkd <- c("grey90","grey75","grey60")
alpha <- 0.5

# list points to label (random sample of 10 + 5 B1's)
label.pts <- c(sample(d$lngid, 10, replace = FALSE), sample(d$lngid[d$biodiv_sig == "B1: Outstanding significance"], 5, replace = FALSE))
# manual list for labeling
# label.pts <- c(3659, 3337, 220, 58, 15, 395)

# subset columns
# d <- d[c("objectid","site_name","biodiv_sig","forwet_mean","impsur_mean", "lngid")]
# labels
d$lab <- ifelse(d$lngid %in% label.pts, as.character(d$lngid), NA)

# leader lines
dend <- d[!is.na(d$lab),]
dend$forwet_mean <- dend$forwet_mean + 0.005
dend$impsur_mean <- dend$impsur_mean + 0.01
dl <- rbind(d[!is.na(d$lab),], dend)

# look at data
plot(d$forwet_mean, d$impsur_mean, col = factor(d$biodiv_sig))

# create background color polygons
rects <- data.frame(
  group = c("sensitive","impacted","non-supporting"), 
  x1 = c(0,0,0), x2 = c(1,1,1), y1 = c(0,0.1,0.25), y2 = c(0.1,0.25,0.35))
rects$group <- factor(rects$group, levels = c("sensitive","impacted","non-supporting"), ordered = TRUE)

#####
# Scatter plot with all B-ranks
#####

p <- ggplot(data = d) + 
  geom_rect(data = rects, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = factor(group)), alpha = alpha, show.legend = FALSE) +
  scale_fill_manual(values = color.bkd) + 
  geom_text(data = rects, aes(x=x1+0.02, y=y2-0.01, label=group, hjust = 0), size=4, fontface = "italic") +
  geom_point(aes(forwet_mean, impsur_mean, shape = factor(biodiv_sig), color = factor(biodiv_sig), size = factor(biodiv_sig)), alpha = 0.9, stroke = 1) + 
  geom_line(data = dl, aes(forwet_mean, impsur_mean, group = objectid, color = factor(biodiv_sig)), show.legend = FALSE, size = 0.3) + #, color = "white") +
  geom_label(aes(forwet_mean, impsur_mean, label=lab, color = factor(biodiv_sig)), fill= "grey90", show.legend = FALSE,
              size = 3, alpha = 0.8, fontface = "bold", nudge_x = 0.01, nudge_y = 0.015) +
  #geom_text(aes(forwet_mean, impsur_mean, label=lab), size = 3, alpha = 0.7, fontface = "bold") +
  scale_color_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = color.pts) +
  scale_shape_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(1,1,2,0,6)) +
  scale_size_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(5,3,2,2,2)) +
  # theme_dark() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = margin(rep(0.25,4), unit= "in"),
        legend.position = c(.85,.75),
        legend.title.align = 0.5,
        legend.key = element_rect(fill = "white"), # background color for legend symbols
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = 0.2),
        panel.grid.minor.y = element_blank(),
        legend.background = element_rect(fill = "grey98", color = "black"),
        legend.box.background = element_rect(fill = "grey25"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black")) +
  scale_x_continuous(labels = format(seq(0,1,by=0.1),nsmall=2), breaks = seq(0,1,by=0.1), expand = c(0,0.022)) +
  scale_y_continuous(labels = format(seq(0,1,by=0.05),nsmall = 2), breaks = seq(0,1,by=0.05), expand = c(0,0.01)) +
  xlab("\n% Forest or Wetland Cover in 250-m Flow Buffer") +
  ylab("% Impervious Surface in 250-m Flow Buffer\n") + 
  ggtitle("Stream Conservation Units and Landscape Integrity")

p

ggsave(filename = "output/scu_biodiv1-5.png", plot = p, width = 11, height = 8, units = "in", pointsize = 14, family = "sans")
# ggsave(filename = "scu_biodiv1-5.svg", plot = p, width = 11, height = 8, units = "in", pointsize = 14)

#####
# Scatter plot with B-ranks 1-2
#####

# create subsets for just B1/B2
d2 <- d[d$biodiv_sig %in% c("B1: Outstanding significance","B2: Very high significance"),]
dl2 <- dl[dl$biodiv_sig %in% c("B1: Outstanding significance","B2: Very high significance"),]
rects2 <- rects[1:2,]

# plot all B-ranks
p2 <- ggplot(data = d2) + 
  geom_rect(data = rects2, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = factor(group)), alpha = alpha, show.legend = FALSE) +
  scale_fill_manual(values = color.bkd) + 
  geom_text(data = rects2, aes(x=x1+0.02, y=y2-0.01, label=group, hjust = 0), size=4, fontface = "italic") +
  geom_point(aes(forwet_mean, impsur_mean, shape = factor(biodiv_sig), color = factor(biodiv_sig), size = factor(biodiv_sig)), alpha = 0.9, stroke = 1) + 
  geom_line(data = dl2, aes(forwet_mean, impsur_mean, group = objectid, color = factor(biodiv_sig)), show.legend = FALSE, size = 0.3) + #, color = "white") +
  geom_label(aes(forwet_mean, impsur_mean, label=lab, color = factor(biodiv_sig)), fill= "grey90", show.legend = FALSE,
             size = 3, alpha = 0.8, fontface = "bold", nudge_x = 0.01, nudge_y = 0.013) +
  #geom_text(aes(forwet_mean, impsur_mean, label=lab), size = 3, alpha = 0.7, fontface = "bold") +
  scale_color_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = color.pts[1:length(unique(d2$biodiv_sig))]) +
  scale_shape_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(1,1,2,0,6)) +
  scale_size_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(5,3,2,2,2)) +
  # theme_dark() + 
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        plot.margin = margin(rep(0.25,4), unit= "in"),
        legend.position = c(.85,.75),
        legend.title.align = 0.5,
        legend.key = element_rect(fill = "white"), # background color for legend symbols
        panel.background = element_rect(fill = "white", color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(color = "black", size = 0.2),
        panel.grid.minor.y = element_blank(),
        legend.background = element_rect(fill = "grey98", color = "black"),
        legend.box.background = element_rect(fill = "grey25"),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, color = "black")) +
  scale_x_continuous(labels = format(seq(0,1,by=0.1),nsmall=2), breaks = seq(0,1,by=0.1), expand = c(0,0.022)) +
  scale_y_continuous(labels = format(seq(0,1,by=0.05),nsmall = 2), breaks = seq(0,1,by=0.05), expand = c(0,0.01)) +
  xlab("\n% Forest or Wetland Cover in 250-m Flow Buffer") +
  ylab("% Impervious Surface in 250-m Flow Buffer\n") + 
  ggtitle("Stream Conservation Units and Landscape Integrity")

p2 

ggsave(filename = "output/scu_biodiv1-2.png", plot = p2, width = 11, height = 8, units = "in", pointsize = 14, family = "sans")
# ggsave(filename = "scu_biodiv1-2.svg", plot = p2, width = 11, height = 8, units = "in", pointsize = 14)

#####
# Heatmap with histograms on axes
#####

# generate spectral color ramp for heatmap
library(RColorBrewer)
rf <- colorRampPalette(rev(brewer.pal(11,'Spectral')))
r <- rf(10)

m <- ggplot(d, aes(forwet_mean,impsur_mean)) + 
  geom_bin2d(bins = 15, binwidth = c(0.05, .01)) + 
  scale_fill_gradientn(name = "SCU count", colors= c("grey80",rf(7))) +
  theme_bw() + 
  theme(legend.position = c(0.85,0.8), legend.background = element_rect(fill=NA), legend.title.align = 0.5,
        legend.box.background = element_rect(colour = "black", fill = NA),
        panel.border = element_rect(size = 1.25),
        plot.background = element_blank(), plot.margin = unit(c(0,0,0,0), units = "in"),
        panel.grid.minor = element_blank()) + 
  xlab("Proportion of Forest or Wetland Cover in 250-m Flow Buffer") +
  ylab("Proportion of Impervious Surface in 250-m Flow Buffer") + 
  coord_cartesian(xlim = c(0, 1), ylim = c(0,.35), expand = 0) + 
  theme(plot.margin = unit(c(0.2,0.2,0.2,0.2), units = "in"), aspect.ratio = 1) + 
  scale_x_continuous(labels = format(seq(0,1,by=0.2),nsmall=2), breaks = seq(0,1,by=0.2)) +
  scale_y_continuous(labels = format(seq(0,1,by=0.05),nsmall =2), breaks = seq(0,1,by=0.05))

# standalone heatmap
m.sing <- m + ggtitle("Stream Conservation Units and Landscape Integrity") + xlab("\nProportion of Forest or Wetland Cover in 250-m Flow Buffer") +
  ylab("Proportion of Impervious Surface in 250-m Flow Buffer\n")
m.sing

ggsave(m.sing,filename = "output/scu_heatmap.png", height = 6, width = 6, dpi = 300, units = "in")

# x-axis histogram
hx <- ggplot(data = d) + 
  geom_histogram(aes(forwet_mean), binwidth = 0.05, alpha = .9, fill = "darkgreen", boundary = 0.5) +
  theme_classic() +
  coord_cartesian(xlim = c(0, 1), expand = 0) +
  theme(plot.margin = unit(c(5.4,0.20,-.45,0.34), units = "in"),
        plot.title = element_text(hjust = .75),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank()) +
  scale_y_continuous(labels = c(20,40,60), breaks =c(20,40,60)) +
  xlab("") +
  ylab("SCU count") +
  ggtitle("Stream Conservation Units and Landscape Integrity\n")

# y-axis histogram
hy <- ggplot(data = d) + 
  geom_histogram(aes(impsur_mean), binwidth = 0.01, alpha = .9, fill = "purple", boundary = .01) +
  theme_classic() +
  coord_flip(xlim = c(0, 0.35), expand = 0) + 
  theme(plot.margin = unit(c(0.29,5.25,0.29,-.35), units = "in"),
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank()) +
  scale_y_continuous(labels = c(1, 10, 300), breaks =c(1, 10, 300), trans = "log1p") +
  xlab("") +
  ylab("SCU count") 

# load multiplot fn
source("multiplot_fn.R")
# plot it
multiplot(hx, m, NULL, hy, cols = 2)
# Note that due to margin settings, plot will likely not display correctly in R plotting window

# ouptut svg
# svg(filename = "multi12.svg", width = 12, height = 12, family = "sans")
# multiplot(hx, m, NULL, hy, cols = 2)
# dev.off()

# output png
png(filename = "output/multiplot_heat-hists.png", width = 12, height = 12, family = "sans", units = "in", res = 300)
multiplot(hx, m, NULL, hy, cols = 2)
dev.off()

# crop if magick installed
if ("magick" %in% installed.packages()) {
  library(magick)
  pg <- image_read("output/multiplot_heat-hists.png")
  pg2 <- image_crop(pg, "2100x2100+0+1500")
  image_write(pg2, path = "output/multiplot_heat-hists.png")
}

#####
# Boxplots
#####

# 1. Boxplot with scores by group (one type of scoring)

# rename levels for better plotting
d$biodiv_sig <- factor(d$biodiv_sig, levels = bscores,
                       labels = c("B1: Outstanding \nsignificance","B2: Very high \nsignificance","B3: High \nsignificance","B4: Moderate \nsignificance","B5: General \nsignificance"))
# set up color ramp
color.pts<- color.fn(length(unique(d$biodiv_sig)))

p <- ggplot(data = d) + 
  stat_boxplot(aes(biodiv_sig, score_2b), geom = "errorbar", width = 0.2, size = 0.8) + 
  geom_boxplot(aes(biodiv_sig, score_2b), fill = color.pts, size = 0.8, color = "grey20", alpha = 0.7) +
  theme_bw() +
  theme(plot.title = element_text(size = 16, hjust = 0.5),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)) + 
  xlab("\nBiodiversity ranking") +
  ylab("Score\n") +
  ggtitle("Title")

p

ggsave(p, filename = "output/barplot_scores.png", device = "png", width = 10, height = 6, units = "in", dpi = 300)


# 2. Boxplot grouped by biodiversity score and factor (by scoring type)

# first re-arrange the data
d1<-d[c("biodiv_sig","score_2b")]
names(d1) <- c("biodiv_sig","score")
d1$score_nm <- "Double Biodiversity"
d2<-d[c("biodiv_sig","score_eqwts")]
names(d2) <- c("biodiv_sig","score")
d2$score_nm <- "Equal Weights"

df<-rbind(d1,d2)

p <- ggplot(data = df, aes(x=biodiv_sig, y=score, fill=score_nm)) +
  geom_boxplot(alpha=0.7, width = 0.8) +
  #geom_text(aes(4, 90), label = "bla") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        panel.grid.major.x = element_blank(),
        legend.direction = "horizontal", legend.position = c(0.8,0.95)) +
  xlab("\nBiodiversity ranking") +
  ylab("Score") +
  labs(fill = "Score Type")

p

ggsave(p, filename = "output/barplot_scores.png", res = 300, width = 8, height = 6, units = "in")


# 3. Boxplot grouped by biodiversity score and factor (forest/wetland vs. impervious surface)

# first re-arrange the data
d1<-d[c("biodiv_sig","forwet_mean")]
names(d1) <- c("biodiv_sig","Proportion")
d1$score_nm <- "Forest/Wetland"
d2<-d[c("biodiv_sig","impsur_mean")]
names(d2) <- c("biodiv_sig","Proportion")
d2$Proportion <- d2$Proportion/max(d2$Proportion)
d2$score_nm <- "Impervious Surface"

df<-rbind(d1,d2)
df$score_nm <- factor(df$score_nm, levels = c("Impervious Surface","Forest/Wetland"))

p <- ggplot(data = df, aes(x=biodiv_sig, y=Proportion, fill=score_nm)) +
  geom_boxplot(alpha=0.7, width = 0.8) +
  #geom_text(aes(4, 90), label = "bla") +
  theme_bw() +
  theme(plot.title = element_text(size = 14, face = "bold"),
        text = element_text(size = 12),
        legend.title = element_text(face = "bold"),
        axis.title = element_text(face="bold"),
        axis.text.x=element_text(size = 11),
        panel.grid.major.x = element_blank(),
        legend.direction = "vertical", legend.position = c(0.12,0.93), legend.background = element_blank()) +
  scale_y_continuous(sec.axis = sec_axis(~.*0.3211435, name = "Proportion of Impervious Surface in 250-m Flow Buffer\n")) + 
  xlab("\nBiodiversity ranking") +
  ylab("Proportion of Forest or Wetland Cover in 250-m Flow Buffer\n") + 
  labs(fill = NULL)

p

ggsave(p, filename = "output/barplot_forwet-impsur.png", dpi = 300, width = 9, height = 6, units = "in")
