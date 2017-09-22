# Plot Stream Conservation Unit (SCU) Biodiversity Ranks

# Scatter plot of SCU B-ranks, with seperate graphs for B-ranks 1-5 and 1-2,
# at locations relative to forest/wetland coverage (x) and 
# impervious surface coverage (y) in a buffer around the SCU.

# Created By: David Bucklin
# Last updated: 2017-09-22


# Load libraries
library(ggplot2)
# library(readxl) for reading excel worksheets
# library(RColorBrewer) # can be used to generate color ramps

# load data
setwd(".../r-plot/scu")
# d <- read_xls("SCUs.xls")
d <- read.csv("SCUs.csv")
names(d) <- tolower(names(d))

d$biodiv_sig <- factor(d$biodiv_sig, levels = c("B1","B2","B3","B4","B5"),
       labels = c("B1: Outstanding significance","B2: Very high significance","B3: High significance","B4: Moderate significance","B5: General significance"))

# blue-yellow-red points, grey bkgd
# color.pts <- rev(brewer.pal(5,"RdYlBu"))
# color.bkd <- c("grey90","grey75","grey60")
# alpha <- 0.5

# automated blue->red points, blue-yellow-red bkgd
color.pts<- colorRampPalette(c("blue1","firebrick"))
color.pts<- color.pts(length(unique(d$biodiv_sig)))

# background colors and alpha
color.bkd <- c("grey90","grey75","grey60")
alpha <- 0.5

# list points to label (random sample of 10 + 5 B1's)
# label.pts <- c(sample(d$lngid, 10, replace = FALSE, prob = d$impsur_mean), sample(d$lngid[d$biodiv_sig == "B1"], 5, replace = FALSE))
# manual list for labeling
label.pts <- c(3659, 3337, 220, 58, 15, 395)

# subset columns
d <- d[c("objectid","site_name","biodiv_sig","forwet_mean","impsur_mean", "lngid")]
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

# plot all B-ranks
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

ggsave(filename = "scu_biodiv1-5.png", plot = p, width = 11, height = 8, units = "in", pointsize = 14, family = "sans")
ggsave(filename = "scu_biodiv1-5.svg", plot = p, width = 11, height = 8, units = "in", pointsize = 14)

#######

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

ggsave(filename = "scu_biodiv1-2.png", plot = p2, width = 11, height = 8, units = "in", pointsize = 14, family = "sans")
ggsave(filename = "scu_biodiv1-2.svg", plot = p2, width = 11, height = 8, units = "in", pointsize = 14)

#svg(filename = "scu_biodiv1-2.svg", width = 11, height = 8, pointsize = 14, family = "sans")
#p2
#dev.off()
