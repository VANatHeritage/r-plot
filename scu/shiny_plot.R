# SCU Prioritization graphs - shiny app

# Run full script to execute app

# Created By: David Bucklin
# Last updated: 2017-09-22

library(shiny)
library(ggplot2)

# ui
ui <- fluidPage(
  titlePanel('SCU plots'),
  sidebarLayout(
    sidebarPanel(
      fileInput("d", label = "SCU CSV file"), 
      selectInput("br", label = "B-Rank selection", multiple = TRUE, choices = c("B1: Outstanding significance","B2: Very high significance","B3: High significance","B4: Moderate significance","B5: General significance"),
                  selected = c("B1: Outstanding significance","B2: Very high significance","B3: High significance","B4: Moderate significance","B5: General significance")),
      selectInput("lab", label = "Select features to label:", choices = c(0), selected = c(0), multiple = TRUE)
    ),
    mainPanel(
      plotOutput("plot", brush = "plot_brush"),
      h4(""),
      verbatimTextOutput("info")
    )
  )
)

server <- function(input, output, session) {
  
  # create background color polygons
  rects <- data.frame(
    group = c("sensitive","impacted","non-supporting"), 
    x1 = c(0,0,0), x2 = c(1,1,1), y1 = c(0,0.1,0.25), y2 = c(0.1,0.25,0.35))
  rects$group <- factor(rects$group, levels = c("sensitive","impacted","non-supporting"), ordered = TRUE)
  
  d <- reactive({
    file <- input$d
    if (is.null(file)) return(NULL)
    d <- read.csv(file$datapath)
    names(d) <- tolower(names(d))
    d <- d[c("objectid","site_name","biodiv_sig","forwet_mean","impsur_mean", "lngid")]
    d
  })
  
  d1 <- reactive({
      d <- d()
      # subset by group
      d$biodiv_sig <- factor(d$biodiv_sig, levels = c("B1","B2","B3","B4","B5"),
                             labels = c("B1: Outstanding significance","B2: Very high significance","B3: High significance","B4: Moderate significance","B5: General significance"))
      d <- d[d$biodiv_sig %in% input$br,]
      d
  })
  
  observe({
    if (!is.null(d()) && length(input$br) > 0) {
      d <- d1()
      if (isolate(input$lab) == 0) samp <- sample(d$lngid,5) else samp <- isolate(input$lab)
      updateSelectInput(session, inputId = "lab", choices = d$lngid, selected = samp[samp %in% d$lngid])
    }
  })
  
  d2 <- reactive({
    label.pts <- input$lab
    d <- d1()
    d$lab <- ifelse(d$lngid %in% label.pts, as.character(d$lngid), NA)
    d
  })
  
  dl <- reactive({
    # leader lines
    d <- d2()
    dend <- d[!is.na(d$lab),]
    dend$forwet_mean <- dend$forwet_mean + 0.005
    dend$impsur_mean <- dend$impsur_mean + 0.01
    dl <- rbind(d[!is.na(d$lab),], dend)
    dl
  })
  
  # plot all B-ranks
  
  output$plot <- renderPlot({
    d <- as.data.frame(d2())
    dl <- dl()
    
    # automated blue->red points, blue-yellow-red bkgd
    color.pts<- colorRampPalette(c("blue1","firebrick"))
    color.pts<- color.pts(length(unique(d()$biodiv_sig)))
    
    # background colors and alpha
    color.bkd <- c("grey90","grey75","grey60")
    alpha <- 0.5

    ggplot(data = d) + 
      geom_rect(data = rects, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, fill = factor(group)), alpha = alpha, show.legend = FALSE) +
      scale_fill_manual(values = color.bkd) + 
      geom_text(data = rects, aes(x=x1+0.02, y=y2-0.01, label=group, hjust = 0), size=4, fontface = "italic") +
      geom_point(aes(forwet_mean, impsur_mean, shape = factor(biodiv_sig), color = factor(biodiv_sig), size = factor(biodiv_sig)), alpha = 0.9, stroke = 2) + 
      geom_line(data = dl, aes(forwet_mean, impsur_mean, group = objectid, color = factor(biodiv_sig)), show.legend = FALSE, size = 0.3) + #, color = "white") +
      geom_label(aes(forwet_mean, impsur_mean, label=lab, color = factor(biodiv_sig)), fill= "grey90", show.legend = FALSE,
                 size = 3, alpha = 0.8, fontface = "bold", nudge_x = 0.01, nudge_y = 0.015) +
      #geom_text(aes(forwet_mean, impsur_mean, label=lab), size = 3, alpha = 0.7, fontface = "bold") +
      scale_color_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = color.pts[sort(as.numeric(unique(d$biodiv_sig)))]) +
      scale_shape_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(1,1,2,0,6)[sort(as.numeric(unique(d$biodiv_sig)))]) +
      scale_size_manual(name = "Stream Conservation Units\n Biodiversity Rank", values = c(6,4,3,3,3)[sort(as.numeric(unique(d$biodiv_sig)))]) +
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
  }#, height = 800, width = 1100
  )
  
  output$info <- renderPrint({
    d <- as.data.frame(d2())[,2:6]
    bp <- brushedPoints(d, input$plot_brush, xvar = "forwet_mean", yvar = "impsur_mean")
    bp[order(bp$biodiv_sig),]
  })
  
}

shinyApp(ui, server)
