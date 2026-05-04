library(shiny)
library(rcarbon)
library(scales)
library(stringi)


plot_custom_cal <- function(x, ind=1, id="Sample", calendar="BCAD", bw=FALSE, calCurves = "intcal20", 
                            cex.axis=1.1, cex.lab=1.1, cex.label=1.0, cex.main=1.3, ...){
  
  if (bw) {
    pal <- list(main="grey40", uncal="grey85", curve="black", curve_shd="grey90", marker="black", text="black")
  } else {
    pal <- list(main="grey40", uncal="#b6cbdb", curve="#e67e22", curve_shd="#e67e2233", marker="#c0392b", text="#34495e")
  }
  
  df <- as.data.frame(x$grids[[ind]])
  cra <- x$metadata$CRA[ind]
  error <- x$metadata$Error[ind]
  hdres <- rcarbon::hpdi(x, credMass=0.954)[[ind]]
  w_mean_bp <- sum(df$calBP * df$PrDens) / sum(df$PrDens)
  
  fmt_year <- function(bp) {
    y <- 1950 - bp
    if (y <= 0) return(paste(abs(round(y)), "BCE"))
    else return(paste(round(y), "CE"))
  }
  
  hpd_txt <- apply(hdres, 1, function(r) paste(fmt_year(r[1]), "-", fmt_year(r[2])))
  legend_content <- c("95.4% probability:", hpd_txt, paste("Mean:", fmt_year(w_mean_bp)))
  
  plotyears <- if(calendar=="BCAD") 1950 - df$calBP else df$calBP
  xvals <- c(plotyears[1], plotyears, plotyears[length(plotyears)], plotyears[1])
  yvals <- c(0, df$PrDens, 0, 0)
  xrng <- c(min(xvals[yvals>1e-6])-50, max(xvals[yvals>1e-6])+50)
  xlim <- if(calendar=="BP") rev(xrng) else xrng
  yrng <- c(0, max(yvals)*2.5) 
  
  par(mar=c(6, 5, 5, 6), family="sans", cex.main=cex.main, cex.lab=cex.lab) 
  
  plot(xvals, yvals, type="n", ylim=yrng, xlim=xlim, xaxt='n', yaxt='n', xlab="", ylab="",
       main=paste0(id, " (", cra, " \u00B1 ", error, " BP)"), col.main=pal$text)
  
  xticks <- seq(round(xlim[1], -1), round(xlim[2], -1), by=ifelse(diff(xlim)>500, 100, 50))
  if(!(0 %in% xticks) && xlim[1] < 0 && xlim[2] > 0) xticks <- sort(c(xticks, 0))
  xtick_labels <- as.character(abs(xticks))
  if(any(xticks == 0)) xtick_labels[xticks == 0] <- "BCE/CE"
  
  axis(1, at=xticks, labels=xtick_labels, las=2, cex.axis=cex.axis, col=pal$text)
  axis(4, cex.axis=cex.axis, col=pal$text)
  mtext("Probability Density", side=4, line=4, cex=cex.lab, col=pal$text)
  mtext(if(calendar=="BCAD") "Years cal BCE/CE" else "Years cal BP", side=1, line=4.5, cex=cex.lab)
  
  if(xlim[1] <= 0 && xlim[2] >= 0) abline(v = 0, lty = 3, col = pal$marker)
  polygon(xvals, yvals, col=pal$main, border=if(bw) "black" else NA)
  
  legend("topright", legend=legend_content, bty="n", cex=cex.label, 
         text.font=c(2, rep(1, length(hpd_txt)), 2), text.col=pal$text)
  
  par(new=TRUE)
  cradf1 <- data.frame(CRA=seq(cra-4*error, cra+4*error, length.out=100))
  cradf1$Prob <- dnorm(cradf1$CRA, mean=cra, sd=error)
  cradf1$RX <- scales::rescale(cradf1$Prob, to=c(xlim[1], xlim[1] + diff(xlim)*0.15))
  
  plot(cradf1$RX, cradf1$CRA, type="l", axes=FALSE, xlab=NA, ylab=NA, xlim=xlim, ylim=c(cra-8*error, cra+8*error), col=pal$uncal)
  polygon(c(xlim[1], cradf1$RX, xlim[1]), c(min(cradf1$CRA), cradf1$CRA, max(cradf1$CRA)), col=pal$uncal, border=NA)
  
  axis(2, las=2, cex.axis=cex.axis, col=pal$text)
  title(ylab="Radiocarbon Age (BP)", line=3.5, cex.lab=cex.lab)
  
  calCurveFile <- system.file("extdata", paste0(calCurves, ".14c"), package="rcarbon")
  if (file.exists(calCurveFile)){
    cc <- read.table(calCurveFile, sep=",", skip=11); names(cc) <- c("BP","CRA","Error","D14C","Sigma")
    cc$RX <- if(calendar=="BCAD") 1950 - cc$BP else cc$BP
    cc <- cc[cc$BP >= (min(df$calBP)-100) & cc$BP <= (max(df$calBP)+100),]
    polygon(c(cc$RX, rev(cc$RX)), c(cc$CRA+cc$Error, rev(cc$CRA-cc$Error)), col=pal$curve_shd, border=NA)
    lines(cc$RX, cc$CRA, col=pal$curve, lwd=1.5, lty=if(bw) 2 else 1)
  }
}

# --- SHINY UI ---
ui <- fluidPage(
  titlePanel("C-Turkey: Radiocarbon Calibration Tool"),
  sidebarLayout(
    sidebarPanel(
      textInput("sample_id", "Sample ID", value = "Sample"),
      numericInput("bp_age", "Uncalibrated Age (BP)", value = 2000, min = 0, max = 50000),
      numericInput("bp_error", "Standard Error (±)", value = 30, min = 1),
      hr(),
      selectInput("calendar", "Calendar System", choices = c("BCE/CE" = "BCAD", "cal BP" = "bp")),
      selectInput("calcurve", "Calibration Curve", 
                  choices = c("intcal20", "intcal13", "shcal20", "marine20")),
      checkboxInput("bw_mode", "Black & White Mode", value = FALSE),
      hr(),
      h4("Download Results"),
      downloadButton("downloadPNG", "Download PNG", class = "btn-info"),
      downloadButton("downloadPDF", "Download PDF", class = "btn-primary"),
      br(), br(),
      helpText("Uses rcarbon engine."),
      helpText("Cite: Altınışık, N. E. (2026). C-Turkey: A comprehensive radiocarbon dataset from Türkiye (v0). Zenodo. https://doi.org/10.5281/zenodo.20011918")
    ),
    mainPanel(
      plotOutput("calPlot", height = "700px")
    )
  )
)

# --- SHINY SERVER ---
server <- function(input, output) {
  
  calData <- reactive({
    calibrate(x = input$bp_age, errors = input$bp_error, calCurves = input$calcurve)
  })
  
  output$calPlot <- renderPlot({
    plot_custom_cal(calData(), id = input$sample_id, calendar = input$calendar, 
                    bw = input$bw_mode, calCurves = input$calcurve)
  })
  
  output$downloadPNG <- downloadHandler(
    filename = function() { 
      clean_id <- gsub("[^[:alnum:]]", "_", stri_trans_general(input$sample_id, "latin-ascii"))
      paste0(clean_id, "_", input$bp_age, "BP.png") 
    },
    content = function(file) {
      png(file, width = 2400, height = 1800, res = 300) 
      plot_custom_cal(calData(), id = input$sample_id, calendar = input$calendar, 
                      bw = input$bw_mode, calCurves = input$calcurve)
      dev.off()
    }
  )
  
  output$downloadPDF <- downloadHandler(
    filename = function() { 
      clean_id <- gsub("[^[:alnum:]]", "_", stri_trans_general(input$sample_id, "latin-ascii"))
      paste0(clean_id, "_", input$bp_age, "BP.pdf") 
    },
    content = function(file) {
      cairo_pdf(file, width = 10, height = 8)
      plot_custom_cal(calData(), id = input$sample_id, calendar = input$calendar, 
                      bw = input$bw_mode, calCurves = input$calcurve)
      dev.off()
    }
  )
}

shinyApp(ui = ui, server = server)