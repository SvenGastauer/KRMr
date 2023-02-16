#' RUn KRM shiny App
#' @import shiny
#' @import plotly
#' @import shinythemes
#' @import DT
#' @import tidyverse
#' @import ggplot2
#' @import magick
#' @import grid
#' @import sf
#' @import tibble
#' @export

run_krm<-function(){
  #Check if all packages are available and install missing ones
  #Make sure you have KRMr installed

  # list.of.packages <- c("shiny","grid","shiny","DT","tidyverse","ggplot2", "magick","sf","shinythemes","plotly")
  # new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  # if(length(new.packages)) install.packages(new.packages)
  #
  # #####################################################################
  # library(shiny) #shiny app
  # library(plotly)
  # library(shinythemes)
  # library(DT)
  # library(tidyverse) # elegant writing
  # library(ggplot2) #plotting
  # library(magick) #image procesing
  # library(grid) #raster image
  # library(sf) #coordinatre matching/ lines crossing polygons
  # library(KRMr) # KRM modelling
  # #####################################################################
  # #####################################################################

  #scale values between 0 and1
  range01 <- function(xy){
    xy$y = (xy$y)/(max(xy$x)-min(xy$x))
    xy$x=(xy$x-min(xy$x))/(max(xy$x)-min(xy$x))
    return(xy)}

  #####################################################################
  #####################################################################

  #get intersection of polygon and lines
  getx=function(xy, n=0.01){
    xy=rbind(xy,xy[1,])
    spol = sf::st_polygon(list(as.matrix(xy)))
    ns=length(seq(0, max(xy$x), by=n))
    table_xy <- data.frame(

      x = rep(seq(0, max(xy$x), by=n), each=2),
      y = rep(c(-50,50),times=ns),
      id = rep(seq(1,ns), each=2)) %>%
      group_by(id) %>%
      st_as_sf(coords = c("x", "y")) %>% # create points
      group_by(id) %>%
      summarise() %>% # union points into lines using our created lineid
      st_cast("LINESTRING")

    i1=sf::st_intersection(table_xy,spol)

    ixy=data.frame()

    for(x in 1:length(i1$geometry)){
      tmp=data.frame(st_coordinates(i1$geometry[x][1]))
      if(nrow(tmp)>0){
        tmp$L1=x
        tmp=tmp[,1:3]
        tmp=tmp%>%group_by(X)%>%
          summarise(Ymin=min(Y),
                    Ymax=max(Y))
        if(nrow(tmp)==1){tmp=rbind(tmp,tmp)}
        ixy=rbind(ixy,tmp)
      }
    }
    ixy$dy=ixy$Ymax - ixy$Ymin
    return(ixy)
  }
  #####################################################################
  #####################################################################

  # generate xyz points for ellipse
  gel=function(np=100, xc=0,yc=0, h=1,w=1,phi=0){
    w=w/2
    h=h/2
    phi <- (phi + 90) * pi / 180 #degrees to radians
    t <- seq(0, 2*pi, length.out = np) # Compute angular points
    x <- xc + w*cos(t)*cos(phi) - h*sin(t)*sin(phi) #X coordinates
    y <- y <- yc + w * cos(t) * sin(phi) + h * sin(t) * cos(phi) #y coordinates
    return(cbind(x,y))
  }

  #####################################################################
  #####################################################################

  # get xyz points for ellipse from top and side view
  getxyz<-function(xxy,xxy2){
    pd=data.frame()
    for(i in 1:nrow(xxy)){
      o=data.frame(gel(w=xxy$dy[i],h=xxy2$dy[i], xc=xxy$X[i], yc=(xxy$Ymax[i]+xxy$Ymin[i])/2))
      o$z=xxy$X[i]
      pd=rbind(pd,o)
    }
    return(pd)
  }

  #####################################################################
  #####################################################################

  get_shp3d<-function(shpfn){
    shp=read.csv(shpfn)

    fb=do.call('rbind',lapply(1:nrow(shp), FUN=function(i){
      ellipse(x=0,
              y=0,
              major=shp$w_fb[i],
              minor=abs(shp$z_fbU[i]-shp$z_fbL[i]), nv=100)%>%mutate(z=shp$x_fb[i])
    }))
    swb=do.call('rbind',lapply(1:length(shp$x_sb[is.na(shp$x_sb)==F]), FUN=function(i){
      ellipse(x=0,
              y=0,
              major=shp$w_sb[i],
              minor=abs(shp$z_sbU[i]-shp$z_sbL[i]), nv=100)%>%mutate(z=shp$x_sb[i])
    }))


    swb2=do.call('rbind',lapply(1:length(shp$x_sb2[is.na(shp$x_sb2)==F]), FUN=function(i){
      ellipse(x=0,
              y=0,
              major=shp$w_sb2[i],
              minor=abs(shp$z_sbU2[i]-shp$z_sbL2[i]), nv=100)%>%mutate(z=shp$x_sb2[i])
    }))

    fig=plot_ly(data=fb, x = ~x, y = ~y, z = ~z,
                #width=0.1,
                opacity=0.1, type = 'scatter3d', mode = 'lines', name='FB')
    if(nrow(swb)>0){
      fig <- fig %>% add_trace(data=swb,x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',name="swb")
    }
    if(nrow(swb2)>0){
      fig <- fig %>% add_trace(data=swb2,x = ~x, y = ~y, z = ~z, type = 'scatter3d', mode = 'lines',name="swb2")
    }
    return(fig%>%layout(scene = list(aspectmode='data')))
  }




  ellipse <- function (x=0, y=0, major = 1, minor = 1, theta = 0, nv = 10){
    major=major/2
    minor=minor/2
    angle <- theta * pi/180
    segment <- c(0, 360)
    segment <- segment * pi/180

    z <- seq(segment[1], segment[2], length = nv + 1)
    xx <- minor * cos(z)
    yy <- major * sin(z)
    alpha <- atan2(xx, yy)
    rad <- sqrt(xx^2 + yy^2)
    xp <- rad * cos(alpha + angle) + x
    yp <- rad * sin(alpha + angle) + y

    return(data.frame(x=xp,y=yp))
  }

  #####################################################################
  #####################################################################


  lims=c(-1,1)

  #Design App
  ui <- fluidPage(theme = shinytheme("cyborg"),
                  titlePanel("Fishy Kirchhoff-Ray Mode model"),
                  tabsetPanel(
                    tabPanel("Run KRM",
                             fluidRow(
                               column(3, numericInput("freq_min", "Minimum Frequency (kHz):", 10, step = 0.1, min=1, max=2000)),
                               column(3, numericInput("freq_max", "Maximum Frequency (kHz):", 260, step = 0.1, min=1, max=2000)),
                               column(3, numericInput("freq_n", "N Frequency values:", 251, min=1, max=1000))),
                             fluidRow(
                               column(3, numericInput("theta_min", "Minimum rotation:", 65, step = 0.01, min=65, max=115)),
                               column(3, numericInput("theta_max", "Maximum rotation:", 115, step = 0.01, min=65, max=115)),
                               column(3, numericInput("theta_n", "N rotation values:", 41, min=1, max=1000))),
                             fluidRow(
                               column(3, numericInput("cw", "Ambient Sound speed (m/s):", 1490.0, step = 0.01, min=0.1, max=300000)),
                               column(3, numericInput("cfb", "Sound speed Body:", 1570, min=1, max=300000)),
                               column(3, numericInput("csb", "Sound speed Swimbladder:", 345, min=1, max=300000))),
                             fluidRow(
                               column(3, numericInput("rhow", "Ambient Denisty (kg/m3):", 1030, step = 0.01, min=0.1, max=5000)),
                               column(3, numericInput("rhofb", "Body Density (kg/m3):", 1070, step = 0.01, min=0.1, max=5000)),
                               column(3, numericInput("rhosb", "Swimbladder Density (kg/m3):", 1.24, step = 0.01, min=0.1, max=5000))),
                             fluidRow(
                               column(3, numericInput("L_min", "Minimum Length (m):", 0.25, step = 0.001, min=0.001, max=2)),
                               column(3, numericInput("L_max", "Maximum Length (m):", 0.25, step = 0.001, min=0.001, max=2)),
                               column(3, numericInput("L_n", "N Length values:", 1, min=1, max=1000))),
                             fluidRow(
                               column(3,fileInput(inputId = "shpfile",
                                                  label = "Upload Shape FIle",
                                                  accept = c("text/csv",
                                                             "text/comma-separated-values,text/plain",
                                                             ".csv"))),
                               column(3, selectInput("modsb", "SwB Model:",choices=c('fluid','soft'),selected='soft', multiple = F)),
                               column(3, selectInput("modsb2", "SwB2 Model:",choices=c('fluid','soft'),selected='soft', multiple = F))
                             ),
                             fluidRow(
                               column(3),
                               column(6, plotlyOutput('shp3D'))),
                             actionButton("rKRM", "Run KRM"),
                             downloadButton("exportTS", "Download KRM Results"),
                             hr(),
                             fluidRow(
                               column(3, numericInput("clow", "Minimum TS (dB):", -80, step = 0.1, min=-999, max=30)),
                               column(3, numericInput("cup", "Maximim TS (dB):", -30, step=0.1,min=-999, max=30)),
                               column(3, selectInput("thetas", "Theta:",choices=90, multiple = T)),
                               column(3, selectInput("Ls", "L:",choices=0.25, multiple = T))
                             ),
                             fluidRow(
                               column(6,
                                      plotlyOutput("krmplot")),
                               column(6, plotlyOutput("krmfreqplot"))),
                             hr(),
                             DT::dataTableOutput("TStable"),
                             #tableOutput("TStable"),
                    ),
                    tabPanel("Shape Generator",
                             fluidRow(
                               column(6, fileInput(inputId = "file1",
                                                   label = "Upload Image",
                                                   accept = c('image/png', 'image/jpeg','image/jpg')),
                                      numericInput("rot1", "Rotation:", 0, min = -360, max = 360)),

                               column(6, fileInput(inputId = "file2",
                                                   label = "Upload Image",
                                                   accept = c('image/png', 'image/jpeg','image/jpg')),
                                      numericInput("rot2", "Rotation:", 0, min = -360, max = 360))),

                             fluidRow(
                               column(6, textInput("polygon_name", label = "Polygon name", value = "Left")),
                               column(6, textInput("polygon_name2", label = "Polygon name", value = "Right"))),

                             fluidRow(
                               column(6, plotOutput("plot1", click = "plot_click")),
                               column(6, plotOutput("plot2", click = "plot_click2")),
                             ),
                             plotOutput("combined"),
                             fluidRow(column(2, actionButton("pshp", "Plot KRM shape")),
                                      column(2, downloadButton("downloadShp", "Download KRM shape"))),
                             plotOutput("krmshp"),
                             # fluidRow(
                             #   column(6, downloadButton("downloadDataLeft","Download"),
                             #          tableOutput("table")),
                             #   column(6, downloadButton("downloadDataRight","Download"),
                             #          tableOutput("table2"))),
                    )))

  #############################################################################
  #############################################################################
  #Server side
  server <- function(input, output,session) {
    #empty coordinates
    coords <- reactiveVal(value = tibble(x = numeric(), y = numeric(), name = character()))
    coords2 <- reactiveVal(value = tibble(x = numeric(), y = numeric(), name = character()))

    #read left file
    re1 <- reactive({
      rf=gsub("\\\\", "/", input$file1$datapath)
      img <- image_read(rf)

      # rotate
      img <- image_rotate(img, input$rot1)
      g <- rasterGrob(img, interpolate=TRUE)
      return(g)

    })

    #read right file
    re2 <- reactive({
      rf=gsub("\\\\", "/", input$file2$datapath)
      img <- image_read(rf)

      # rotate
      img <- image_rotate(img, input$rot2)
      g <- rasterGrob(img, interpolate=TRUE)
      return(g)

    })

    #add coordinate on click (left plot)
    observeEvent(input$plot_click, {
      add_row(coords(),
              x = isolate(input$plot_click$x),
              y = isolate(input$plot_click$y),
              name = isolate(input$polygon_name)
      ) %>% coords()
    })

    #add coordinate on click (rightt plot)
    observeEvent(input$plot_click2, {
      add_row(coords2(),
              x = isolate(input$plot_click2$x),
              y = isolate(input$plot_click2$y),
              name = isolate(input$polygon_name2)
      ) %>% coords2()
    })

    #plot the loaded images and polygons based on clicks (left)
    output$plot1 <- renderPlot({
      ggplot() +
        annotation_custom(re1(), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
        scale_x_continuous(limits = lims,expand=c(0,0))+
        scale_y_continuous(limits = lims,expand=c(0,0))+
        geom_point(data=coords(), aes(x=x,y=y),color="green", size=0.3)+
        geom_polygon(data=coords(), aes(x=x,y=y, group=name, fill=name), alpha=0.1)+
        coord_equal()


    })
    #plot the loaded images and polygons based on clicks (right)
    output$plot2 <- renderPlot({
      ggplot() +
        annotation_custom(re2(), xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf)+
        scale_x_continuous(limits = lims,expand=c(0,0))+
        scale_y_continuous(limits = lims,expand=c(0,0))+
        geom_point(data=coords2(), aes(x=x,y=y),color="green", size=0.3)+
        geom_polygon(data=coords2(), aes(x=x,y=y, group=name, fill=name), alpha=0.1)+
        coord_equal()
    })

    #get the coordinates of the clicked points and scale them
    coco <- reactive({
      #get coords
      c1=coords()
      c2=coords2()

      #scale to 0-1
      c1s=range01(c1)
      c2s=range01(c2)

      #add scaled values
      c1$xs=c1s$x; c1$ys=c1s$y
      c2$xs=c2s$x; c2$ys=c2s$y
      return(list(c1=c1,c2=c2))
    })

    #plot the scaled polygons on top of each other
    output$combined <- renderPlot({
      sh=coco()
      c1=sh$c1
      c2=sh$c2
      #scale internal structure
      if(length(unique(c1$name))==2 & length(unique(c2$name))==2){
        c2$xs[c2$name==unique(c2$name)[2]] = c2$xs[c2$name==unique(c2$name)[2]] - (min(c2$xs[c2$name==unique(c2$name)[2]]) - min(c1$xs[c1$name==unique(c1$name)[2]]))
      }
      co=rbind(c1,c2)

      #make ggplot
      ggplot()+
        geom_point(data=co, aes(x=xs,y=ys))+
        geom_polygon(data=co, aes(x=xs,y=ys, group=name, fill=name), alpha=0.3)+
        coord_equal()
    })

    #render a table with coordinates (obsolete)
    #output$table <- renderTable(coords())
    #output$table2 <- renderTable(coords2())

    #download coordinates (left) (obsolete)
    # output$downloadDataLeft <- downloadHandler(
    #   filename = function() {
    #     paste("side.csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(coords(), file, row.names = FALSE)
    #   }
    # )
    #download coordinates (rightt) (obsolete)
    # output$downloadDataRight <- downloadHandler(
    #   filename = function() {
    #     paste("top.csv", sep = "")
    #   },
    #   content = function(file) {
    #     write.csv(coords2(), file, row.names = FALSE)
    #   }
    # )

    #transform coordinates into shape information for the KRM model
    gshp<-reactive({
      sh=coco()
      c1=sh$c1
      c2=sh$c2
      #scale internal structure
      if(length(unique(c1$name))>1 & length(unique(c2$name))>1){
        for(i in 2:length(unique(c1$name))){
          c2$xs[c2$name==unique(c2$name)[i]] = c2$xs[c2$name==unique(c2$name)[i]] -
            (min(c2$xs[c2$name==unique(c2$name)[i]]) -
               min(c1$xs[c1$name==unique(c1$name)[i]]))
        }
      }

      get_shp=function(c1,c2,i=1){
        fbn = unique(c1$name)[i]
        fbn2 = unique(c2$name)[i]

        #left plot assume side view
        fbd=c1[c1$name==fbn,c("xs","ys")]
        names(fbd)=c("x","y")

        #right plot assume top view
        fbd2=c2[c2$name==fbn2,c("xs","ys")]
        names(fbd2)=c("x","y")

        #side view gives z, top view gives width
        xxy=getx(fbd) #z low up
        xxy2=getx(fbd2) # width
        xxy2 = xxy2%>%filter(xxy2$X %in% xxy$X)
        xxy = xxy%>%filter(xxy$X %in% xxy2$X)
        shp = data.frame(x=xxy$X, w=xxy2$dy, z_U=xxy$Ymax, z_L=xxy$Ymin)
        return(shp)
      }
      fb=get_shp(c1,c2,1)

      if(length(unique(c1$name))==2 & length(unique(c2$name))==2){
        swb=get_shp(c1,c2,2)
        swb2=NULL
      }else if(length(unique(c1$name))==2 & length(unique(c2$name))==1){
        swb=get_shp(c1,c1,2)
        swb2=NULL
      }else if(length(unique(c1$name))==3 & length(unique(c2$name))==3){
        swb=get_shp(c1,c2,2)
        swb2=get_shp(c1,c2,3)
      }else{
        swb=NULL
        swb2=NULL
      }

      if(is.null(swb2)==F){
        nr = max(nrow(fb),nrow(swb), nrow(swb2))
        if(nrow(swb)<nr){
          swb[(nrow(swb)+1):nr,]=NA
        }
        if(nrow(swb2)<nr){
          swb2[(nrow(swb2)+1):nr,]=NA
        }
        if(nrow(fb)<nr){
          fb[(nrow(fb)+1):nr,]=NA
        }
        shpdf=data.frame(x_fb = fb$x, w_fb = fb$w,
                         x_sb = swb$x, w_sb = swb$w,
                         x_sb2 = swb2$x, w_sb2 = swb2$w,
                         z_fbU = fb$z_U, z_fbL = fb$z_L,
                         z_sbU = swb$z_U, z_sbL = swb$z_L,
                         z_sbU2 = swb2$z_U, z_sbL2 = swb2$z_L)
      }else if(is.null(swb)==F){
        nr = max(nrow(fb),nrow(swb))
        if(nrow(swb)<nr){
          swb[(nrow(swb)+1):nr,]=NA
        }
        if(nrow(fb)<nr){
          fb[(nrow(fb)+1):nr,]=NA
        }
        shpdf=data.frame(x_fb = fb$x, w_fb = fb$w,
                         x_sb = swb$x, w_sb = swb$w,
                         z_fbU = fb$z_U, z_fbL = fb$z_L,
                         z_sbU = swb$z_U, z_sbL = swb$z_L)
      }else{
        shpdf=data.frame(x_fb = fb$x, w_fb = fb$w,
                         z_fbU = fb$z_U, z_fbL = fb$z_L)
      }
      #print(shpdf)

      return(shpdf)
    })

    #download the KRM shape information as csv
    output$downloadShp <- downloadHandler(
      filename = function() {
        paste("KRMshp.csv", sep = "")
      },
      content = function(file) {
        write.csv(gshp(), file, row.names = FALSE)
      }
    )

    #create the KRM shape plot
    ppshp = observeEvent(input$pshp,{
      output$krmshp <- renderPlot({
        shpdf=gshp()
        if("x_sb" %in% names(shpdf)){
          layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
          shplot(x_fb = shpdf$x_fb, w_fb = shpdf$w_fb,
                       x_sb = shpdf$x_sb, w_sb = shpdf$w_sb,
                       z_fbU = shpdf$z_fbU, z_fbL = shpdf$z_fbL,
                       z_sbU = shpdf$z_sbU, z_sbL = shpdf$z_sbL)
        }else if("x_sb2" %in% names(shpdf)){
          layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
          shplot(x_fb = shpdf$x_fb, w_fb = shpdf$w_fb,
                       x_sb = shpdf$x_sb, w_sb = shpdf$w_sb,
                       x_sb2 = shpdf$x_sb2, w_sb2 = shpdf$w_sb2,
                       z_fbU = shpdf$z_fbU, z_fbL = shpdf$z_fbL,
                       z_sbU = shpdf$z_sbU, z_sbL = shpdf$z_sbL,
                       z_sbU2 = shpdf$z_sbU2, z_sbL2 = shpdf$z_sbL2)
        }else{
          layout(matrix(c(1,2,3,3), 2, 2, byrow = TRUE))
          shplot(x_fb = shpdf$x_fb, w_fb = shpdf$w_fb,
                       x_sb = NULL, w_sb = NULL,
                       z_fbU = shpdf$z_fbU, z_fbL = shpdf$z_fbL,
                       z_sbU = NULL, z_sbL = NULL)
        }

      })
    })

    output$krmshp <- renderPlot({ppshp})

    #############################################################################
    #############################################################################
    #plot 3D file
    observeEvent(input$shpfile,{
      inFile <- input$shpfile

      if (is.null(inFile))
        return(NULL)

      output$shp3D = renderPlotly(get_shp3d(inFile$datapath))

    })

    # RUN KRM
    tsvalues <- shiny::reactiveValues()

    observeEvent(input$rKRM,{
      print("Running KRM")
      showNotification("Running KRM simulation")
      inFile <- input$shpfile

      if (is.null(inFile))
        return(NULL)

      shpdf = read.csv(inFile$datapath)
      for (v in c("x_fb", "x_sb","w_fb","w_sb","z_fbU","z_fbL","z_sbU","z_sbL")){
        assign(v, if(v %in% names(shpdf)){shpdf[,v]}else{NULL})
      }
      TS =krm.sim(frequency = seq(input$freq_min, input$freq_max,length.out=input$freq_n) * 1000,
                        c.w = input$cw,
                        rho.w = input$rhow,
                        theta=seq(input$theta_min, input$theta_max,length.out=input$theta_n),
                        c.fb = input$cfb,
                        c.sb = input$csb,
                        rho.sb = input$rhosb,
                        rho.fb = input$rhofb,
                        L=seq(input$L_min, input$L_max,length.out=input$L_n),
                        x_fb = x_fb[is.na(x_fb)==F],
                        x_sb = x_sb[is.na(x_sb)==F],
                        w_fb = w_fb[is.na(w_fb)==F],
                        w_sb = w_sb[is.na(w_sb)==F],
                        z_fbU = z_fbU[is.na(z_fbU)==F],
                        z_fbL = z_fbL[is.na(z_fbL)==F],
                        z_sbU = z_sbU[is.na(z_sbU)==F],
                        z_sbL = z_sbL[is.na(z_sbL)==F],
                        modsb = input$modsb,
                        modsb2 = input$modsb2)
      tsvalues$TS=TS
      showNotification("KRM simulation completed!")
      output$TStable <- DT::renderDataTable(TS)

      updateSelectInput(session, "thetas",
                        label = "Theta:",
                        choices = unique(TS$theta),
                        selected=unique(TS$theta)[1])

      updateSelectInput(session, "Ls",
                        label = "L:",
                        choices = unique(TS$L),
                        selected=unique(TS$L)[1])

    })

    observeEvent(tsvalues$TS,{
      output$krmplot = renderPlotly(
        ggplotly(ggplot(data=tsvalues$TS%>%filter(L %in% input$Ls),aes(x=frequency/1e3, y=theta, fill=TS))+
                   geom_tile()+
                   facet_wrap(.~L)+
                   xlab('Frequency (kHz)')+
                   ylab(' Theta (°)')+
                   scale_fill_gradientn(colours = rev(pals::brewer.rdylbu(15)), limits=c(input$clow,input$cup), oob=scales::squish)+
                   theme_classic()))
    })


    observeEvent(tsvalues$TS,{
      output$krmfreqplot = renderPlotly(
        ggplotly(ggplot(data=tsvalues$TS%>%filter(theta %in% input$thetas, L %in% input$Ls),
                        aes(x=frequency/1e3, y=TS, col=interaction(L,theta), group=interaction(L,theta)))+
                   geom_line()+
                   scale_color_discrete(name='Length (m).Theta (°)')+
                   xlab('Frequency (kHz)')+
                   ylab(' Theta (°)')+
                   theme_classic()))
    })

    # Export data table
    output$exportTS <- shiny::downloadHandler(
      filename = function(){
        paste("KRM", ".csv",sep="")
      },
      content = function(file) {
        write.csv(tsvalues$TS, file, row.names=FALSE)
      })
  }
  shinyApp(ui, server)#
}
run_krm()
