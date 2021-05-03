meta_regs <- readRDS('meta_regs.RData')

gn250 <- readRDS('Geocodes.RData')

v <- reactiveValues()

server <- function(input, output, session) {


  ## Selectizeinputs ----
  auswahl = meta_regs %>% filter(reg_typ == 1000 | reg_typ == 1000000)
  updateSelectizeInput(session, 'regk1', choices = auswahl$names, server = TRUE)
  auswahl1 = meta_regs %>% filter(reg_typ == 1000 | reg_typ == 1000000)
  updateSelectizeInput(session, 'regk2', choices = auswahl1$names, server = TRUE)



  # User Inputs ----

  ### Other inputs ----

  output$mapchoice1 <- renderUI({
    req(input$clicksk1)
    radioButtons('mchoice1', label = 'Select background map',
                 choices = c('Open Street Map', 'TopPlusOpen  (Color)', 'TopPlusOpen  (Grey)'),
                 selected = 'Open Street Map')
  })

  output$checkbox <- renderUI({
    req(input$clicksk1)
    checkboxInput('radiuslabels', label = 'Labels', value = FALSE)

  })




  output$mapchoice2 <- renderUI({
    req(input$clicksk2)
    radioButtons('mchoice2', label = 'Select background map',
                 choices = c('Open Street Map', 'TopPlusOpen  (Color)', 'TopPlusOpen  (Grey)',
                             'Stamen Terrain', "Orthophoto maps (not for all areas available)"),
                 selected = "Orthophoto maps (not for all areas available)")
  })



  output$radius1 <- renderUI({
    req(input$clicksk1)
    numericInput('radiusk1', label = 'Buffer radius',
                 min = 5, max = 15, value = 10, step = 5)
  })

  output$bezirkliste <- renderUI({
    req(input$clicksk2)
    req(v$regnamek2)
    #browser()
    r = paste0('^',v$regnamek2,'-.')
    bzk_data <- gn250 %>%
      filter(grepl(r, NAME) | grepl(r, NAME))
    selectInput('bzk', label = 'Select districts',
                choices = as.character(bzk_data$NAME), multiple = TRUE)
  })

  output$applybuttonk2 <- renderUI({
    req(input$clicksk2)
    req(input$bzk)
    actionButton(inputId = "applyk2", label = "Apply labels")
  })


  ### Quellen ----

  output$quellek1 <- renderUI({
    req(input$mchoice1)
    if (input$mchoice1 == 'Open Street Map') {
      h5("Source: own calculation; background map: © OpenStreetMap Contributors")
    } else if (input$mchoice1 == 'TopPlusOpen  (Color)') {
      h5("Source: own calculation; background map:  © Bundesamt für Kartographie und Geodäsie 2021, data source: https://sg.geodatenzentrum.de/web_public/data source_TopPlus_Open.pdf")
    } else if (input$mchoice1 == 'TopPlusOpen  (Grey)') {
      h5("Source: own calculation; background map:  © Bundesamt für Kartographie und Geodäsie 2021, data source: https://sg.geodatenzentrum.de/web_public/data source_TopPlus_Open.pdf")
    }
  })

  output$quellek2 <- renderUI({
    req(input$mchoice2)
    req(v$regnamek2)
    ##browser()
    if (input$mchoice2 == 'Open Street Map') {
      h5("Source: own calculation; background map: © OpenStreetMap Contributors")
    } else if (input$mchoice2 == 'TopPlusOpen  (Color)') {
      h5("Source: own calculation; background map:  © Bundesamt für Kartographie und Geodäsie 2021, data source: https://sg.geodatenzentrum.de/web_public/data source_TopPlus_Open.pdf")
    } else if (input$mchoice2 == 'TopPlusOpen  (Grey)') {
      h5("Source: own calculation; background map:  © Bundesamt für Kartographie und Geodäsie 2021, data source: https://sg.geodatenzentrum.de/web_public/data source_TopPlus_Open.pdf")
    } else if (input$mchoice2 == 'Stamen Terrain') {
      h5("Source: own calculation; background map:  Stamen Design, unter CC BY 3.0, data source: © OpenStreetMap Contributors")
    } else if (input$mchoice2 == 'WMS Orthofotos (nicht für alle Länder)') {
      if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Nordrhein-Westfalen') {
        h5("Source: own calculation; background map: Geobasis NRW")
      } else if (gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Hamburg') {
        h5("Source: own calculation; background map: Freie und Hansestadt Hamburg, Landesbetrieb Geoinformation und Vermessung")
      } else if (gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Berlin') {
        h5("Source: own calculation; background map: Geoportal Berlin / Digitale farbige Orthophotos 2020 (DOP20RGB), dl-de-by-2.0, https://www.govdata.de/dl-de/by-2-0")
      } else if (gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen') {
        h5("Source: own calculation; background map: Staatsbetrieb Geobasisinformation und Vermessung Sachsen (GeoSN)")
      } else if (gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen-Anhalt') {
        h5("Source: own calculation; background map: © GeoBasis-DE / LVermGeo LSA, 2021, Es gelten die Nutzungsbedingungen des LVermGeo LSA")
      } else if (gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Thüringen') {
        h5("Source: own calculation; background map: © GDI-Th, dl-de-by-2.0, https://www.govdata.de/dl-de/by-2-0")
      } else {h5("Source: own calculation; background map:  Stamen Design, unter CC BY 3.0, data source: © OpenStreetMap Contributors")}
    }

  })


  # Functions ----

  ### Mapping ----

  dmstodec <- function(dms) {
    dms <- as.character(dms)

    if (nchar(dms) == 5) {
      dms <- paste0('0',dms)
    }

    d <- substr(dms, start = 1, stop = 2)
    m <- substr(dms, start = 3, stop = 4)
    s <- substr(dms, start = 5, stop = 6)
    dms <- paste(d,m,s, sep = ' ')

    dec <- conv_unit(dms, from = 'deg_min_sec', to = 'dec_deg')
    return(as.numeric(dec))
  }

  geocoder <- function(r) {
    ##browser()
    data <- gn250 %>%
      filter(NAME == r)
    if (!is.na(data[r, 'GEMEINDE'])) {
      data <- data %>%
        filter(GEMEINDE == r)
    }
    if (!is.na(data[r, 'VERWGEM'])) {
      data <- data %>%
        filter(VERWGEM == r)
    }
    coords <- c(data[1,'GEOLA'], data[1,'GEOBR'])
    coords <- c(dmstodec(coords[1]), dmstodec(coords[2]))
    return(coords)
  }

  Multiring <- function(x,n,d){
    #observer()
    buffers <- list(); names <- list(); nd <- d
    for (i in 1:n){
      buffers[[i]] <- st_as_sf(st_union(st_buffer(x,nd)))
      buffers[[i]]$ID <- paste0("Buffer ", round(nd/1000,1), " km")
      nd <- nd+d
    }

    jlayers <- function(x){
      if (length(x)==1){
        # if length is == 1 , return 1 layer
        xm <- x[[1]]
      } else {
        for (i in 1:(length(x)-1)){
          if(i==1){xm <- x[[1]]}
          xm <- rbind(xm, x[[i+1]])
        }
      }
      return(xm)
    }

    return(jlayers(buffers))
  }




  # Abbildungen ----

  ### Karte 1 ----

  foundational.map <- reactive({
    req(input$clicksk1)
    req(v$gcodes1)
    req(input$radiusk1)
    ##browser()
    coords <- v$gcodes1
    pnt <- st_point(c(coords[1], coords[2]))
    geom <- st_sfc(pnt, crs = 4326)
    df <- data.frame(name = input$regk1)
    sfobj <- st_sf(df, geometry = geom)
    sfobj <- st_transform(sfobj, crs = 4839)
    radius <- as.numeric(input$radiusk1)*1000
    return <- Multiring(sfobj, 3, radius)
    return2 <- st_transform(return, crs = 4326)
    lat = coords[2]
    lon = coords[1]
    if (input$mchoice1 == 'Open Street Map') {

      leaflet(return2, options = leafletOptions(zoomControl = FALSE,attributionControl=FALSE)) %>%
        addTiles() %>% addPolygons(color = 'red', weight = 2, fillOpacity = 0.1)

    }

    else if (input$mchoice1 == 'TopPlusOpen  (Color)') {

      leaflet(return2, options = leafletOptions(zoomControl = FALSE,attributionControl=FALSE)) %>%
        addWMSTiles("https://sg.geodatenzentrum.de/wms_topplus_open",
                    layers = "web",
                    options = WMSTileOptions(format = "image/png", transparent = F)) %>%
        addPolygons(color = 'red', weight = 2, fillOpacity = 0.2)

    }

    else if (input$mchoice1 == 'TopPlusOpen  (Grey)') {

      leaflet(return2, options = leafletOptions(zoomControl = FALSE,attributionControl=FALSE)) %>%
        addWMSTiles("https://sg.geodatenzentrum.de/wms_topplus_open",
                    layers = "web_grau",
                    options = WMSTileOptions(format = "image/png", transparent = F)) %>%
        addPolygons(color = 'red', weight = 2, fillOpacity = 0.2)
    }
  })

  output$map1 <- renderLeaflet({
    req(v$gcodes1)
    req(input$clicksk1)
    req(input$mchoice1)
    #req(input$radiuslabels)
    coords <- v$gcodes1
    radius <- as.numeric(input$radiusk1)*1000
    lat = coords[2]
    lon = coords[1]
    if (input$radiuslabels)  {
      if (radius == 5000) {
        p1 = 0.04
        p2 = 0.085
        p3 = 0.1325
        foundation.map() %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p1, label = '5 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '20px', style = list("font-weight" = "bold", "font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p2, label = '10 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '20px', style = list("font-weight" = "bold", "font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p3, label = '15 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '20px', style = list("font-weight" = "bold", "font-family" = 'calibri')))
      } else if (radius == 10000) {
        p1 = 0.08
        p2 = 0.17
        p3 = 0.26
        foundational.map() %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p1, label = '10 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold","font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p2, label = '20 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold", "font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p3, label = '30 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold", "font-family" = 'calibri')))
      } else {
        p1 = 0.12
        p2 = 0.26
        p3 = 0.39
        foundational.map() %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p1, label = '15 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold","font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p2, label = '30 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold","font-family" = 'calibri'))) %>%
          addLabelOnlyMarkers(lng = lon, lat = lat + p3, label = '45 Km', labelOptions = labelOptions(noHide = T, textOnly = TRUE, textsize = '18px', style = list("font-weight" = "bold","font-family" = 'calibri')))
      }
    } else {foundational.map()}

  })

  # user.created.map <- reactive({
  #
  #   # call the foundational Leaflet map
  #   foundational.map() %>%
  #
  #     # store the view based on UI
  #     setView( lng = input$map_center$lng
  #              ,  lat = input$map_center$lat
  #              , zoom = input$map_zoom
  #     )
  #
  # }) # end of creating user.created.map()
  #
  # output$dl <- downloadHandler(
  #   filename = paste0( Sys.Date()
  #                      , "_customLeafletmap"
  #                      , ".png"
  #   )
  #
  #   , content = function(file) {
  #     #browser()
  #     mapshot( x = user.created.map()
  #              , file = file
  #              , cliprect = "viewport" # the clipping rectangle matches the height & width from the viewing port
  #              , selfcontained = FALSE # when this was not specified, the function for produced a PDF of two pages: one of the leaflet map, the other a blank page.
  #     )
  #   } # end of content() function
  # ) # end of downloadHandler() function


  ### Karte 2 ----

  karte2 <- reactive({
    req(input$mchoice2)
    coords <- v$gcodes2
    if (input$mchoice2 == 'Open Street Map') {
      leaflet(options = leafletOptions(
        zoomControl = FALSE,attributionControl=FALSE)) %>% addTiles() %>%
        setView(lng = coords[1], lat = coords[2], zoom = 13)
    } else if (input$mchoice2 == 'TopPlusOpen  (Color)') {
      leaflet(options = leafletOptions(
        zoomControl = FALSE,attributionControl=FALSE)) %>%
        #addProviderTiles(providers$Stamen.Terrain) %>%
        addWMSTiles("https://sg.geodatenzentrum.de/wms_topplus_open",
                    layers = "web",
                    options = WMSTileOptions(format = "image/png", transparent = F)) %>%
        setView(lng = coords[1], lat = coords[2], zoom = 13)
    } else if (input$mchoice2 == 'TopPlusOpen  (Grey)') {
      leaflet(options = leafletOptions(
        zoomControl = FALSE,attributionControl=FALSE)) %>%
        #addProviderTiles(providers$Stamen.Terrain) %>%
        addWMSTiles("https://sg.geodatenzentrum.de/wms_topplus_open",
                    layers = "web_grau",
                    options = WMSTileOptions(format = "image/png", transparent = F)) %>%
        setView(lng = coords[1], lat = coords[2], zoom = 13)
    } else if (input$mchoice2 == 'Stamen Terrain') {
      leaflet(options = leafletOptions(
        zoomControl = FALSE,attributionControl=FALSE)) %>%
        addProviderTiles(providers$Stamen.Terrain) %>%
        setView(lng = coords[1], lat = coords[2], zoom = 13)
    } else if (input$mchoice2 == 'WMS Orthofotos (nicht für alle Länder)') {
      if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Nordrhein-Westfalen'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          #addProviderTiles(providers$Stamen.Terrain) %>%
          addWMSTiles("https://www.wms.nrw.de/geobasis/wms_nw_dop",
                      layers = "nw_dop_rgb",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Hamburg'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://geodienste.hamburg.de/HH_WMS_DOP",
                      layers = "DOP_2020_downscale",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Berlin'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://isk.geobasis-bb.de/ows/dop20c_wms?",
                      layers = "bebb_dop20c",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Berlin'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://fbinter.stadt-berlin.de/fb/wms/senstadt/k_luftbild2020_rgb",
                      layers = "0",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://geodienste.sachsen.de/wms_geosn_dop-rgb/guest?REQUEST=GetCapabilities&SERVICE=WMS&VERSION=1.3.0",
                      layers = "sn_dop_020",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen-Anhalt'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://www.geodatenportal.sachsen-anhalt.de/wss/service/ST_LVermGeo_DOP_WMS_OpenData/guest",
                      layers = "lsa_lvermgeo_dop20_2",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen-Anhalt'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://www.geoproxy.geoportal-th.de/geoproxy/services/DOP",
                      layers = "lsa_lvermgeo_dop20_2",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else if(gn250[gn250$NAME == v$regnamek2[1],'BUNDESLAND'][1]=='Sachsen-Anhalt'){
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addWMSTiles("https://www.geoproxy.geoportal-th.de/geoproxy/services/DOP",
                      layers = "th_dop",
                      options = WMSTileOptions(format = "image/png", transparent = F)) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      } else {
        leaflet(options = leafletOptions(
          zoomControl = FALSE,attributionControl=FALSE)) %>%
          addProviderTiles(providers$Stamen.Terrain) %>%
          setView(lng = coords[1], lat = coords[2], zoom = 13)
      }
    }

  })

  output$map2 <- renderLeaflet({
    req(v$gcodes2)
    req(input$clicksk2)
    a <- karte2()
    if (!is.null(input$bzk)) {
      req(input$applyk2)
      Labels <- v$labelsk2
      labels_gcodes <- map(Labels, geocoder)
      if ((input$mchoice2 == 'Open Street Map') || (input$mchoice2 == 'Stamen Terrain')) {
        for (i in 1:length(Labels)) {

          a <- addLabelOnlyMarkers(map = a, lng = labels_gcodes[[i]][1],
                                   lat = labels_gcodes[[i]][2],
                                   label = Labels[i],
                                   labelOptions = labelOptions(
                                     noHide = T, textOnly = TRUE, textsize = '25px', direction = 'bottom',
                                     style = list("font-weight" = "bold", "font-family" = 'calibri', 'color' = 'black')
                                   )
          )
        } } else if ((input$mchoice2 == 'TopPlusOpen  (Color)') || (input$mchoice2 == 'TopPlusOpen  (Grey)')) {
          for (i in 1:length(Labels)) {

            a <- addLabelOnlyMarkers(map = a, lng = labels_gcodes[[i]][1],
                                     lat = labels_gcodes[[i]][2],
                                     label = Labels[i],
                                     labelOptions = labelOptions(
                                       noHide = T, textOnly = TRUE, textsize = '25px', direction = 'bottom',
                                       style = list("font-weight" = "bold", "font-family" = 'calibri', 'color' = 'brown')
                                     )
            )
          } } else if ((input$mchoice2 == 'WMS Orthofotos (nicht für alle Länder)')) {
            for (i in 1:length(Labels)) {
              ##browser()
              a <- addLabelOnlyMarkers(map = a, lng = labels_gcodes[[i]][1],
                                       lat = labels_gcodes[[i]][2],
                                       label = Labels[i],
                                       labelOptions = labelOptions(
                                         noHide = T, textOnly = TRUE, textsize = '25px', direction = 'bottom',
                                         style = list("font-weight" = "bold", "font-family" = 'calibri', 'color' = 'white')
                                       )
              )
            } }

      a
    } else {karte2()}



  })



  ### Submitbuttons ----

  observeEvent(input$clicksk1,
               {

                 #v$regnamek1 <- to_vec(for (i in input$regk1) (word(i,2)))
                 v$regnamek1 <- to_vec(for (i in input$regk1) (sub(".*?\\s+(.*)\\s.*", "\\1", i, perl=TRUE)))
                 v$gcodes1 <- geocoder(v$regnamek1[1])

               }

  )

  observeEvent(input$clicksk2,
               {
                 #v$regnamek2 <- to_vec(for (i in input$regk2) (word(i,2)))
                 v$regnamek2 <- to_vec(for (i in input$regk2) (sub(".*?\\s+(.*)\\s.*", "\\1", i, perl=TRUE)))
                 v$gcodes2 <- geocoder(v$regnamek2[1])

               }

  )


  ### Applybuttons ----

  observeEvent(input$applyk2,
               {
                 v$labelsk2 <- input$bzk

               }

  )



}
