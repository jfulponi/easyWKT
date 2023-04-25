library(shiny)
library(leaflet)
library(sf)
library(leaflet.extras)
library(sp)
library(htmltools)
library(clipr)
library(bslib)

theme <- bs_theme(
    bg = "#0b3d91", fg = "white", primary = "#FCC780",
    base_font = font_google("Space Mono"),
    code_font = font_google("Space Mono")
)
bs_theme_update(theme, bg = "#34058E", primary = "#F29415", secondary = "#6898E9", 
                base_font = font_google("Rubik"), `enable-gradients` = TRUE, 
                fg = "#FFFFFF")


obj_to_wkt <- function(obj) {
    coords_list <- lapply(obj$geometry$coordinates[[1]], function(pt) {
        c(pt[[1]], pt[[2]])
    })
    
    coords_matrix <- do.call(rbind, coords_list)
    
    if (obj$geometry$type == "Polygon") {
        sf_obj <- st_polygon(list(coords_matrix))
    } else if (obj$geometry$type == "LineString") {
        sf_obj <- st_linestring(coords_matrix)
    } else if (obj$geometry$type == "Point") {
        sf_obj <- st_point(coords_matrix[1,])
    } else {
        stop("Tipo de geometría no soportado")
    }
    
    wkt <- st_as_text(sf_obj)
    return(wkt)
}

ui <- fluidPage(
    theme = theme,
    titlePanel("Easy WKT, v. 0.1"),
    sidebarLayout(
        sidebarPanel(
            img(src = "https://datapartnership.org/images/logo-title.png",
                width="100%")
        ),
        mainPanel(
            leafletOutput("mymap"),
            verbatimTextOutput("wkt_output"),
            br(),
            actionButton("copy_wkt", "Copy WKT"),
            actionButton("clear_wkt", "Erase WKT"),
            downloadButton("downloadData", "Get GeoJSON")
        )
    )
)


# Define server logic 
server <- function(input, output) {
    
    output$mymap <- renderLeaflet(
        leaflet() %>%
            addProviderTiles(providers$CartoDB.Voyager) %>%
            setView(lng = -58, lat = -34, zoom = 5) %>%
            addDrawToolbar(
                targetGroup='draw', polylineOptions = FALSE,
                circleOptions = F, markerOptions = F, circleMarkerOptions = F, singleFeature = T,
                editOptions = editToolbarOptions(selectedPathOptions = selectedPathOptions()))  
    )
    
    observeEvent(input$mymap_draw_new_feature,{
        feature <- input$mymap_draw_new_feature
        
        output$wkt_output <- renderPrint({
            obj_to_wkt(feature)
        })
        observeEvent(input$copy_wkt, {
            wkt <- obj_to_wkt(feature)
            if (length(wkt) > 0) {
                wkt_text <- paste(wkt, collapse = "\n")
                write_clip(wkt_text)
            }
        })
        
        output$downloadData <- downloadHandler(
            filename = function() {
                paste("Shape-", Sys.time(), ".geojson", sep="")
            },
            content = function(file) {
                data.frame(type = "polygon", 
                           wkt = obj_to_wkt(feature)) %>% 
                    st_as_sf(wkt = "wkt") %>% st_write(file)            }
        )
        
    })
    
    observeEvent(input$clear_wkt, {
        output$wkt_output <- NULL
        leafletProxy("mymap") %>%
            clearShapes()
        
    }
    )
    
}

# Run the application
shinyApp(ui = ui, server = server)