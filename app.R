library(shiny)
library(sp)
library(rgdal)
library(geojsonio,warn.conflicts = FALSE)
library(leaflet)


#setwd("/Users/kbslalith/Desktop/Bhaskar/Final Dashboard")

datax <- read.csv("town_coordinates.csv")
names(datax)

town_latlon <- datax

# From http://leafletjs.com/examples/choropleth/us-states.js
postcodes <- "dublin_postcodes_1.geojson"
states <- rgdal::readOGR(dsn = postcodes,
                         layer = "OGRGeoJSON",
                         require_geomType = "wkbPolygon",
                         encoding = "UTF-8")

bins <- c(200000, 300000, 400000, 500000, 600000, 700000, 800000, 900000, Inf)
pal <- colorBin("YlOrRd", domain = states$avg_price, bins = bins)

labels <- sprintf(
  "<strong>%s</strong><br/>average property price = %g",
  states$description, states$avg_price
) %>% lapply(htmltools::HTML)



# Define UI for dataset viewer app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Dublin House Price Prediction"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Text for providing a caption ----
      # Note: Changes made to the caption in the textInput control
      # are updated in the output area immediately as you type
      textInput(inputId = "area",
                label = "Enter Floor Area of Property (in metres squared):",
                value = "100"),
      
      # Input: Number of Bedrooms----
      sliderInput(inputId = "bed",
                   label = "Number of Bedrooms:",min=1,max=6,
                  value = 2),
      # Input: Number of Bathrooms ----
      sliderInput(inputId = "bath",
                   label = "Number of Bathrooms:",min=1,max=6,
                   value = 2),
     
       # Input: Selector for Dwelling Type ----
      selectInput(inputId = "dwelling",
                  label = "Dwelling Type: ",
                  choices = c("Apartment"=0,
                              "Bungalow"=0.2472,
                              "Detached House"=0.2760,
                              "Duplex"=-0.0161,
                              "End of Terrace House"=0.1428,
                
                              "Semi-Detached House"=0.1963,
                              "Terraced House"=0.1314,
                              "Townhouse"=0.0849)),
      
      # Input: Selector for Postcode ----
      selectInput(inputId = "postcode",
                  label = "Postcode: ",
            choices = c(     "Dublin 1"=0.2143,
                              "Dublin 2"=0.6140,
                              "Dublin 3"=0.1619,
                              "Dublin 4"=0.5007,
                              "Dublin 5"=0.1941,
                              "Dublin 6"=0.1827,
                              "Dublin 6W"=0.2069,
                              "Dublin 7"=0.3490,
                              "Dublin 8"=0.3652,
                              "Dublin 9"=0.1811,
                              "Dublin 10"=-0.2038,
                              "Dublin 11"=-0.1206,
                              "Dublin 12"=0.0968,
                              "Dublin 13"=0.1082,
                              "Dublin 14"=0.4398,
                              "Dublin 15"=-0.1779,
                              "Dublin 16"=0.3310,
                              "Dublin 17"=0.0248,
                              "Dublin 18"=0.8384,
                              "Dublin 20"=0.0400,
                              "Dublin 22"=-0.0751,
                              "Dublin 24"=-0.1193,
                              "Co. Dublin North"=0,
                             "Co. Dublin South"=0.3619,
                            "Co. Dublin West"=0.0171),selected = "Dublin 1"),
      #OutPut Of Towns
      uiOutput("ui"),
    
      
      # Input: Selector for Side of city ----
     # radioButtons(inputId = "north_south",
      #            label = "Side of City: ",
       #           choices = c("North"=0,
        #                      "South"=-0.4082428)),

      
      
      # Input: Selector for BER ----
      selectInput(inputId = "ber",
                  label = "BER : ",
                  choices = c("A"=0.0554,
                              
                              
                              "B"=0.0527,
                             
                             
                              "C"=0.0145,
                              
                           
                              "D"=0.0038,
                             
                              "E"=0.0052,
                             
                              "F"=0.0006,
                              "G"=-0.0131,
                              "SI"=0.0400)),
      
      #CHECKBOX For Description
      checkboxGroupInput("descrip",
                         "Property Descriptions (select relevant choices):",
                         choices = c("attic"=0.0036,
                                     "study"=0.0250,
                                     "garage"=0.0746,
                                     "shed"=0.0167,
                                     "sunroom"=0.0031,
                                     "conservatory"=0.0129,
                                     "walk-in wardrobe"=-0.0433,
                                     "garden"=0.0221,
                                     
                                     "showhouse condition"=0.0210,
                                     "refurbished"=0.0281,
                                     "new build"=0.0658,
                                     "requires renovation"=-0.0381,
                                     "auction for sale"=-0.0168
      ))
      # multiple =TRUE),
      

    ),
    
    # Main panel for displaying outputs ----
    mainPanel(

      tags$head(tags$style("
                  #container * {  
                           display: inline-block;
                           vertical-align: middle;
                           line-height: normal;
                           
                           }")),
      
      # Output: Formatted text for caption ----
      h4("Predicted Asking Price Of The Property", align = "center"),
      hr(),

      div(id="container",h1('€'),h1(textOutput("summary")), align = "middle"),
      br(),
#      h3(textOutput("change")),
      
      br(),
      
      h4("Predicted Sale Price Of The Property", align = "center"),
      
      hr(),
      
      div(id="container",h1('€'), h1(textOutput("summary_sale")),align="middle"),
      
      br(),
#      h3(textOutput("change"))

      h4("Choropleth Map of Average Asking Price by Postcode", align = "center"),
      leafletOutput("mymap")

    )
  )
)




#############################################################################

#############################################################################



# Define server logic to summarize and view selected dataset ----
server <- function(input, output) {

  output$ui <- renderUI({
    if (is.null(input$postcode))
      return()
    switch(input$postcode,
           #Co. Dublin North
           '0' = selectInput(inputId = "town",
                                    label = "Town: ",
                                    choices = c(
                                      "Balbriggan" = -0.2785,
                                      "Ballyboughal" = -0.0886,
                                      "Donabate" = 0.0283,
                                      "Garristown" = -0.3773,
                                      "Kinsealy" = -0.0238,
                                      "Lusk" = -0.2158,
                                      "Malahide" = 0.4341,
                                      "Naul" = -0.3033,
                                      "Portmarnock" = 0.3196,
                                      "Rush" = -0.1568,
                                      "Skerries" = 0.1353,
                                      "Swords" = -0.0029,
                                      "town_Co. DublinNorth"=0.0006
                                    
                                      
                                      ),selected = "Balbriggan"),
         
           #Co. Dublin South
           '0.3619' = selectInput(inputId = "town",
                             label = "Town: ",
                             choices = c(
                               "Ballybrack" = -0.1491,
                               "Blackrock" = 0.2549,
                               "Booterstown" = 0.2929,
                               "Dalkey" = 0.4227,
                               "Dun Laoghaire" = 0.1243,
                               "Glasthule" = 0.2735,
                               "Glenageary" = 0.2686,
                               "Killiney" = 0.1517,
                               "Monkstown" = 0.2831,
                               "Mount Merrion" = 0.3816,
                               "Rathmichael" = -0.1383,
                               "Sandycove" = 0.5716,
                               "Shankill" = -0.474,
                               "Stillorgan" = 0.1593,
                               "town_Co. DublinSouth"=0.0007
                             ),selected = "Ballybrack"),
           
           #Co. Dublin West
           '0.0171' = selectInput(inputId = "town",
                             label = "Town: ",
                             choices = c(
                               "Lucan" = -0.0171,
                               "Newcastle" = -0.0977,
                               "Citywest" = -0.0531,
                               
                               
                               "Rathcoole" = -0.1508,
                               "Saggart" = -0.0042,
                               "town_Co. DublinWest"=0.00003
                             ),selected = "Lucan"),
           
           #Dublin 1
           '0.2143' = selectInput(inputId = "town",
                             label = "Town: ",
                             choices = c(
                               "Ifsc" = 0.3247,
                               "North Circular Road" = -0.1376,
                               "North City Centre" = -0.0848,
                               "Smithfield" = 0.087,
                               "town_Dublin 1" = 0.0593
                               ),selected = "Ifsc"),
           
           #Dublin 10
           '-0.2038' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Ballyfermot" = 0.0334,
                                        "Cherry Orchard"=0.0002
                                      ),selected = "Ballyfermot"),
           
           #Dublin 11
           '-0.1206' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Ballymun" = -0.3735,
                                        "Finglas" = -0.0416,
                                        "Glasnevin" = 0.3687
                                      ),selected = "Ballymun"),
          
           #Dublin 12
            '0.0968' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Crumlin" = 0.0423,
                                        "Drimnagh" = -0.0199,
                                        "Kimmage" = 0.1996,
                                        "Park West" = -0.1512,
                                        "town_Dublin 12" = -0.1013,
                                        "Walkinstown"=0.00001
                                      ),selected = "Crumlin"),
 
           #Dublin 13
           '0.1082' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Baldoyle" = 0.0555,
                                       "Balgriffin" = -0.1078,
                                       "Clarehall" = -0.0783,
                                       "Clongriffin" = -0.1231,
                                       "Donaghmede" = -0.0907,
                                       "Howth" = 0.5423,
                                       "Sutton" = 0.3736,
                                       "town_Dublin 13" = -0.1327
                                     ),selected = "Baldoyle"),
           
           #Dublin 14
           '0.4398' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Churchtown" = 0.0772,
                                        "Clonskeagh" = 0.2403,
                                        "Dundrum" = 0.1464,
                                        "Goatstown" = 0.1487,
                                        "Rathfarnham"=0.0004
                                      ),selected = "Churchtown"), 
           
           #Dublin 15
           '-0.1779' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Ashtown" = 0.3656,
                                       "Blanchardstown" = 0.1402,
                                       "Castleknock" = 0.5513,
                                       "Clonee" = 0.0935,
                                       "Clonsilla" = 0.1147,
                                       "Hollystown" = 0.0079,
                                       "Ongar" = -0.0054,
                                       "town_Dublin 15" = -0.0426,
                                       "Tyrrelstown"=0.0009
                                     ),selected = "Ashtown"),
           
           #Dublin 16
           '0.331' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Ballinteer" = 0.0022,
                                       "Dundrum" = 0.1464,
                                       "Knocklyon" = -0.0353,
                                       "Rathfarnham"=0.0004
                                     ),selected = "Ballinteer"),
           
           #Dublin 17
           '0.0248' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Clarehall" = -0.0783,
                                       "Clonshaugh" = -0.1037,
                                       "Coolock" = -0.1134
                                     ),selected = "Clarehall"),
           
           #Dublin 18
           '0.8384' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Cabinteely" = -0.4683,
                                        "Carrickmines" = -0.4957,
                                        "Foxrock" = -0.2633,
                                        "Kilternan" = -0.4477,
                                        "Leopardstown" = -0.4797,
                                        "Sandyford" = -0.4923,
                                        "Shankill" = -0.474,
                                        "Stepaside" = -0.5755
                                      ),selected = "Carrickmines"),
           
           #Dublin 2
           '0.614' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Grand Canal Dock" = 0.1896,
                                        "South City Centre" = 0.0181,
                                        "town_Dublin 2" = -0.0012
                                      ),selected = "Grand Canal Dock"),
           
           #Dublin 20
           '0.04' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Chapelizod" = 0.2706,
                                        "Palmerstown"=0.0003
                                      ),selected = "Chapelizod"),
           
           #Dublin 22
           '-0.0751' = selectInput(inputId = "town",
                                    label = "Town: ",
                                    choices = c(
                                      "Clondalkin" = -0.0839,
                                      "Kingswood" = 0.1408
                                    ),selected = "Clondalkin"),
           
           #Dublin 24
           '-0.1193' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Ballycullen" = 0.1964,
                                        "Firhouse" = 0.1664,
                                        "Kilnamanagh" = -0.0367,
                                        "Kingswood" = 0.1408,
                                        "Tallaght"=0.0005
                                      ),selected = "Ballycullen"),
           
           
           #Dublin 3
           '0.1619' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Clontarf" = 0.4085,
                                        "Donnycarney" = 0.1194,
                                        "Drumcondra" = 0.2192,
                                        "East Wall" = 0.1288,
                                        "Fairview" = 0.0744,
                                        "Marino" = 0.2468,
                                        "North Strand" = -0.0972
                                      ),selected = "Clontarf"),
           
           
           #Dublin 4
           '0.5007' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Ballsbridge" = 0.4238,
                                        "Donnybrook" = 0.3333,
                                        "Grand Canal Dock" = 0.1896,
                                        "Ringsend" = 0.0415,
                                        "Sandymount" = 0.3576,
                                        "town_Dublin 4" = 0.3112
                                      ),selected = "Ballsbridge"),
           
           
           #Dublin 5
           '0.1941' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Artane" = -0.0121,
                                       "Coolock" = -0.1134,
                                       "Donnycarney" = 0.1194,
                                       "Kilbarrack" = -0.1317,
                                       "Killester" = 0.1934,
                                       "Kilmore" = -0.2964,
                                       "Raheny" = 0.1246
                                     ),selected = "Artane"),
           
           #Dublin 6
           '0.1827' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Dartry" = 0.6227,
                                       "Harold'S Cross" = 0.3115,
                                       "Milltown" = 0.5389,
                                       "Ranelagh" = 0.6723,
                                       "Rathgar" = 0.5023,
                                       "Rathmines" = 0.4691,
                                       "Terenure" = 0.3423
                                     ),selected = "Harold'S Cross"),
           
           
           #Dublin 6W
           '0.2069' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Harold'S Cross" = 0.3115,
                                       "Kimmage" = 0.1996,
                                       "Templeogue" = 0.1903,
                                       "Terenure" = 0.3423
                                     ),selected = "Kimmage"),
           
           
           #Dublin 7
           '0.349' = selectInput(inputId = "town",
                                     label = "Town: ",
                                     choices = c(
                                       "Arbour Hill"=0.0001,
                                       "Cabra" = -0.1146,
                                       "Navan Road (D7)" = -0.0298,
                                       "North Circular Road" = -0.1376,
                                       "North City Centre" = -0.0848,
                                       "Phibsborough" = 0.0278,
                                       "Smithfield" = 0.087,
                                       "Stoneybatter" = 0.1065,
                                       "town_Dublin 7" = -0.0483
                                     ),selected = "Arbour Hill"),
           
           
           #Dublin 8
           '0.3652' = selectInput(inputId = "town",
                                      label = "Town: ",
                                      choices = c(
                                        "Christchurch" = 0.0115,
                                        "Cork Street" = -0.0337,
                                        "Inchicore" = -0.1808,
                                        "Islandbridge" = -0.0061,
                                        "Kilmainham" = -0.0284,
                                        "Portobello" = 0.2895,
                                        "Rialto" = 0.0151,
                                        "South Circular Road" = 0.112,
                                        "South City Centre" = 0.0181,
                                        "The Coombe" = -0.0798,
                                        "town_Dublin 8"=0.0008
                                      ),selected = "Christchurch"),
           
           #Dublin 9
           '0.1811' = selectInput(inputId = "town",
                                  label = "Town: ",
                                  choices = c(
                                    "Ballymun" = -0.3735,
                                    "Beaumont" = 0.0018,
                                    "Donnycarney" = 0.1194,
                                    "Drumcondra" = 0.2192,
                                    "Glasnevin" = 0.3687,
                                    "Santry" = -0.0892,
                                    "Whitehall"=0.00002
                                  ),selected = "Beaumont")
           
           )
    
  })
  
  
  predicted_price <- (reactive({floor(exp(12.0200+as.numeric(input$area)*0.004425+((as.numeric(input$bed)-3.01)*0.0532)+(((as.numeric(input$bath)-2.01)*0.0225)+(((as.numeric(input$bath)-2.01)**2)*-0.0225))+as.numeric(input$dwelling)+as.numeric(input$postcode)+as.numeric(input$town)+sum(as.numeric(input$descrip))+as.numeric(input$ber)))}))

    predicted_price_sale <- (reactive({floor(25150+(floor(exp(12.0200+as.numeric(input$area)*0.004425+((as.numeric(input$bed)-3.01)*0.0532)+(((as.numeric(input$bath)-2.01)*0.0225)+(((as.numeric(input$bath)-2.01)**2)*-0.0225))+as.numeric(input$dwelling)+as.numeric(input$postcode)+as.numeric(input$town)+sum(as.numeric(input$descrip))+as.numeric(input$ber)))*0.9651))}))

  
  typeof(predicted_price_sale)
  output$summary <- predicted_price
  output$summary_sale <- predicted_price_sale
  
  ####NEED TO FIGURE THIS OUT####
  vals <- reactiveValues()
  observe({
    vals$town <- input$town
    
  })
  #town <- town_latlon[town_latlon$id == vals$town,]
  # town <- towns[towns$Town == reactive({floor(input$town)}),]
  # town <- towns[towns$Town == reactive({(input$town)}),]
  
  output$mymap <- renderLeaflet({
    
     town <- town_latlon[town_latlon$id == vals$town,]
     town_lon <- town$longitude[1]
     town_lat <- town$latitude[1]
     leaflet(states) %>%
      setView(town_lon, town_lat, 10) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(
        fillColor = ~pal(avg_price),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 5,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = labels,
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto")) %>%
      addMarkers(~town$longitude, ~town$latitude, popup = ~as.character(town$Town), label = ~as.character(town$Town)) %>%
      addLegend(pal = pal, values = ~avg_price, opacity = 0.7, title = NULL,
                position = "bottomright")

  })    
}


# Create Shiny app ----
shinyApp(ui, server)


