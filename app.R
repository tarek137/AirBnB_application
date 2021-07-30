#
# This is an Airbnb web application. You can run the application by clicking
# the 'Run App' button above.
#
#
#

library(caret)
library(dplyr)
library(DT)
library(ggplot2)
library(shinythemes)
library(tidyr)
library(lubridate)

# Load File
load("C:/Users/TL73/Documents/R/Exam/AirBnB.Rdata")
L$price<-as.numeric(L$price)
listings <- L

# Define UI for the Airbnb application
ui <- fluidPage(
    
    # Set Shiny Theme
    theme = shinytheme("united"),

    # Application title
    titlePanel("Airbnb PriceR: Airbnb Analytics", windowTitle = "Airbnb Data for Hosts and Guests"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "neighbourhood", 
                        label = "Neighbourhood",
                        choices = c("Batignolles", "Champs-Elysées", "Ternes",
                                    "Monceau", "Saint-Lazare", "XVII Arrondissement", "Porte de Clignancourt",
                                    "Montparnasse", "Montmartre", "Passy", "Tour Eiffel - Champ de Mars",
                                    "Invalides - Ecole Militaire", "Saint-Germain-des-Prés - Odéon", "Louvre - Tuileries",
                                    "Tour Eiffel - Champs de Mars", "VI Arrondissement", "La Villette",
                                    "Buttes-Chaumont - Belleville", "La Chapelle", "X Arrondissement",
                                    "Cannes", "XIX Arrondissement", "Gare du Nord - Gare de I'Est", "Pigalle - Saint-Georges", "Opéra - Grands Boulevards",
                                    "Madeleine - Vendôme", "II Arrondissement", "IX Arrondissement", "République","Canal Saint-Martin","Enclos-St-Laurent (X Arrondissement)",
                                    "III Arrondissement", "Austerlitz", "Place d'Italie - Quartier Chinois","Port-Royal","Bercy","Alésia","Panthéon","Porte de Versailles",
                                    "XIII Arrondissement","Vaugirard","Commerce - Dupleix","XV Arrondissement","Nation","Bastille","Gare de Lyon","XI Arrondissement",
                                    "Châtelet - Les Halles - Beaubourg","Palais Royal","Notre Dame - Ile de La Cité","I Arrondissement","Saint-Paul - Ile Saint-Louis",
                                    "Saint-Michel","Le Marais","Temple (III Arrondissement)","Hotel-de-Ville (IV Arrondissement)","Père Lachaise - Ménilmontant",
                                    "Auteuil","XVI Arrondissement","XIV Arrondissement","Popincourt (XI Arrondissement)","IV Arrondissement","Bourse (II Arrondissement)"
                                    )),
            sliderInput("rooms",
                        "Number of Rooms",
                        min = 1,
                        max = 10,
                        value = 1),
            sliderInput("accommodates",
                        "Number of Guests",
                        min = 1,
                        max = 20,
                        value = 2),
            selectInput(inputId = "room_types_input", 
                        label = "Room Type",
                        choices = c("All types",
                                    "Entire home/apt",
                                    "Private room",
                                    "Shared room",
                                    "Hotel room"
                        )), width = 4
        ),

        # Output of Shiny pp
        mainPanel(
                        tabPanel("Explore Listings",
                                 h4("List of Available Spaces"),
                                 br(),
                                 DT::dataTableOutput("prices")),
                        tabPanel("Features",
                                 h4("Price vs Features"),
                                 p("Find out more about the supply and prices of listings in your neighbourhood"),
                                 br(),
                                 plotOutput("property"),
                                 br(),
                                 plotOutput("bed"),
                                 br(),
                                 plotOutput("sqft"),
                                 br(),
                                 plotOutput('rating'),
                                 br()),
                        tabPanel("Unique Analysis",
                                 h4("Owner, City Renting & Visit Frequency"),
                                 br(),
                                 plotOutput("owner"),
                                 br(),
                                 plotOutput("renting"),
                                 br(),
                                 plotOutput("visitfreq"),
                                 br(),
                                 )
                                 )
                        
            
        )
    )


# Define server logic
server <- function(input, output) {
    # Data Table Output
    output$prices <- renderDataTable({
        if (input$room_types_input == "All types"){
            listings_table <- listings %>% 
                dplyr::filter(neighbourhood == input$neighbourhood,
                              bedrooms == input$rooms,
                              accommodates == input$accommodates) %>%
                select("Price (£)" = price,
                       "Property Type" = property_type,
                       "Review" = review_scores_rating)
            DT::datatable(listings_table,
                          rownames = FALSE,
                          options = list(pageLength = 8,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE)
        } else {
            listings_table <- listings %>% 
                dplyr::filter(neighbourhood == input$neighbourhood,
                              bedrooms == input$rooms,
                              accommodates == input$accommodates,
                              room_type == input$room_types_input) %>%
                select("Price (£)" = price,
                       "Property Type" = property_type,
                       "Review" = review_scores_rating)
            DT::datatable(listings_table,
                          rownames = FALSE,
                          options = list(pageLength = 8,
                                         columnDefs = list(list(className = 'dt-center', targets = '_all'))),
                          escape = FALSE)
        }
        
    })
    
    
    
    
    # Bar Plot
    output$property <- renderPlot ({
        data_sub10 <- listings %>% dplyr::filter(neighbourhood == input$neighbourhood)
        
        data_sub10 %>%
            select(property_type, accommodates,price) %>%
            group_by(property_type, accommodates) %>%
            summarise(Avg=mean(as.numeric(price))) %>%
            ggplot(aes(fill=property_type, y=Avg, x=factor(accommodates))) +
            geom_bar(position ="stack", stat="identity", alpha=0.7) +
            scale_fill_manual(name = "Room Type", 
                              values = c("red", "lightblue", "orange", "grey", "blue","green","black","yellow")) +
            labs(x= "Number of Guests Listing can Accommodate", y = "Average Prices",
                 title = "Property price with guests sccomodation") +
            theme_minimal() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))
    })
    
    # Bed Type
    output$bed <- renderPlot ({
        
        data_sub2 <- listings %>% dplyr::filter(neighbourhood == input$neighbourhood)
        
        data_sub2 %>%
            select(bed_type, accommodates,price) %>%
            group_by(bed_type, accommodates) %>%
            summarise(Avg=mean(as.numeric(price))) %>%
            ggplot(aes(fill=bed_type, y=Avg, x=factor(accommodates))) +
            geom_bar(position ="stack", stat="identity", alpha=0.7) +
            scale_fill_manual(name = "Bed Type", 
                              values = c("red", "lightblue", "orange", "grey", "blue")) +
            labs(x= "Number of Guests Listing can Accommodate", y = "Average Prices",
                 title = "Bed price with guests sccomodation") +
            theme_minimal() +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))
    })
    
    
    # Square Footage
    output$sqft <- renderPlot ({
        
        data_sub4 <- listings %>% dplyr::filter(neighbourhood == input$neighbourhood,
                                                accommodates == input$accommodates)
        
        data_sub4 %>%
            select(square_feet,price) %>%
            ggplot(aes(y=as.numeric(price), x=square_feet)) +
            geom_point() +labs(x= "Room Size", y = "Prices",
                 title = "Price vs Room Size")
    })
    
    # Rating
    output$rating <- renderPlot ({
        
        data_sub5 <- listings %>% dplyr::filter(neighbourhood == input$neighbourhood,
                                                accommodates == input$accommodates)
        
        data_sub5 %>%
            select(review_scores_rating,price) %>%
            ggplot(aes(y=as.numeric(price), x=review_scores_rating)) +
            geom_point() +labs(x= "Room Size", y = "Rating",
                               title = "Price vs Rating")
    })
    

    
    # Bar Plot
    output$owner <- renderPlot ({
        data_sub6 <- listings %>% dplyr::filter(neighbourhood == input$neighbourhood)
        
        data_sub6 %>%
            select(host_name,price) %>%
            group_by(host_name) %>%
            summarise(Count=n()) %>%arrange(desc(Count))%>%top_n(50)%>%
            ggplot(aes(y=Count, x=reorder(host_name,-Count))) +
            geom_bar(position ="stack", stat="identity", alpha=0.7)+
            scale_fill_manual(name = "Owner Properties") +
            labs(x= "Name of Owners", y = "Count of Properties",
                 title = "Number of properties owned") +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))
    })
    
    
    output$renting <- renderPlot ({
        data_sub7 <- listings %>% dplyr::filter(room_type == input$room_types_input)
        
        data_sub7 %>%
            select(city,price) %>%
            group_by(city) %>%
            summarise(Avg=mean(as.numeric(price))) %>%arrange(desc(Avg))%>%top_n(50)%>%
            ggplot(aes(y=Avg, x=reorder(city,-Avg))) +
            geom_bar(position ="stack", stat="identity", alpha=0.7)+scale_fill_manual(name = "City wise Properties") +
            labs(x= "City", y = "Average Prices",
                 title = "City wise Prices of Properties") +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))
    })
    
    output$visitfreq <- renderPlot ({
        data_sub8 <- R %>% mutate(Qtr=quarter(R$date,with_year=TRUE))
        
        data_sub8 %>%
            group_by(Qtr) %>%
            summarise(Visit=n())%>%
            ggplot(aes(y=Visit, x=Qtr)) +
            geom_bar(position ="stack", stat="identity", alpha=0.7)+
            scale_fill_manual(name = "Count of Visits") +
            labs(x= "Quaterweise Timeline", y = "Visits",
                 title = "Number of visits in each quarter") +
            theme(panel.background = element_rect(fill = "white",
                                                  colour = "black",
                                                  size = 0.5, 
                                                  linetype = "solid"),
                  panel.grid.major.x = element_blank(),
                  panel.grid.major.y = element_line(size = .1, 
                                                    color = "black",
                                                    linetype = "dashed"),
                  panel.grid.minor = element_blank(),
                  legend.position = c(.95, .95),
                  legend.justification = c("right", "top"),
                  legend.box.just = "right",
                  legend.margin = margin(6, 6, 6, 6),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
                  plot.title = element_text(size = 12, margin = margin(b = 10)))
    })
}

# Run the application
# You should not have any R code after this line of code
shinyApp(ui = ui, server = server)
