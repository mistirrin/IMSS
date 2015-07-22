library(shiny)
shinyUI(
  pageWithSidebar(
    headerPanel("Mexico: Number of Insured Persons VS Age"),
    
    
    sidebarPanel(
      ##SELECT TYPE
      radioButtons("type", "Type:",
                   c("By Sex" = "sex",
                     "By Economic Sector" = "sector")),
      
      ##STATE
      selectInput("estado", "State:",
                  c("AGUASCALIENTES", "BAJA CALIFORNIA NORTE", "BAJA CALIFORNIA SUR", 
                    "CAMPECHE", "COAHUILA", "COLIMA", "CHIAPAS", "CHIHUAHUA", "DURANGO", 
                    "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", rep("ESTADO DE MEXICO",2), 
                    "MICHOACAN", "MORELOS",  "NAYARIT", "NUEVO LEON", "OAXACA", "PUEBLA", 
                    "QUERETARO", "QUINTANA ROO", "SAN LUIS POTOSI", "SINALOA", "SONORA", 
                    "TABASCO", "TAMAULIPAS", "TLAXCALA", rep("VERACRUZ", 2), "YUCATAN", 
                    "ZACATECAS", rep("DISTRITO FEDERAL", 6)
                  )
                ),
      
      ##BY SEX
      conditionalPanel(
        condition = "input.type == 'sex'",
        selectInput("sector", "Sector:", c(
          "AGRICULTURE, LIVESTOCK, FORESTRY, FISHING AND HUNTING",
          "EXTRACTIVE INDUSTRIES",
          "PROCESSING INDUSTRIES",
          "CONSTRUCTION INDUSTRIES",
          "ELECTRICITY SUPPLY AND WATER COLLECTION TREATMENT AND SUPPLY",
          "COMMERCE",
          "TRANSPORTS AND COMMUNICATIONS",
          "SERVICES FOR BUSINESS, INDIVIDUAL AND HOME",
          "SOCIAL AND COMMUNITY SERVICES")
        )
      ),

      ##BY SECTOR
      conditionalPanel(
        condition = "input.type == 'sector'",
        tags$label("Sex:", class = "control-label"),
        p('Male and Female')
      ),
      
      ##INSTRUCTIONS
      h2("Instructions"),
      p("The purpose of this application is to graphically represent
        the data retrieved from ",
      tags$a(href="http://busca.datos.gob.mx/#/conjuntos/asegurados-en-el-imss", "here"), "."),
      p("The data displayed is for 2014 for the month of January."),
      p("The charts will indicate the number of insured people by age, the user can then choose to display the data in one of two ways:"),
      tags$ul(
        tags$li(tags$b("By sex:"), tags$span("The user can then choose to display the data for a specific state or a specific economic sector.")),
        tags$li(tags$b("By economic sector:"), tags$span("The user can then choose to display the data for a specific economic sector."))
      )
    ),
    
    mainPanel(
      plotOutput(outputId = 'oPlot')
    )
  )
)