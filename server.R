library(shiny);
library(dplyr);
library(ggplot2);
library(scales);

#SET WD
#**setwd("C:/Users/Daniel/Desktop/Data Science/Coursera/Developing Data Products/Course Project")

##ALL DATA GATHERED FROM HERE:
##http://busca.datos.gob.mx/#/conjuntos/asegurados-en-el-imss

##LOAD DATA
##Data gathered from 
## http://busca.datos.gob.mx/#/conjuntos/asegurados-en-el-imss
# data <- read.csv(
#     "Data/31-ene-14.csv", 
#     sep =  "|", 
#     na.strings = c("NA", ""), 
#     colClasses=c(rep('factor', 11), rep('numeric', 18)),
#     encoding = "UTF-8"
#   )
# 
# colnames(data)[1] <- "cve_delegacion"
# 
# #ESTADOS
# ## FROM http://datos.imss.gob.mx/sites/default/files/catalogo-de-archivos-asegurados.pdf
# delegaciones <- data.frame(cve_delegacion = as.factor(c(1:8, 10:40)), 
#                            estado = c("AGUASCALIENTES", "BAJA CALIFORNIA NORTE", "BAJA CALIFORNIA SUR", 
#                                        "CAMPECHE", "COAHUILA", "COLIMA", "CHIAPAS", "CHIHUAHUA", "DURANGO", 
#                                        "GUANAJUATO", "GUERRERO", "HIDALGO", "JALISCO", rep("ESTADO DE MEXICO",2), 
#                                        "MICHOACAN", "MORELOS",  "NAYARIT", "NUEVO LEON", "OAXACA", "PUEBLA", 
#                                        "QUERETARO", "QUINTANA ROO", "SAN LUIS POTOSI", "SINALOA", "SONORA", 
#                                        "TABASCO", "TAMAULIPAS", "TLAXCALA", rep("VERACRUZ", 2), "YUCATAN", 
#                                        "ZACATECAS", rep("DISTRITO FEDERAL", 6)
#                            )
# )
# 
#Merge States
# gData <- data %>% inner_join(delegaciones)
# 
# #AGE RANGE
# ## FROM http://datos.imss.gob.mx/dataset/glosario-asegurados
# edades <- data.frame(rango_edad = paste("E", 1:14, sep = ""),
#                      edad = c("< 15", "15 - 19", "20 - 24", 
#                               "25 - 29", "30 - 34", "35 - 39",
#                               "40 - 44", "45 - 49", "50 - 54",
#                               "55 - 59", "60 - 64", "65 - 69",
#                               "70 - 74", "> 75")
#                      )
# #Merge Ages
# gData <- gData %>% inner_join(edades)
# gData$edad <- factor(gData$edad, levels = edades$edad)
# 
# #ECONOMIC SECTOR
##SPANISH
## sector <- data.frame(sector_economico_1 = factor(c(0,1, 3:9)),
##                      sector_economico = c(
##                        "AGRICULTURA, GANADERIA, SILVICULTURA, PESCA Y CAZA",
##                        "INDUSTRIAS EXTRACTIVAS",
##                        "INDUSTRIAS DE TRANSFORMACION",
##                        "INDUSTRIA DE LA CONSTRUCCION",
##                        "IND.ELECTRICA Y CAPTACION Y SUMINISTRO DE AGUA POTABLE",
##                        "COMERCIO",
##                        "TRANSPORTES Y COMUNICACIONES",
##                        "SERVICIOS PARA EMPRESAS, PERSONAS Y EL HOGAR",
##                        "SERVICIOS SOCIALES Y COMUNALES"
##                        )
##                      )
##ENGLISH
## sector <- data.frame(sector_economico_1 = factor(c(0,1, 3:9)),
##                      sector_economico = c(
##                        "AGRICULTURE, LIVESTOCK, FORESTRY, FISHING AND HUNTING",
##                        "EXTRACTIVE INDUSTRIES",
##                        "PROCESSING INDUSTRIES",
##                        "CONSTRUCTION INDUSTRIES",
##                        "ELECTRICITY SUPPLY AND WATER COLLECTION TREATMENT AND SUPPLY",
##                        "COMMERCE",
##                        "TRANSPORTS AND COMMUNICATIONS",
##                        "SERVICES FOR BUSINESS, INDIVIDUAL AND HOME",
##                        "SOCIAL AND COMMUNITY SERVICES"
##                        )
##                      )

# #Merge Sector
# gData <- gData %>% inner_join(sector)
# 
# #Group by State
# gData <- gData %>% group_by(estado, sector_economico, edad, sexo)
# gData <- summarise(gData, 
#                    asegurados = sum(asegurados),
#                    no_trabajadores = sum(no_trabajadores),
#                    ta = sum(ta),
#                    teu = sum(teu),
#                    tec = sum(tec),
#                    tpu = sum(tpu),
#                    tpc = sum(tpc),
#                    ta_sal = sum(ta_sal),
#                    teu_sal = sum(teu_sal),
#                    tec_sal = sum(tec_sal),
#                    tpu_sal = sum(tpu_sal),
#                    tpc_sal = sum(tpc_sal),
#                    masa_sal_ta = sum(masa_sal_ta),
#                    masa_sal_teu = sum(masa_sal_teu),
#                    masa_sal_tec = sum(masa_sal_tec),
#                    masa_sal_tpu = sum(masa_sal_tpu),
#                    masa_sal_tpc = sum(masa_sal_tpc),
#                    patrones = sum(patrones)
#                   )


##Write file to CSV
#write.csv(x = gData, file = "gData.csv")

gData <- read.csv(
    "Data/gData.csv", 
    sep =  ",", 
    na.strings = c("NA", ""), 
    encoding = "UTF-8"
  )

gData$edad <- ordered(gData$edad, levels = c("< 15", "15 - 19", "20 - 24", 
                               "25 - 29", "30 - 34", "35 - 39",
                               "40 - 44", "45 - 49", "50 - 54",
                               "55 - 59", "60 - 64", "65 - 69",
                               "70 - 74", "> 75"))

shinyServer(
  function(input, output) {
    output$oEstado <- renderPrint({input$estado})
    output$oSector <- renderPrint({input$sector})
    
    
    output$oPlot <- renderPlot({
      if(input$type == 'sector') {
        #PLOT
        g <- ggplot(gData %>% na.omit() %>% filter(estado == input$estado), 
                    aes(x = edad, y = asegurados, fill = sector_economico, order = sector_economico))
        g <- g + geom_bar(stat = "identity")
        #g <- g + facet_wrap( ~ estado, scales = "free_y") 
        g <- g + scale_y_continuous(name = "Insured", labels = comma)
        g <- g + scale_x_discrete(name = "Age")
        g <- g + theme(legend.direction = "vertical", legend.position = "bottom")
        g <- g + guides(fill=guide_legend(title="Economic Sector", ncol = 2))
        g <- g + ggtitle(paste(input$estado, "\n"))
        print(g)
      }
      
      if(input$type == 'sex') {
        #PLOT
        g <- ggplot(gData %>% na.omit() %>% filter(estado == input$estado & sector_economico == input$sector), 
                    aes(x = edad, y = asegurados, fill = as.factor(sexo), order = sexo))
        #g <- g + facet_wrap( ~ estado, scales = "free_y")
        g <- g + geom_bar(stat = "identity")
        g <- g + scale_y_continuous(name = "Insured", labels = comma)
        g <- g + scale_fill_manual(values = c("steelblue3", "salmon"),
                                   name="Sex",
                                   breaks = c(1, 2),
                                   labels = c("Male", "Female"))
        g <- g + scale_x_discrete(name = "Age")
        g <- g + ggtitle(paste(input$estado, "\n", input$sector, "\n"))
        print(g)                  
      }
    }, height = 600)
  }
)