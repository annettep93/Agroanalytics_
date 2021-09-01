# Paquetes ----------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)


shinyServer( function(input, output, session) {
  
  
  #Fertilizante
 
  output$title <- renderText({
    "<center><h3><strong>CALCULADORA PARA FERTILIZAR</h3></center>"
  })
  
  output$nppal <- renderText({
    "Cantidad de fertilizante a utilizar (kg/ha)"
    
  })
  unique(df_ej$Ejemplos)
  output$n1 <- renderText({
    switch(input$nomb,
           "Amoníaco anhidro"="-",
           "Nitro doble"= "Calcio a incorporar (kg/ha)",
           "Nitrato de amonio"="-",
           "Nitrato de amonio calcáreo (CAN)"="-",
           "UAN"="-",
           "Urea"="-",
           "Fosfato diamónico (FDA)"="Nitrógeno a incorporar (kg/ha)",
           "Fosfato monoamónico (MAP)"="Nitrógeno a incorporar (kg/ha)",
           "Superfosfato triple (SFT)"="-",
           "Superfosfato simple (SPS)"="Azufre a incorporar (kg/ha)",
           "Cloruro de potasio"="-",
           "Nitrato de potasio"="Nitrógeno a incorporar (kg/ha)",
           "Nitrato de sodio y potasio"="Nitrógeno a incorporar (kg/ha)",
           "Sulfato de potasio" ="Azufre a incorporar (kg/ha)",
           "Sulfato de potasio y magnesio (Sulpomag)"="Azufre a incorporar (kg/ha)",
           "Azufre elemental" ="-",
           "Sulfato de amonio"="Nitrógeno a incorporar (kg/ha)",
           "Sulfonitrato de amonio"="Nitrógeno a incorporar (kg/ha)",
           "Tiosulfato de amonio"="Nitrógeno a incorporar (kg/ha)",
           "Yeso agrícola"="Calcio a incorporar (kg/ha)")
  })
  
  output$n2 <- renderText({
    switch(input$nomb,
           "Amoníaco anhidro"="-",
           "Nitro doble"= "Magnesio a incorporar (kg/ha)",
           "Nitrato de amonio"="-",
           "Nitrato de amonio calcáreo (CAN)"="-",
           "UAN"="-",
           "Urea"="-",
           "Fosfato diamónico (FDA)"="-",
           "Fosfato monoamónico (MAP)"="-",
           "Superfosfato triple (SFT)"="-",
           "Superfosfato simple (SPS)"="-",
           "Cloruro de potasio"="-",
           "Nitrato de potasio"="-",
           "Nitrato de sodio y potasio"="-",
           "Sulfato de potasio" ="-",
           "Sulfato de potasio y magnesio (Sulpomag)"="Magnesio a incorporar (kg/ha)",
           "Azufre elemental" ="-",
           "Sulfato de amonio"="-",
           "Sulfonitrato de amonio"="-",
           "Tiosulfato de amonio"="-",
           "Yeso agrícola"="-")
  })
  
  output$n3 <- renderText({
    switch(input$nomb,
           "Amoníaco anhidro"="-",
           "Nitro doble"= "Azufre a incorporar (kg/ha)",
           "Nitrato de amonio"="-",
           "Nitrato de amonio calcáreo (CAN)"="-",
           "UAN"="-",
           "Urea"="-",
           "Fosfato diamónico (FDA)"="-",
           "Fosfato monoamónico (MAP)"="-",
           "Superfosfato triple (SFT)"="-",
           "Superfosfato simple (SPS)"="-",
           "Cloruro de potasio"="-",
           "Nitrato de potasio"="-",
           "Nitrato de sodio y potasio"="-",
           "Sulfato de potasio" ="-",
           "Sulfato de potasio y magnesio (Sulpomag)"="-",
           "Azufre elemental" ="-",
           "Sulfato de amonio"="-",
           "Sulfonitrato de amonio"="-",
           "Tiosulfato de amonio"="-",
           "Yeso agrícola"="-")
  })
  output$resultado <- renderText({
    
    switch(input$nomb,
           "Amoníaco anhidro"=as.numeric(input$num)*100/82,
           "Nitro doble"= as.numeric(input$num)*100/27,
           "Nitrato de amonio"=as.numeric(input$num)*100/33,
           "Nitrato de amonio calcáreo (CAN)"=as.numeric(input$num)*100/27,
           "UAN"=as.numeric(input$num)*100/31,
           "Urea"=as.numeric(input$num)*100/46,
           "Fosfato diamónico (FDA)"=as.numeric(input$num)*100/20,
           "Fosfato monoamónico (MAP)"=as.numeric(input$num)*100/23,
           "Superfosfato triple (SFT)"=as.numeric(input$num)*100/20,
           "Superfosfato simple (SPS)"=as.numeric(input$num)*100/9,
           "Cloruro de potasio"=as.numeric(input$num)*100/50,
           "Nitrato de potasio"=as.numeric(input$num)*100/36,
           "Nitrato de sodio y potasio"=as.numeric(input$num)*100/12,
           "Sulfato de potasio" =as.numeric(input$num)*100/42,
           "Sulfato de potasio y magnesio (Sulpomag)"=as.numeric(input$num)*100/18,
           "Azufre elemental" =as.numeric(input$num)*100/90,
           "Sulfato de amonio"=as.numeric(input$num)*100/24,
           "Sulfonitrato de amonio"=as.numeric(input$num)*100/12,
           "Tiosulfato de amonio"=as.numeric(input$num)*100/26,
           "Yeso agrícola"=as.numeric(input$num)*100/17)
    
  })
  
  output$adicional1 <- renderText({
    
    switch(input$nomb,
           "Nitro doble"= (as.numeric(input$num)*100/27)*6/100,
           
           "Fosfato diamónico (FDA)"=(as.numeric(input$num)*100/20)*18/100,
           "Fosfato monoamónico (MAP)"=(as.numeric(input$num)*100/23)*11/100,
           
           "Superfosfato simple (SPS)"=(as.numeric(input$num)*100/9)*12/100,
           "Cloruro de potasio"="-",
           "Nitrato de potasio"=(as.numeric(input$num)*100/36)*13/100,
           "Nitrato de sodio y potasio"=(as.numeric(input$num)*100/12)*15/100,
           "Sulfato de potasio" =(as.numeric(input$num)*100/42)*16/100,
           "Sulfato de potasio y magnesio (Sulpomag)"=(as.numeric(input$num)*100/18)*22/100,
           "Azufre elemental" ="-",
           "Sulfato de amonio"=(as.numeric(input$num)*100/23)*11/100,
           "Sulfonitrato de amonio"=(as.numeric(input$num)*100/12)*26/100,
           "Tiosulfato de amonio"=(as.numeric(input$num)*100/26)*12/100,
           "Yeso agrícola"=(as.numeric(input$num)*100/17)*22/100)
    
  })
  output$adicional2 <- renderText({
    
    switch(input$nomb,
           
           "Nitro doble"= (as.numeric(input$num)*100/27)*4/100,
           "Amoníaco anhidro"="-",
           
           "Nitrato de amonio"="-",
           "Nitrato de amonio calcáreo (CAN)"="-",
           "UAN"="-",
           "Urea"="-",
           "Fosfato diamónico (FDA)"="-",
           "Fosfato monoamónico (MAP)"="-",
           "Superfosfato triple (SFT)"="-",
           "Superfosfato simple (SPS)"="-",
           "Cloruro de potasio"="-",
           "Nitrato de potasio"="-",
           "Nitrato de sodio y potasio"="-",
           "Sulfato de potasio" ="-",
           "Sulfato de potasio y magnesio (Sulpomag)"=(as.numeric(input$num)*100/18)*11/100,
           "Azufre elemental" ="-",
           "Sulfato de amonio"="-",
           "Sulfonitrato de amonio"="-",
           "Tiosulfato de amonio"="-",
           "Yeso agrícola"="-"
    )
    
  })
  output$adicional3 <- renderText({
    switch(input$nomb,
           
           "Nitro doble"= (as.numeric(input$prod2)*100/27)*6/100)
    
  })
  
  output$ejemplo <- renderTable({
    df_ej %>% filter(X.U.FEFF.Fertilizantes==input$ej)
    
  })
  
  output$desc <- renderText({
    "Extracción de nutrientes de los principales cultivos en Argentina"
  })
  
  output$extraccion <- renderTable({
    df_extraccion
  })
 
  
  })


