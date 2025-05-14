library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(networkD3)
library(shiny)
library(plotly)
datos_crudos <- read_excel("datosReporte.xlsx")

datoss <- datos_crudos[1:12, -c(1,2,4)]

datos <- datoss %>%
  mutate(across(2:62, as.numeric))

datos1 <- datos %>%
  pivot_longer(
    cols = ends_with("]"),
    names_to = "Año",
    values_to = "Valor"
  ) %>%
  rename(Indicador = `Series Name`)

datos2 <- datos1 %>%
  pivot_wider(
    names_from = Indicador, 
    values_from = Valor
  )

datos1 <- datos1 %>%
  mutate(Año = sub("\\[.*", "", Año))

datos1$Año <- as.numeric(datos1$Año)

datos2 <- datos2 %>%
  mutate(Año = sub("\\[.*", "", Año))

datos2$Año <- as.numeric(datos2$Año)

datos1$Indicador <- recode(datos1$Indicador,
                           "Population ages 0-14 (% of total population)" = "0-14 años",
                           "Population ages 15-64 (% of total population)" = "15-64 años",
                           "Population ages 65 and above (% of total population)" = "65+ años",
                           "Birth rate, crude (per 1,000 people)" = "Tasa de natalidad (cada 1000)",
                           "Death rate, crude (per 1,000 people)" = "Tasa de mortalidad (cada 1000)",
                           "Fertility rate, total (births per woman)" = "Tasa de fecundidad",
                           "Net migration" = "Migración neta"
)

indicadores_interes <- c(
  "Tasa de fecundidad",
  "Tasa de mortalidad (cada 1000)",
  "Tasa de natalidad (cada 1000)",
  "Migración neta"
)

datos_linea <- datos1 %>%
  filter(Indicador %in% indicadores_interes)

poblacion <- datos2[,c(1,13)]

poblacion$tasa_crecimiento <- c(NA, diff(poblacion$`Population, total`)/poblacion$`Population, total`[-nrow(poblacion)])


ui <- navbarPage("Reporte demográfico de Uruguay",
                 
                 tabPanel("Introducción",
                          fluidPage(
                            titlePanel("Introducción"),
                            p("Este reporte es una profundización del informe realizado previamente: El impacto del envejecimiento poblacional en el gasto social en Uruguay. Tiene como objetivo continuar analizando variables demográficas de Uruguay a lo largo del tiempo."),
                            p("Los datos utilizados fueron proporcionados por el Banco Mundial, con información disponible hasta el año 2022 inclusive, y proyecciones futuras hasta 2050. En la mayoría de los gráficos se podrá interactuar, para poder ver información con mayor profundidad, y en dos de ellos al colocar el cursor encima se observará información más detallada."),
                            p("Esta herramienta permite explorar a los usuarios cómo a evolucionado la población de Uruguay, y gracias a las proyecciones poder anticipar posibles cambios demográficos, y visualizar los posibles desafíos que podría enfrentar Uruguay en el futuro.")
                          )),
                 
                 tabPanel("Distribución por grupo etario y año",
                          fluidPage(
                            titlePanel(HTML("<h4>Distribución por grupo etario y año</h4>")),
                            sidebarLayout(
                              sidebarPanel(
                                selectInput("Año", "Selecciona un año:", 
                                            choices = unique(datos1$Año[datos1$Año != 1990 & datos1$Año != 2050]),
                                            selected = 2022)),
                              mainPanel(
                                sankeyNetworkOutput("sankey_plot")
                              )))),
                 
                 tabPanel("Esperanza de vida",
                          fluidPage(
                            plotlyOutput("grafico_plotly")
                          )),
                 
                 tabPanel("Población total",
                          fluidPage(
                            plotOutput("grafico_poblacion", height = "600px")
                          )),
                 
                 tabPanel("Gráficos de puntos",
                          sidebarLayout(
                            sidebarPanel(
                              selectInput("indicador_select", "Seleccionar indicador:",
                                          choices = unique(datos_linea$Indicador),
                                          selected = "Tasa de natalidad (cada 1000)")),
                            mainPanel(
                              plotOutput("grafico_linea")
                            ))),
                 
                 
                 tabPanel("Conclusiones",
                          fluidPage(
                            titlePanel("Conclusiones"),
                            p("En el primer gráfico se observó cómo la población de Uruguay ha ido cambiando a lo largo de los años, pasando de tener un mayor porcentaje de personas entre 0 y 14 años de edad, a invertirse y tener un mayor porcentaje de personas de más de 65 años de edad. Lo cual significa que Uruguay presenta una población envejecida, y esto generará problemas en el futuro respecto, por ejemplo, a la relación de dependencia."),
                            p("En el segundo gráfico pudimos ver cómo la esperanza de vida aumenta continuamente (a excepción de 2021 dada la pandemia de Covid 19), esto genera mayor envejecimiento en la población, dado que aumenta la cantidad de personas mayores de 65 años, respecto de otros años en los que las personas fallecían tiempo antes."),
                            p("En el tercer gráfico podemos notar que la tasa de crecimiento anual de la población comenzó decreciente, volviendo a crecer, y a decrecer nuevamente, pero por encima del 0, lo cual significa que la población continuaba creciendo aunque algunas veces en menor medida. Pero a partir del año 2020 comenzó a ser negativa, es decir que la población se encontraba decreciendo, y se espera que continúe así. También podemos notar eso en el área de fondo que representa el tamaño de la población."),
                            p("Por último, en el cuarto gráfico se ven cómo han variado la tasa de fecundidad, la tasa de natalidad, la tasa de mortalidad y la migración neta con el paso de los años. Las primeras dos han disminuido notoriamente, lo cual también aumenta el envejecimiento poblacional, mientras que las otras dos han aumentado, la tasa de mortalidad debería ser estudidada más en profundidad para dar una conclusión, dado que no se contiene la información de las edades, y la migración neta es bueno que aumente, ya que es la diferencia entre los emigrantes e inmigrantes, pero el problema aquí es que a pesar de que aumentó se ha seguido manteniendo negativa, lo cual significa que son más las personas que se van del país que las que vienen a este."),
                            p("En conjunto, los gráficos analizados permiten comprender con mayor profundidad el proceso de envejecimiento poblacional que atraviesa Uruguay. Esta transformación demográfica no sólo afecta la estructura etaria, sino que también tiene implicancias directas en el desarrollo económico y en las políticas sociales del país.")
                          )))

server <- function(input, output) {
  
  output$sankey_plot <- renderSankeyNetwork({
    
    años_seleccionados <- c(1990, input$Año, 2050) 
    
    datos_sankey <- datos1 %>%
      filter(Indicador %in% c("0-14 años", "15-64 años", "65+ años"),
             Año %in% años_seleccionados) %>%
      rename(grupoEtario = Indicador) %>%
      mutate(Año = as.character(Año),
             grupoEtario = as.character(grupoEtario))
    
    datos_sankey$grupoEtario <- factor(datos_sankey$grupoEtario, 
                                       levels = c("0-14 años", "15-64 años", "65+ años"))
    
    
    nodos <- data.frame(name = c(as.character(años_seleccionados), 
                                 "0-14 años", "15-64 años", "65+ años"))
    
    
    enlaces <- datos_sankey %>%
      mutate(source = match(Año, nodos$name) - 1,
             target = match(grupoEtario, nodos$name) - 1) %>%
      select(source, target, Valor)
    
    
    sn <-  sankeyNetwork(Links = enlaces,
                         Nodes = nodos,
                         Source = "source",
                         Target = "target",
                         Value = "Valor",
                         NodeID = "name",
                         fontSize = 12,
                         nodeWidth = 30,
                         colourScale = JS("d3.scaleOrdinal()
                      .domain(['0-14', '15-64', '65+', '1990', '2025', '2050'])
                      .range(['#E7969C', '#E7969C', '#E7969C', '#E7969C', '#E7969C', '#E7969C'])"),
                         iterations = 0) 
    
    htmlwidgets::onRender(sn, '
  function(el, x) {
    // Cambiar tooltip de los links (líneas)
    d3.select(el).selectAll(".link").select("title")
      .text(function(d) {
        return d.value.toFixed(1) + "%";
      });

    // Ocultar tooltip de los nodos (rectángulos)
    d3.select(el).selectAll(".node").select("title").text("");
  }
')
    
    
  })
  
  
  output$grafico_poblacion <- renderPlot({
    
    max_poblacion <- max(poblacion$`Population, total`, na.rm = TRUE)
    max_tasa <- max(abs(poblacion$tasa_crecimiento), na.rm = TRUE)
    factor_escala <- max_poblacion / (max_tasa * 1.2) 
    
    ggplot(poblacion, aes(x = Año)) +
      
      geom_area(aes(y = poblacion$`Population, total`, fill = "Población"), 
                alpha = 0.5, position = "identity") +
      
      
      geom_line(aes(y = poblacion$tasa_crecimiento * factor_escala, 
                    color = "Tasa de crecimiento"), 
                linewidth = 1.2, na.rm = TRUE) +
      
      
      scale_y_continuous(
        name = "Población total",
        labels = label_number(),
        sec.axis = sec_axis(~ . / factor_escala,
                            name = "Tasa de crecimiento",
                            labels = percent_format(accuracy = 0.1))
      ) +
      
      
      scale_fill_manual(values = c("Población" = "#3A5FCD")) +
      scale_color_manual(values = c("Tasa de crecimiento" = "#FF6347")) +
      
      
      labs(title = "Evolución de la población y tasa de crecimiento",
           x = "Año") +
      
      theme_minimal() +
      theme(
        legend.position = "top",
        legend.title = element_blank(),
        plot.title = element_text(hjust = 0.5, face = "bold")
      )
    
  })
  
  output$grafico_plotly <- renderPlotly({
    
    datosplotly <- datos1 %>%
      mutate(
        grupo = case_when(
          grepl("Life expectancy at birth, total \\(years\\)", Indicador) ~ "Total",
          grepl("Life expectancy at birth, male \\(years\\)", Indicador) ~ "Hombre",
          grepl("Life expectancy at birth, female \\(years\\)", Indicador) ~ "Mujer",
          TRUE ~ NA_character_
        )
      ) %>%
      filter(!is.na(grupo))
    
    
    rango_min <- floor(min(datosplotly$Valor, na.rm = TRUE) - 2)
    rango_max <- ceiling(max(datosplotly$Valor, na.rm = TRUE) + 2)
    
    
    grafico <- plot_ly(
      data = datosplotly,
      x = ~grupo,
      y = ~Valor,
      color = ~grupo,
      colors = c("Total" = "#1f77b4", "Hombre" = "#ff7f0e", "Mujer" = "#d62728"),
      type = "bar",
      frame = ~Año,
      text = ~paste0("Año: ", Año,
                     "<br>Grupo: ", grupo,
                     "<br>Esperanza de vida: ", round(Valor, 1), " años"),
      hoverinfo = "text"
    ) %>%
      layout(
        title = list(text = "<b>Evolución de la Esperanza de Vida</b>", 
                     x = 0.05, y = 0.95),
        xaxis = list(title = ""),
        yaxis = list(title = "Años de vida esperados", range = c(rango_min, rango_max))
        ,
        font = list(family = "Arial"),
        margin = list(l = 50, r = 50, b = 80, t = 80)
      ) %>%
      animation_opts(
        frame = 800,
        transition = 400,
        easing = "linear"
      ) %>%
      animation_slider(currentvalue = list(prefix = "Año: ", font = list(size = 14))) %>%
      animation_button(label = "Play")
    
    grafico
    
  })
  
  output$grafico_linea <- renderPlot({
    datos_filtrados <- datos_linea %>%
      filter(Indicador == input$indicador_select)
    
    ggplot(datos_filtrados, aes(x = Año, y = Valor)) +
      geom_point(color = "#E7969C") + 
      labs(title = input$indicador_select,
           x = "Año", y = "Valor") +
      theme_minimal()
    
    
  })
  
}


shinyApp(ui = ui, server = server)