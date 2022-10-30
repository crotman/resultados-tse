#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(DBI)
library(reactable)

conexao <- DBI::dbConnect(drv = RSQLite::SQLite(),"db/db.db")

resultados <- tbl(conexao, "resultado") %>% 
    collect()

resultados_tidy <- resultados %>% 
    pivot_longer(
        cols = c(lula, bozo),
        names_to = "candidato",
        values_to = "voto"
    ) %>% 
    mutate(
        voto_ajustado = 1/totalizacao * voto
    )


totais <- resultados_tidy %>%
    mutate(
        totais = sum(voto_ajustado)
    ) %>% 
    group_by(
        candidato
    ) %>% 
    summarise(
        votos = sum(voto_ajustado)/mean(totais)
    )



poli <- read_rds("poli")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Resultados"),
    
    "Resultado Projetado",
    
    reactableOutput(
        outputId = "resultado"
    ),
    
    "Detalhes",
    
    reactableOutput(
        outputId = "detalhe"
    ),
    
    plotOutput(
        outputId = "votos"
    ),
    
    plotOutput(
        outputId = "progresso"
    ),
    
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$resultado <-  renderReactable(
        reactable(
            totais,
            columns = list(
                votos = colDef(format = colFormat(percent = TRUE))
            )
                
        )
    )
    
    
    output$detalhe <-  renderReactable(
        reactable(
            resultados_tidy %>% 
                group_by(
                    uf
                ) %>% 
                mutate(
                    total_uf = sum(voto_ajustado)
                ) %>% 
                ungroup() %>% 
                mutate(
                    perc = voto_ajustado / total_uf
                ) %>% 
                rename(
                    voto_projetado = voto_ajustado
                ) %>% 
                pivot_wider(
                    names_from = candidato,
                    values_from = c(voto, voto_projetado, perc)
                ) %>% 
                select(-total_uf),
            pagination = FALSE,
            columns = list(
                totalizacao = colDef(format = colFormat(percent = TRUE)),
                perc_lula = colDef(format = colFormat(percent = TRUE)),
                perc_bozo = colDef(format = colFormat(percent = TRUE))
            )
            
        )
    )
    

}

# Run the application 
shinyApp(ui = ui, server = server)
