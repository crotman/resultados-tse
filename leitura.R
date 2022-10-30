
library(tidyverse)
library(rvest)
library(webdriver)

read_html("https://resultados.tse.jus.br/oficial/app/index.html#/eleicao;e=e545;uf=rj;ufbu=rj/resultados")

pjs <- run_phantomjs()

ses <- Session$new(port = pjs$port)

conexao <- DBI::dbConnect(drv = RSQLite::SQLite(), "resultados/db/db.db")

le_uf <- function(uf = "SP"){

    print(uf)
    
    ses$go("https://resultados.tse.jus.br/oficial/app/index.html#/eleicao;e=e545;uf={uf};ufbu={uf}/resultados" %>% str_glue())

    Sys.sleep(time = 3)
    
    lula <- ses$findElement(xpath = "/html/body/app-root/ion-app/ion-router-outlet/ng-component/div/div[2]/ng-component/ng-component/app-centralizar/section/div/app-grade-paginada/ul/li[1]/app-cartao-candidato/div/div[1]/div/div[2]")
    
    votoslula <- lula$getText() %>% 
        str_remove(" votos") %>% 
        as.integer()
    
    total <- ses$findElement(xpath = "/html/body/app-root/ion-app/ion-router-outlet/ng-component/div/div[2]/ng-component/ng-component/app-centralizar/app-barra-acompanhamento/div/div[1]/div")
    
    total_uf <- total$getText() %>% 
        str_extract(pattern = "[0-9,]*%") %>% 
        str_remove("\\%") %>%
        parse_number(
            locale = locale(decimal_mark = ",")
        )

    bozo <- ses$findElement(xpath = "/html/body/app-root/ion-app/ion-router-outlet/ng-component/div/div[2]/ng-component/ng-component/app-centralizar/section/div/app-grade-paginada/ul/li[2]/app-cartao-candidato/div/div[1]/div/div[2]")
    
    votosbozo <- bozo$getText() %>% 
        str_remove(" votos") %>% 
        as.integer()

    saida <- tribble(
        ~uf, ~lula, ~bozo, ~totalizacao,
        uf, votoslula+1, votosbozo+1, (total_uf/100) + 0.000001
    )
    
}
    


resultados <- map_df(
    .x = ufs$uf,
    .f = le_uf
)

DBI::dbWriteTable(
    conn = conexao,
    name = "resultado",
    value = resultados,
    overwrite = TRUE
)





