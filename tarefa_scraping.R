library(tidyverse)
library(chromote)
library(rvest)


#!abrir o site
webpage <- "https://www.scrapethissite.com/pages/ajax-javascript/"


#!criar uma sessao do Chrome
b <- ChromoteSession$new()

#!abrir a pagina
b$Page$navigate(webpage)
Sys.sleep(2)

#!pegar o html da pagina
html <- b$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

#!ver o começo do html
cat(substr(html, 1, 500), "...\n")

#!transformar o html em pagina
page <- read_html(html)

#!ver os textos dos links da pagina
page %>%
  html_elements("a") %>%
  html_text2()

#!ver os links dos anos
page %>%
  html_elements("a") %>%
  html_attr("href")

#!ver os ids dos links
page %>%
  html_elements("a") %>%
  html_attr("id")

#!clicar no ano de 2015
b$Runtime$evaluate(
  "document.querySelector('#\\\\32 015').click();"
)

#!pegar o html depois do clique
Sys.sleep(2)

html <- b$Runtime$evaluate(
  "document.documentElement.outerHTML"
)$result$value

#!transformar o html atualizado em pagina
page <- read_html(html)

#!extrair a tabela
tabela <- page %>%
  html_element("table.table") %>%
  html_table()
#?funcionou!! agora generalizar para todos os anos

#!pegar os anos
anos <- page %>%
  html_elements("a") %>%
  html_text2()

anos

#!filtrar so os anos
anos <- anos[8:13]

anos

#!extrair as tabelas
tabelas <- map(anos, function(ano){
  
  #?clicar no ano
  b$Runtime$evaluate(
    paste0("document.querySelector('#\\\\32 ", ano, "').click();")
  )
  
  Sys.sleep(1.5)
  
  #?pegar o html atualizado
  html <- b$Runtime$evaluate(
    "document.documentElement.outerHTML"
  )$result$value
  
  #?extrair a tabela
  read_html(html) %>%
    html_element("table.table") %>%
    html_table()
})

#!identificar as tabelas de acordo com os anos
names(tabelas) <- anos 

tabelas
