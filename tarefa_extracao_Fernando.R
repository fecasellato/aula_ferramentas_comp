library(tidyverse)

#!ler o arquivo .pdf
arquivo <- "./cadastro.pdf"

conteudo <- pdftools::pdf_text(arquivo) #?transforma o conteudo do pdf em texto


#!quebrar o conteudo do pdf em linhas
conteudo <- str_split(conteudo, "\\n")

conteudo <- Reduce(c, conteudo) #?juntar todas as linhas


#!localizar onde começa cada cadastro
#?o nome marca o inicio de cada cadastro
pos_nome <- grep("^[[:space:]]*[Nn]ome\\s*:", conteudo)#?tem nome com maiusculo e nome com minusculo

pos_nome <- c(pos_nome, length(conteudo) + 1) #?adiciona uma posicao final para conseguir separar o ultimo cadastro


#!funcao para extrair cada cadastro
extrair_cadastro <- function(x, conteudo, pos_nome){

  i <- x

  #?pegar somente as linhas pertencentes a este cadastro
  cadastro <- conteudo[pos_nome[i]:(pos_nome[i + 1] - 1)]

  cadastro <- trimws(cadastro, "both")


  #!nome e apelido

  linha_nome <- cadastro[grep(
    "^[[:space:]]*[Nn]ome\\s*:",
    cadastro
  )][1]

  linha_nome <- gsub(
    "^[Nn]ome\\s*:\\s*",
    "",
    linha_nome
  ) #?tirar o "Nome:"

  apelido <- str_extract(
    linha_nome,
    "\\([^\\)]*\\)"
  ) #?o apelido esta entre parenteses

  nome <- gsub(
    "\\s*\\([^\\)]*\\)",
    "",
    linha_nome
  ) #?tirar o apelido do nome

  nome <- trimws(nome, "both")


  #?tirar o aka do apelido
  if(!is.na(apelido)){

    apelido <- gsub(
      "^\\(|\\)$",
      "",
      apelido
    )

    apelido <- gsub(
      "^[Aa][Kk][Aa]\\s+",
      "",
      apelido
    )

    apelido <- trimws(apelido, "both")

  }


  #!data de nascimento

  pos_data <- grep(
    "[Dd]ata\\s+de\\s+nascimento|[Dd]t\\.?\\s+nasc",
    cadastro
  ) #?tem "Data de nascimento" e "Dt nasc"

  data_nascimento <- NA

  if(length(pos_data) > 0){

    linha_data <- cadastro[pos_data[1]]

    data_nascimento <- str_extract(
      linha_data,
      "[0-9]{1,2}[-/][0-9]{1,2}[-/][0-9]{2,4}[[:alnum:]\\.]*"
    )

    if(is.na(data_nascimento)){

      data_nascimento <- str_extract(
        linha_data,
        "[0-9]{1,2}/[A-Za-z]{3}/[0-9]{4}"
      ) #?caso a data tenha o mes escrito

    }

  }


  #!endereco

  pos_end <- grep(
    "^[Ee]ndere[cç]o\\s*:",
    cadastro
  )

  endereco <- NA

  if(length(pos_end) > 0){

    linha_end <- cadastro[pos_end[1]]

    endereco <- gsub(
      "^[Ee]ndere[cç]o\\s*:\\s*",
      "",
      linha_end
    ) #?tirar "Endereco:"

    endereco <- gsub(
      "[Cc][Ee][Pp]\\s*:\\s*[0-9\\.\\-]+",
      "",
      endereco
    ) #?tem caso em que o CEP esta na mesma linha do endereco

    endereco <- trimws(endereco, "both")

  }


  #!CEP

  pos_cep <- grep(
    "[Cc][Ee][Pp]\\s*:",
    cadastro
  ) #?o CEP pode estar na linha de baixo ou na mesma linha do endereco

  cep <- NA

  if(length(pos_cep) > 0){

    linha_cep <- cadastro[pos_cep[1]]

    cep <- str_extract(
      linha_cep,
      "[0-9]{2}[0-9\\.\\-]*[0-9]{3}"
    )

  }


  #!telefone

  pos_tel <- grep(
    "[Tt]el(efone)?\\s*:",
    cadastro
  ) #?tem Tel e Telefone

  telefone <- NA

  if(length(pos_tel) > 0){

    linha_tel <- cadastro[pos_tel[1]]

    telefone <- str_extract(
      linha_tel,
      "\\(?[0-9]{2}\\)?[[:space:]]*[0-9]{4,5}[[:space:]]*[-]?[[:space:]]*[0-9]{4}"
    ) #?pode ter parenteses, espacos ou hifen

  }


  #!CPF

  pos_cpf <- grep(
    "^[Cc][Pp][Ff]\\s*:",
    cadastro
  )

  cpf <- NA

  if(length(pos_cpf) > 0){

    linha_cpf <- cadastro[pos_cpf[1]]

    cpf <- str_extract(
      linha_cpf,
      "[0-9]{3}[\\. ]?[0-9]{3}[\\. ]?[0-9]{3}[- ]?[0-9]{2}"
    ) #?alguns CPFs tem ponto e hifen e outros nao

  }


  #!montar o data frame

  dados <- data.frame(
    Nome = nome,
    Apelido = apelido,
    Data_de_nascimento = data_nascimento,
    Endereco = endereco,
    CEP = cep,
    Telefone = telefone,
    CPF = cpf
  )

  return(dados)

}


#!extrair todos os cadastros

dados <- lapply(
  1:(length(pos_nome) - 1),
  extrair_cadastro,
  conteudo = conteudo,
  pos_nome = pos_nome
)


#!juntar todos os individuos em uma tabela

dados <- Reduce(rbind, dados)

View(dados) #?visualizar o resultado final