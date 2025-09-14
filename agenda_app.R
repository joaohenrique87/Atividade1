# Atividade 1 - CRUD de Agenda em Python ou R - Prof. Welton Dionisio

# Regras de validacao para telefone e email(formato)
validar_telefone <- function(telefone) {
  regex <- "^\\d{11}$"
  return(grepl(regex, telefone))
}

validar_email <- function(email) {
  regex <- "^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}$"
  return(grepl(regex, email))
}

formatar_telefone <- function(telefone_num) {
  telefone_formatado <- sub("^(\\d{2})(\\d{5})(\\d{4})$", "(\\1) \\2-\\3", telefone_num)
  return(telefone_formatado)
}

# Buildando a agenda
build <- function() {
  cat("Bem vindo a agenda \n")
  data.frame(
    id = integer(),
    nome = character(),
    telefone = character(),
    email = character(),
    stringsAsFactors = FALSE
  )
}

# Criacao de contatos e validacoes
add_contato <- function(agenda) {
  cat("\n >>>>> Adicionar contato: <<<<<\n")
  nome <- readline(prompt = "Digite o nome: ")
  
 
  while (TRUE) {
    telefone <- readline(prompt = "Digite o telefone (apenas 11 numeros): ")
    if (validar_telefone(telefone)) {
      break # Sai do loop se for válido
    } else {
      cat("ERRO: Formato inválido! Por favor, digite exatamente 11 numeros.\n")
    }
  }
  
  telefone_formatado <- formatar_telefone(telefone)
  cat("Número formatado para:", telefone_formatado, "\n")
  
  while (TRUE) {
    email <- readline(prompt = "Digite o e-mail: ")
    if (validar_email(email)) {
      break
    } else {
      cat("ERRO: Formato de e-mail invalido! Tente novamente.\n")
    }
  }
  
  if (nome == "") {
    cat("\nErro: O campo nome nao pode ser vazio.\n")
    return(agenda)
  }
  
  dup <- any(agenda$nome == nome & agenda$telefone == telefone_formatado & agenda$email == email)
  
  if (dup) {
    cat("\nErro: Esse contato ja esta registrado.\n")
    return(agenda)
  }
  else {
    if (nrow(agenda) == 0) {
      id_temp <- 1
    } else {
      id_temp <- max(agenda$id) + 1
    }
    
    contato_temp <- data.frame(
      id = id_temp,
      nome = nome,
      telefone = telefone_formatado,
      email = email,
      stringsAsFactors = FALSE
    )
    
    agenda <- rbind(agenda, contato_temp)
    cat("\nContato: '", nome, "' adicionado com sucesso com o ID ", id_temp, ".\n", sep = "")
  }
  
  return(agenda)
}

list_contatos <- function(agenda) {
  cat("\n>>>>> Lista de Contatos <<<<<\n")
  if (nrow(agenda) == 0) {
    cat("Nao ha nenhum contato.\n")
  } else {
    print(agenda)
  }
  cat("--------------------------\n")
}

# Atualizar contatos
att_contato <- function(agenda) {
  cat("\n>>>>> Atualizar Contato <<<<<\n")
  if (nrow(agenda) == 0) {
    cat("Nao ha contato para atualizar.\n")
    return(agenda)
  }
  
  id_para_att <- as.integer(readline(prompt = "Digite o ID do contato que deseja atualizar: "))
  
  indice <- which(agenda$id == id_para_att)
  
  if (length(indice) == 0) {
    cat("\nERRO: ID nao encontrado.\n")
  } else {
    contato_atual <- agenda[indice, ]
    cat("Atualizando dados para o contato:\n")
    print(contato_atual)
    
    novo_nome <- readline(prompt = paste("Novo nome (ou Enter para manter '", contato_atual$nome, "'): ", sep = ""))
    if (novo_nome != "") {
      agenda$nome[indice] <- novo_nome
    }
    
    while (TRUE) {
      novo_telefone <- readline(prompt = paste("Novo telefone (11 números ou Enter para manter): ", sep = ""))
      if (novo_telefone == "") {
        break
      }
      if (validar_telefone(novo_telefone)) {
        agenda$telefone[indice] <- formatar_telefone(novo_telefone)
        break
      } else {
        cat("ERRO: Formato inválido! Digite 11 números ou pressione Enter.\n")
      }
    }
    
    while (TRUE) {
      novo_email <- readline(prompt = paste("Novo e-mail (ou Enter para manter '", contato_atual$email, "'): ", sep = ""))
      if (novo_email == "") {
        break
      }
      if (validar_email(novo_email)) {
        agenda$email[indice] <- novo_email
        break
      } else {
        cat("ERRO: Formato de e-mail inválido! Tente novamente ou pressione Enter.\n")
      }
    }
    
    cat("\nSUCESSO: Contato com ID ", id_para_att, " foi atualizado.\n", sep = "")
  }
  
  return(agenda)
}

excluir_contato <- function(agenda) {
  cat("\n>>>>> Excluir Contato <<<<<\n")
  if (nrow(agenda) == 0) {
    cat("Nenhum contato para excluir.\n")
    return(agenda)
  }
  
  id_excluir <- as.integer(readline(prompt = "Digite o ID do contato que deseja excluir: "))
  
  if (!(id_excluir %in% agenda$id)) {
    cat("\nERRO: ID não encontrado.\n")
  } else {
    agenda <- agenda[agenda$id != id_excluir, ]
    cat("\nSUCESSO: Contato com ID ", id_excluir, " foi excluído.\n", sep = "")
  }
  
  return(agenda)
}

main <- function() {
  agenda <- build()
  
  while (TRUE) {
    cat("\n========== MENU ==========\n")
    cat("1. Adicionar Contato\n")
    cat("2. Listar Contatos\n")
    cat("3. Atualizar Contato\n")
    cat("4. Excluir Contato\n")
    cat("5. Sair\n")
    cat("==========================\n")
    
    opcao <- readline(prompt = "Escolha uma opção: ")
    
    switch(opcao,
           "1" = { agenda <- add_contato(agenda) },
           "2" = { list_contatos(agenda) },
           "3" = { agenda <- att_contato(agenda) },
           "4" = { agenda <- excluir_contato(agenda) },
           "5" = {
             cat("Voce saiu da agenda\n")
             break
           },
           { cat("\nOpção inválida! Por favor, escolha um número de 1 a 5.\n") }
    )
  }
}


main()