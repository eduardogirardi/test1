
#' Modelo simples
#'
#' @param A coeficiente A
#' @param m coeficiente m
#' @param Y1 idade no tempo 1
#' @param IA idade de atual
#' @param IP idade de projecao
#'
#' @return retorna o volume Y1 projetado para a idade IP
#' @export
#'
#' @examples
#'
Korf_e1 <- function(A=1,m=1,Y1=1,IA=1,IP=1) {
  Y2 <- A * (Y1/A) ^ ((IA/IP)^m)
  return(Y2)
}



#' Modelo duplo de projecao
#'
#' @param A1 coeficiente A do primeiro modelo
#' @param A2 coeficiente A do segundo modelo
#' @param m1 coeficiente m do primeiro modelo
#' @param m2 coeficiente m do segundo modelo
#' @param IB idade base (idade de de apoio)
#' @param Y1 volume no tempo 1
#' @param IA idade atual
#' @param IP idade de projecao
#'
#' @return retorna o volume Y1 projetado para a idade IP
#' @export
#'
#' @examples
Korf_proj <- function(A1=1, A2= 1, m1=1, m2=1, IB=1, Y1=1, IA=1, IP=1) {
  YB <- A1 * (Y1/A1) ^ ((IA/IB)^m1)
  Y2 <- A2 * (YB/A2) ^ ((IB/IP)^m2)
  return(Y2)
}



#' Modelo duplo de regrecao
#'
#' @param A1 coeficiente A do primeiro modelo
#' @param A2 coeficiente A do segundo modelo
#' @param m1 coeficiente m do primeiro modelo
#' @param m2 coeficiente m do segundo modelo
#' @param IB idade base (idade de de apoio)
#' @param Y1 volume no tempo 1
#' @param IA idade atual
#' @param IP idade de projecao
#'
#' @return retorna o volume Y1 projetado para a idade IP
#' @export
#'
#' @examples
Korf_regre <- function(A1=1, A2= 1, m1=1, m2=1, IB=1, Y1=1, IA=1, IP=1) {
  YB <- A2 * (Y1/A2) ^ ((IA/IB)^m2)
  Y2 <- A1 * (YB/A1) ^ ((IB/IP)^m1)
  return(Y2)
}
