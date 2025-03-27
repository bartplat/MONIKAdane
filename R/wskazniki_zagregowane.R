#' @title Funkcje liczące wskaźniki zagregowane - Monitoring Karier
#' @description
#' Funkcja służąca do wygenerowania ramki danych będącej zbiorem definicji
#' podziałów na grupy i ewentualnie grupy odniesienia dla danego zestawu
#' wskaźników zagregowanych.
#' @param p4 tabela pośrednia *p4*
#' @param rodzaj_wsk ciąg znaków określający dla jakiego rodzaju wskaźników
#' należy zwórócić definicję podziału na grupy. Możliwe wartości:
#' \describe{
#'   \item{`"szk_god"`}{Główny podział na szkołę oraz jej grupę odniesienia z
#'   wykluczeniem}
#'   \item{`"god1_god2"`}{}
#'   \item{`"szkozaw"`}{Zbiór, gdzie jednostką podziału jest zawód nauczany (i
#'   posiadający absolwentów) w danej szkol - szkoło-zawod.}
#'   \item{`"woj"`}{}
#' }
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% select mutate distinct filter arrange
#' @seealso [utworz_grupowanie_odn_teryt()]
#' @return ramka danych będąca zbiorem definicji podziałów na agregaty
#' @export
definicje_podzialu <- function(p4, rodzaj_wsk, rok_ukonczenia = 2024) {
  rodzaje = c("szk_god", "god1_god2", "szkozaw", "woj")
  
  stopifnot(
    is_tibble(p4) | is.data.frame(p4),
    nrow(p4) > 1,
    rodzaje %in% rodzaj_wsk,
    "Do argumentu `rodzaj_wsk` należy przekazać tylko jedną wartość." = length(rodzaj_wsk) == 1
  )
  
  check_kolumny_tabele(p4, "p4", force = TRUE)
  
  stopifnot(
    length(unique(p4$rok_abs)) == 1,
    p4$rok_abs == rok_ukonczenia
  )
  
  if (rodzaj_wsk == "szk_god") {
    grupy = utworz_grupowanie_odn_teryt(
      x = p4,
      zmGrupujace = c(id_szk, rok_abs, typ_szk),
      zmPominGrupaOdniesienia = c(id_szk),
      idAbs = id_abs)
    
    return(grupy)
  } else if (rodzaj_wsk == "god1_god2") {
    p4$nazwa_zaw[is.na(p4$nazwa_zaw)] = "BRAK ZAWODU"
    
    grupy = p4 %>% 
      select(typ_szk, nazwa_zaw) %>%
      mutate(grupa = paste0("(typ_szk %in% \"", typ_szk, "\") & ",
                            "(nazwa_zaw %in% \"", nazwa_zaw, "\")"),
             odniesienie = paste0("typ_szk %in% \"", typ_szk, "\"")) %>% 
      distinct()
    
    return(grupy)
  } else if (rodzaj_wsk == "szkozaw") {
    p4 = p4 %>% 
      filter(typ_szk %in% c("Branżowa szkoła I stopnia",
                            "Branżowa szkoła II stopnia",
                            "Technikum",
                            "Szkoła policealna"))
    
    p4$nazwa_zaw = gsub("\u00A0", " ", p4$nazwa_zaw, fixed = TRUE)
    
    grupy = utworz_grupowanie_odn_teryt(
      x = p4,
      zmGrupujace = c(id_szk, nazwa_zaw, typ_szk))
    
    return(grupy)
  } else if (rodzaj_wsk == "woj") {
    grupy = p4 %>%
      select(typ_szk, teryt_woj) %>%
      mutate(grupa = paste0("teryt_woj %in% ", teryt_woj),
             odniesienie = paste0("(teryt_woj %in% ", teryt_woj, ") & ",
                                  "(typ_szk %in% \"", typ_szk, "\")")) %>% 
      arrange(typ_szk) %>% 
      distinct()
    
    return(grupy)
  }
}
