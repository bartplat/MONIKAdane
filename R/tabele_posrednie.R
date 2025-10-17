#' @title Funkcje przekształcające surowe tabele pośrednie - Monitoring Karier
#' @description
#' Funkcja przekształcająca surowe tabele pośrednie do formy gotowej do użytku w
#' dalszej części procesu przechodzenia od tabel pośrednich do generowania
#' automatycznych raportów. 
#' @param sciezka_tab_posrednie ścieżka w formacie tekstowym, w ktorej znajdują
#' się tabele pośrednie
#' @param sciezka_docelowa ścieżka w formacie tekstowym, w ktorej mają być
#' zapisane surowe tabele pośrednie
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @importFrom dplyr %>% filter group_by count filter pull select distinct
#' left_join join_by coalesce all_of
#' @return multiple `.RData` objects
#' @seealso [rozdziel_tabele()]
#' @export
przygotuj_tabele_posrednie <- function(sciezka_tab_posrednie, sciezka_docelowa,
                                       rok_ukonczenia = 2024) {
  tryCatch({
    log_file = paste0("przygotuj_tabele_posrednie_logfile_", format(Sys.time(), "%Y%m%d_%H%M"), ".txt")
    sink(log_file, split = TRUE)
    czas_start = Sys.time()
    cat("\nStart: ", format(czas_start, "%Y.%m.%d %H:%M:%S"), "\n", sep = "")
    
    stopifnot(is.character(sciezka_tab_posrednie),
              is.character(sciezka_docelowa),
              is.numeric(rok_ukonczenia) & rok_ukonczenia > 2021,
              grepl(".RData$", sciezka_tab_posrednie))
    
    # wczytywanie tabel
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Rozpoczęcie wczytywania obiektu z tabelami pośrednimi.", sep = "")
    obiekt_tabele <- load(sciezka_tab_posrednie)
    
    if (!"p1" %in% ls()) stop(paste0("Bład: W pliku ze ścieżki \"", sciezka_tab_posrednie, "\" brakuje tabel pośrednich p1"))
    if (!"p2" %in% ls()) stop(paste0("Bład: W pliku ze ścieżki \"", sciezka_tab_posrednie, "\" brakuje tabel pośrednich p2"))
    if (!"p3" %in% ls()) stop(paste0("Bład: W pliku ze ścieżki \"", sciezka_tab_posrednie, "\" brakuje tabel pośrednich p3"))
    if (!"p4" %in% ls()) stop(paste0("Bład: W pliku ze ścieżki \"", sciezka_tab_posrednie, "\" brakuje tabel pośrednich p4"))
    if (!"p5" %in% ls()) stop(paste0("Bład: W pliku ze ścieżki \"", sciezka_tab_posrednie, "\" brakuje tabel pośrednich p5"))
    
    # zawężenie do aktualnego roku zostania absolwentem (rok_abs == rok_ukonczenia)
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Zawężanie tabel pośrednich do bieżącej edycji monitoringu, czyli do ",
        rok_ukonczenia + 1, " roku (absolwenci z rocznika ", rok_ukonczenia, ").",
        sep = "")
    p1 <- p1 %>% filter(rok_abs %in% rok_ukonczenia)
    if (nrow(p1) == 0) stop(paste0("\nBłąd: Tabela p1, po zawężeniu do absolwentów z roku ", rok_ukonczenia, " ma 0 wierszy!"))
    p2 <- p2 %>% filter(rok_abs %in% rok_ukonczenia)
    if (nrow(p2) == 0) stop(paste0("\nBłąd: Tabela p2, po zawężeniu do absolwentów z roku ", rok_ukonczenia, " ma 0 wierszy!"))
    p3 <- p3 %>% filter(rok_abs %in% rok_ukonczenia)
    if (nrow(p3) == 0) stop(paste0("\nBłąd: Tabela p3, po zawężeniu do absolwentów z roku ", rok_ukonczenia, " ma 0 wierszy!"))
    p4 <- p4 %>% filter(rok_abs %in% rok_ukonczenia)
    if (nrow(p4) == 0) stop(paste0("\nBłąd: Tabela p4, po zawężeniu do absolwentów z roku ", rok_ukonczenia, " ma 0 wierszy!"))
    p5 <- p5 %>% filter(rok_abs %in% rok_ukonczenia)
    if (nrow(p5) == 0) stop(paste0("\nBłąd: Tabela p5, po zawężeniu do absolwentów z roku ", rok_ukonczenia, " ma 0 wierszy!"))
    
    # Usuwam duplikaty
    duplikaty <- p4 %>% 
      group_by(id_abs) %>% 
      count() %>% 
      filter(n > 1)
    if (nrow(duplikaty > 0)) {
      duplikaty <- duplikaty %>% 
        pull(id_abs)
      
      p1 <- p1 %>% filter(!(id_abs %in% duplikaty))
      p2 <- p2 %>% filter(!(id_abs %in% duplikaty))
      p3 <- p3 %>% filter(!(id_abs %in% duplikaty))
      p4 <- p4 %>% filter(!(id_abs %in% duplikaty))
      p5 <- p5 %>% filter(!(id_abs %in% duplikaty))
      
      cat("\nUsunięte duplikaty: ",
          length(unique(duplikaty)),
          "\nW tym unikalnych absolwentów (wyświetlone zostanie maksymalnie pierwsze 20 wartości oraz ostatnie 20 wartości): \n",
          if (length(unique(duplikaty)) > 40) {
            paste0(paste(head(unique(duplikaty), 20), collapse = ", "), paste(tail(unique(duplikaty), 20), collapse = ", "))
            } else {
              paste(unique(duplikaty), collapse = ", ")
            },
          "\n",
          sep = "")
    } else {
      cat("\nBrak zduplikowanych absolwentów w zbiorze.")
    }
    
    # dodawanie zmiennych grupujących do `p3` i `p2`
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dodawanie zmiennych grupujących do tabel pośrednich `p3` i `p2`.", sep = "")
    zmienneGrupujace <- c("id_szk", "id_abs", "rok_abs", "typ_szk", "teryt_woj_szk", "teryt_pow_szk", "nazwa_zaw", "branza", "plec")
    p3 <- p3 %>% 
      left_join(p4 %>% select(id_abs, rok_abs, all_of(setdiff(zmienneGrupujace, names(p3)))),
                join_by(id_abs, rok_abs))
    if (length(setdiff(zmienneGrupujace, names(p3))) > 0) stop(paste0("\nW tabeli p3 brakuje zmiennych: ", setdiff(zmienneGrupujace, names(p3))))
    p2 <- p2 %>% 
      left_join(p4 %>% select(id_abs, rok_abs, all_of(setdiff(zmienneGrupujace, names(p2)))),
                join_by(id_abs, rok_abs))
    if (length(setdiff(zmienneGrupujace, names(p3))) > 0) stop(paste0("\nW tabeli p2 brakuje zmiennych: ", setdiff(zmienneGrupujace, names(p3))))
    
    # przypomnienie o rekodowaniu zawodów eksperymentalnych
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Sprawdzanie czy w zmiennej `p4$branza` występują braki danych.", sep = "")
    branze_braki <- NULL
    branza_na <- p4 %>% 
      select(id_abs, id_szk, nazwa_zaw, branza) %>% 
      filter(!is.na(p4$nazwa_zaw) & is.na(p4$branza))
    if (nrow(branza_na > 0)) {
      if (!"brakujace_branze.csv" %in% list.files()) {
        branza_na <- branza_na %>% 
          select(nazwa_zaw, branza) %>% 
          distinct()
        write.csv2(branza_na, "brakujace_branze.csv", row.names = FALSE)
        cat("\nLista zawodów z brakującymi branżami została zapisana do pliku \"brakujace_branze.csv\".\n")
        branze_braki <- paste0("* ", nrow(branza_na), " absolwentów ma przypisany zawód, ale nie ma przypisanej branży. Nazwy branż należy uzupełnić, aby kontynuować przetwarzanie.")
      } else {
        branza_na <- read.csv2("brakujace_branze.csv")
        stopifnot(is.data.frame(branza_na),
                  c("nazwa_zaw", "branza") %in% names(branza_na))
        if (sum(is.na(branza_na$branza)) != 0) {
          branze_braki <- "* Znaleziono plik \"brakujace_branze.csv\", ale zawiera on braki danych w kolumnie `branza`. Nazwy branż należy uzupełnić, aby kontynuować przetwarzanie."
        } else {
          p4 <- p4 %>% 
            left_join(branza_na,
                      join_by(nazwa_zaw)) %>%
            mutate(branza = coalesce(branza.x, branza.y), .keep = "unused")
          
          cat("\nBraki danych w zmiennej `p4$branza` zostały uzupełnione na podstawie \"brakujace_branze.csv\".")
        }
      }
    } else {
      cat("\nW zmiennej `p4$branza` nie było braków danych.")
    }
    
    # sprawdzanie czy nie brakuje wynagrodzeń
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Sprawdzanie kompletności danych o średnich wynagrodzeniach w powiecie (zmienna`p3$powiat_sr_wynagrodzenie`).", sep = "")
    wynagrodzenia_braki <- NULL
    powiat_sr_wynagrodzenie_na <- p3 %>% 
      filter(is.na(powiat_sr_wynagrodzenie))
    if (nrow(powiat_sr_wynagrodzenie_na) > 0) {
      p3 <- uzupelnij_wynagrodzenie_powiat(tabela_p3 = p3,
                                           rok_ukonczenia = rok_ukonczenia)
    }
    
    powiat_sr_wynagrodzenie_na <- p3 %>% 
      filter(is.na(powiat_sr_wynagrodzenie))
    if (nrow(powiat_sr_wynagrodzenie_na) > 0) {
      wynagrodzenia_braki <- "\n* Braki danych w zmiennej `p3$powiat_sr_wynagrodzenie` - przed dalszą pracą należy je uzupełnić (np. za pomocą funkcji `uzupelnij_wynagrodzenie_powiat()`).\nMożliwe, że brakuje danych w powiecie Jastrzębie-Zdrój (teryt: 2467)"
    }
    
    if (any(!is.null(branze_braki), !is.null(wynagrodzenia_braki))) {
      stop(paste(branze_braki, wynagrodzenia_braki, collapse = "\n"))
    }
    
    # usuwanie twardych spacji w nazwach zawodów
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Usuwanie twardych spacji w nazwach zawodów.", sep = "")
    p4$nazwa_zaw <- gsub("\u00A0", " ", p4$nazwa_zaw, fixed = TRUE)
    p4$nazwa_zaw <- trimws(p4$nazwa_zaw)
    
    # rekodowanie bednarskiej
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Sprawdzanie czy w danych występuje Bednarska Szkoła Realna - rekodowanie na Liceum ogólnokształcące.", sep = "")
    if (any(p4$typ_szk == "Bednarska Szkoła Realna")) {
      p4$typ_szk[p4$typ_szk == "Bednarska Szkoła Realna"] <- "Liceum ogólnokształcące"
      cat("\nW tabeli `p4` w zmiennej `typ_szk` wartość \"Bednarska Szkoła Realna\" została zmieniona na \"Liceum ogólnokształcące\".")
    }

    # dodawanie zmiennej `adres_szk` do p6
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dodawanie zmiennej `adres_szk` do p6.", sep = "")
    p6 <- p6 %>% 
    mutate(
      across(
        nr_lokalu,
        ~ifelse(. == "-", paste0(""), .)
      )
    ) %>% 
    mutate(
      adres_szk = ifelse(
        is.na(ulica),
        paste0(
          miejscowosc,
          " ",
          nr_budynku,
          ", ",
            pna,
            " ",
            poczta
          ),
          paste0(
            ulica,
            " ",
            nr_budynku,
            ifelse(
              nr_lokalu == "" | is.na(nr_lokalu),
              paste0(", "),
              paste0("/", nr_lokalu, ", ")
            ),
            pna,
            " ",
            poczta)
          )
      )
    cat(
      "\n",
      "Dodano zmienną `adres_szk` do p6.\nLiczba braków danych: ",
      sum(is.na(p6$adres_szk)),
      sep = ""
    )

    # dodawanie zmiennej z adresem i nazwą do p4
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dodawanie zmiennej `adres_szk` i `nazwa_szk` do p4.", sep = "")
    p4 <- p4 %>% 
      left_join(
        p6 %>% select(id_szk, nazwa_szk, adres_szk) %>% distinct(),
        join_by(id_szk)
      )
    stopifnot(
      c("adres_szk", "nazwa_szk") %in% names(p4)
    )
    
    # zapis
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Rozpoczęto zapisywanie tabel pośrednich.", sep = "")
    save(p1, p2, p3, p4, p5, p6, file = paste0(sciezka_docelowa,
                                           "tabele_posrednie_wrz2025_rokabs", rok_ukonczenia, ".RData"))
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Zapisano PEŁNE tabele p1:p6.", sep = "")
    save(p3, p4, file = paste0(sciezka_docelowa,
                               "tabele_posrednie_wrz2025_p3p4_rokabs", rok_ukonczenia, ".RData"))
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Zapisano okrojone tabele p3:p4.", sep = "")
    save(p2, p3, p4, file = paste0(sciezka_docelowa,
                                   "tabele_posrednie_wrz2025_p2-p4_rokabs", rok_ukonczenia, ".RData"))
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Zapisano okrojone tabele p2:p4.", sep = "")
    
    czas_stop <- Sys.time()
    czas_roznica<- round(czas_stop - czas_start, 2)
    cat("\nKoniec: ", format(czas_stop, "%Y.%m.%d %H:%M:%S"), "\nCzas działania funkcji: ", paste(as.numeric(czas_roznica), attr(czas_roznica, "units")), "\n", sep = "")
  },
  error = function(e) {
    cat("\nSkrypt zwrócił błąd: \n", conditionMessage(e), "\n", sep = "")
    cat("\nTraceback:\n")
    traceback()},
  finally = {
    sink()
    message("Zamknięto plik log.")}
  )
  # file.show(log_file, title = "Plik log")
}
#' @title Funkcje przekształcające surowe tabele pośrednie - Monitoring Karier
#' @description
#' Funkcja uzupełnia brakujące dane w zmiennej `powiat_sr_wynagrodzenie` w
#' tabeli pośredniej `p3`. Do tej pory spotkaliśmy się z dwoma rodzajami braków
#' danych:
#' \itemize{
#'   \item{brak **wszystkich** wynagrodzeń}{czasem zdarza się, że z jakiegoś
#'   powodu tabela `p3` w ogóle nie ma danych o wynagrodzeniach w powiecie. W
#'   takim wypadku funkcja pobierze dane z BDL i dołączy je do zbioru.}
#'   \item{brak **wybranych** wynagrodzeń}{w ostatnich latach GUS przestał
#'   publikować dane dla powiatu Jastrzębie-Zdrój (kod teryt: 2467). Do
#'   poprawnego działania funkcji używanych w dalszych krokach procesu
#'   generowania raportów automatycznych wymagana jest kompletność danych o
#'   przeciętnym wynagrodzeniu w powiecie.}
#' }
#' Funkcja rozpoznaje rodzaj braku danych i uzupełnia je albo pobierając dane z
#' BDL albo przewidując brakującą wartość w danym roku na podstawie wartości z
#' lat poprzednich w powiecie oraz w województwie.
#' @details
#' Funkcja, w zależności od napotkanych braków danych, podejmuje rózne próby
#' uzupełnienia danych. W pierwszym kroku sprawdzane jest czy w ogóle zmienna
#' dotycząca wynagrodzeń w powiatach zawiera jakieś braki danych. Jeżeli ich nie
#' ma, to działanie jest przerywane, natomiast można je wymusić za pomocą
#' argumentu `force = TRUE`.
#' 
#' @param tabela_p3 ścieżka do pliku z tabelą pośrednią `p3`, która przechowuje
#' m.in. informację o średnim wynagrodzeniu w powiecie na podstawie danych z
#' GUS.
#' @param rok_ukonczenia rok, którym absolwent ukończył szkołę (jest to tym
#' samym rok monitoringu)
#' @param wynagrodzenia tabela z mapowaniem wynagrodzeń. Jest to argument 
#' opcjonalny, ponieważ funkcja podejmie próbę uzupełnienia brakujących wartości
#' pobierając je z BDL, ale jeśli się to nie uda można przekazać jako argument
#' swoją tabelę z mapowaniem wynagrodzeń na teryty powiatów i na jej podstawie
#' brakujące wartości zostaną dołączone do tabeli pośredniej.
#' @param force wartość logiczna mówiąca o tym czy uzupełnianie wartości ma się
#' odbywać nawet jeśli nie ma braków danych
#' @return tabela pośrednia `p3`
#' @importFrom tibble is_tibble
#' @importFrom dplyr %>% rename mutate across select left_join join_by ungroup
#' group_by lag arrange
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom MLASZdane wskaznik_bdl pobierz_dane_bdl przeksztalc_dane_bdl
#' @export
uzupelnij_wynagrodzenie_powiat <- function(tabela_p3,
                                           rok_ukonczenia = 2024, wynagrodzenia = NULL, force = FALSE) {
  stopifnot(is.data.frame(tabela_p3),
            is.numeric(rok_ukonczenia),
            rok_ukonczenia > 2021,
            is.logical(force))
  
  # assign("p3", get(load(tabela_p3)))
  assign("p3", tabela_p3)
  
  stopifnot("powiat_sr_wynagrodzenie" %in% names(p3))
  
  if(sum(is.na(p3$powiat_sr_wynagrodzenie)) == 0 & force) {
    warning("Przekazana tabela `p3` nie ma brakujących wartości w zmiennej `powiat_sr_wynagrodzenie`. Mimo to, zmienna ta zostanie uzupełniona od nowa.")
  } else if (sum(is.na(p3$powiat_sr_wynagrodzenie)) == 0 & !force) {
    stop("Przekazana tabela `p3` nie ma brakujących wartości w zmiennej `powiat_sr_wynagrodzenie`, więc wartości te nie zostaną nadpisane. W celu wymuszenia nadpisania należy użyć argumentu `force = TRUE`.")
  }
  
  czas_start <- Sys.time()
  cat("Rozpoczęto uzupełnianie wynagrodzeń: ", format(Sys.time(), "%Y.%m.%d %H:%M:%S"), sep = "")
  
  if (!is.null(wynagrodzenia)) {
    # w jakim formacie powinna być ta tabela? może csv LUB RData (oba warianty najchętniej)?
    stopifnot(is_tibble(wynagrodzenia) | is.data.frame(wynagrodzenia),
              nrow(wynagrodzenia) > 0)
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dołączanie danych z pliku przekazanego w argumencie funkcji `wynagrodzenie`.", sep = "")
    # check na strukturę tabeli potem dołączenie danych, znów sprawdzanie braków (jeśli są to chyba lecimy dalej z uzupełnianiem) i save(p3) jeśli wszystko okej
    save(p3, file = tabela_p3)
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Uzupełniono i zapisano na podstawie pliku przekazanego w argumencie `wynagrodzenie`.", sep = "")
  } else {
    # tu jednak chyba bym wolał uzupełniać braki z BDLu dla każdego roku rok_abs, a nie tylko dla obecnego jak teraz.
    # więc to będzie trzedba przepisać. Filtorwanie po rok_abs idzie dopiero na etapie przygotowywania tabel, ale może ktoś będzie chciał skorzystać.
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Pobieranie i dołączanie danych o wynagrodzeniach z BDL.", sep = "")
    bdl_lata <- 2019:rok_ukonczenia
    
    stopifnot(is.numeric(bdl_lata),
              length(bdl_lata) > 3)
    
    cat("\n", format(Sys.time(), "%H:%M:%S"), " - Pobieranie wynagrodzeń dla powiatów.", sep = "")
    wskaznikiPow <- wskaznik_bdl(64428, "przeciętne miesięczne wynagrodzenia brutto") %>%
      pobierz_dane_bdl(bdl_lata, "powiaty") %>% 
      przeksztalc_dane_bdl(bdl_lata[1]) %>% 
      rename(nazwaJst = powiat) %>% 
      mutate(across(where(is.double),
                    ~ifelse(. == 0, NA_real_, .))) %>% 
      mutate(teryt = floor(teryt / 100))
    names(wskaznikiPow) <- c("teryt", "powiat", paste0("wynagrodzenie_", bdl_lata))
    
    bdl_braki = sapply(wskaznikiPow[,3:ncol(wskaznikiPow)], function(col) sum(is.na(col)))
    
    if (sum(bdl_braki) == 0) {
      wskaznikiPow <- wskaznikiPow %>% 
        select(teryt, ncol(.)) %>% 
        rename(powiat_sr_wynagrodzenie = ncol(.)) %>% 
        mutate(teryt = teryt * 100)
      p3 <- p3 %>% 
        select(-powiat_sr_wynagrodzenie) %>% 
        left_join(wskaznikiPow %>% select(teryt, powiat_sr_wynagrodzenie = wynagrodzenie_2023),
                  join_by(teryt_pow_szk == teryt))
      if (sum(is.na(p3$powiat_sr_wynagrodzenie)) == 0) {
        save(p3, file = tabela_p3)
        cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dołączono dane o wynagrodzeniach z BDL.", sep = "")
      } else {
        stop("\nDołączono pełne dane z BDL, ale nadal wsytępuja braki w `p3$powiat_sr_wynagrodzenie`.")
      }
    } else {
      # zgodnie z konwencją nie powinienem nazywać kolumn wartością liczbową, ale mogę nazwać jak w `make.names()`, czyli z  "X" na początku np. `X2023`
      cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dołączone dane z BDL zawierały braki. O ile to możliwe, dane zostaną uzupełnione na podstawie danych wojewódzkich (szczegóły imputacji opisano w dokumentacji funkcji).\nPobieranie wynagrodzeń dla województw.", sep = "")
      wskaznikiWoj <- wskaznik_bdl(64428, "przeciętne miesięczne wynagrodzenia brutto") %>%
        pobierz_dane_bdl(bdl_lata, "województwa") %>% 
        przeksztalc_dane_bdl(bdl_lata[1]) %>% 
        mutate(across(where(is.double),
                      ~ifelse(. == 0, NA_real_, .))) %>% 
        mutate(teryt = floor(teryt / 100))
      names(wskaznikiWoj) <- c("teryt", "nazwaJst", paste0("wynagrodzenie_", bdl_lata))
      
      brakujacePow <- wskaznikiPow %>% 
        filter(is.na(wynagrodzenie_2019) |
                 is.na(wynagrodzenie_2020) |
                 is.na(wynagrodzenie_2021) |
                 is.na(wynagrodzenie_2022) |
                 is.na(wynagrodzenie_2023)) %>% 
        mutate(teryt_woj = floor(teryt / 100))
      
      wskaznikiWoj <- wskaznikiWoj %>% 
        mutate(teryt = floor(teryt / 100)) %>% 
        filter(teryt %in% brakujacePow$teryt_woj) %>% 
        pivot_longer(cols = starts_with("wynagrodzenie"), names_to = "rok", values_to = "wynagrodzenie") %>%
        mutate(rok = as.integer(sub("wynagrodzenie_", "", rok))) %>%
        group_by(nazwaJst) %>%
        mutate(zmiana = wynagrodzenie / lag(wynagrodzenie)) %>%
        ungroup()
      
      brakujacePow <- brakujacePow %>% 
        pivot_longer(cols = starts_with("wynagrodzenie"), names_to = "rok", values_to = "wynagrodzenie") %>%
        mutate(rok = as.integer(sub("wynagrodzenie_", "", rok))) %>% 
        left_join(wskaznikiWoj %>% 
                    select(teryt, rok, zmiana),
                  join_by(teryt_woj == teryt, rok)) %>% 
        group_by(teryt) %>% 
        mutate(wynagrodzenie = ifelse(is.na(wynagrodzenie), lag(wynagrodzenie) * zmiana, wynagrodzenie),
               wynagrodzenie = ifelse(is.na(wynagrodzenie), lag(wynagrodzenie) * zmiana, wynagrodzenie),
               wynagrodzenie = ifelse(is.na(wynagrodzenie), lag(wynagrodzenie) * zmiana, wynagrodzenie),
               wynagrodzenie = ifelse(is.na(wynagrodzenie), lag(wynagrodzenie) * zmiana, wynagrodzenie)) %>% 
        ungroup() %>% 
        select(-c(zmiana, teryt_woj)) %>% 
        pivot_wider(names_from = rok, names_prefix = "wynagrodzenie_", values_from = wynagrodzenie)
      
      wskaznikiPow <- wskaznikiPow %>% 
        filter(!teryt %in% brakujacePow$teryt) %>% 
        rbind(., brakujacePow) %>% 
        arrange(teryt) %>% 
        select(teryt, powiat_sr_wynagrodzenie = wynagrodzenie_2023)
      
      p3 <- p3 %>% 
        # mutate(teryt_pow_szk = floor(teryt_pow_szk / 100)) %>% 
        select(-powiat_sr_wynagrodzenie) %>%
        left_join(wskaznikiPow,
                  join_by(teryt_pow_szk == teryt))
      
      if (sum(is.na(p3$powiat_sr_wynagrodzenie)) == 0) {
        return(p3)
        cat("\n", format(Sys.time(), "%H:%M:%S"), " - Dołączono dane o wynagrodzeniach z BDL.", sep = "")
      } else {
        stop("\nDołączono dane z BDL po imputacji ze względu na dynamikę zmiany wynagrodzenia w województwie, ale nadal wsytępuja braki w `p3$powiat_sr_wynagrodzenie`.")
      }
    }
  }
}
