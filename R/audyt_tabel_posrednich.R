#' @title Sprawdzanie poprawności nazewnictwa tabel pośrednich
#' @description Funkcja zwraca listę nazw kolumn w poszczególnych tabelach
#' pośrenich oraz odpowiadających im klas na potrzeby dalszego sprawdzania czy
#' tabela pośrednia ma wymagane kolumny oraz czy mają one odpowiednią klasę. W
#' przypadku zmiany nazw kolumn (i ich klas) w kolejnych edycjach monitoringu
#' można w tym miejscu zaktualizować to mapowanie.
#' @return lista nazw kolumn w poszczególnych tabelach pośrenich oraz
#' odpowiadających im klas
kolumny_tabele_posrednie <- function() {
  def_col <- function(type, values = NULL, jestNA = FALSE) {
    list(
      type = type,
      values = values,
      jestNA = jestNA
    )
  }

  return(list(
    "p1" = list(
      "id_abs" = def_col("integer"),
      "rok_abs" = def_col("integer"),
      "kod_zaw" = def_col("integer", jestNA = TRUE),
      "branza" = def_col("character", jestNA = TRUE),
      "rok" = def_col("numeric", jestNA = TRUE),
      "miesiac" = def_col("numeric", jestNA = TRUE),
      "mies_od_ukoncz" = def_col("numeric", jestNA = TRUE),
      "okres" = def_col("numeric", jestNA = TRUE),
      "rodzaj_dyplomu" = def_col(
        "factor",
        values = c(
          "tytuł czeladnika",
          "certyfikat kwalifikacji",
          "dyplom zawodowy",
          "matura",
          "dyplom licencjata/inżyniera",
          "dyplom magistra/lekarza",
          "dyplom oficera"
        )
      ),
      "kod_zaw_dyplom" = def_col("integer", jestNA = TRUE),
      "branza_dyplom" = def_col("character", jestNA = TRUE),
      "dyplom_szczegoly" = def_col("character", jestNA = TRUE),
      "dziedzina" = def_col("character", jestNA = TRUE),
      "dyscyplina_wiodaca" = def_col("character", jestNA = TRUE),
      "lp_dyplom" = def_col("integer")
    ),
    "p2" = list(
      "id_abs" = def_col("integer"),
      "rok_abs" = def_col("integer"),
      "kod_zaw" = def_col("integer", jestNA = TRUE),
      "branza" = def_col("character", jestNA = TRUE),
      "rok" = def_col("numeric"),
      "miesiac" = def_col("numeric"),
      "mies_od_ukoncz" = def_col("numeric"),
      "okres" = def_col("integer"),
      "id_szk_kont" = def_col("integer", jestNA = TRUE),
      "typ_szk_kont" = def_col(
        "character",
        values = c(
          "Szkoła policealna",
          "Branżowa szkoła II stopnia",
          "Studia",
          "Centrum Kształcenia Zawodowego",
          "Szkoła specjalna przysposabiająca do pracy",
          "Liceum dla dorosłych",
          "Placówka Kształcenia Ustawicznego - bez szkół",
          "Technikum",
          "Placówka Kształcenia Ustawicznego ze szkołami",
          "Policealna szkoła plastyczna",
          "Branżowa szkoła I stopnia",
          "Szkoła muzyczna I stopnia",
          "Szkoła muzyczna II stopnia",
          "Kolegium Pracowników Służb Społecznych",
          "Policealna szkoła muzyczna",
          "Liceum ogólnokształcące",
          "Inna szkoła artystyczna",
          "Szkoła sztuki cyrkowej",
          "Liceum sztuk plastycznych"
        )
      ),
      "forma_kont" = def_col(
        "factor",
        values = c(
          "uczeń",
          "KKZ",
          "KUZ",
          "student"
        )
      ),
      "teryt_pow_kont" = def_col("numeric", jestNA = TRUE),
      "kod_zaw_kont" = def_col("integer", jestNA = TRUE),
      "branza_kont" = def_col("character", jestNA = TRUE),
      "zrodlo" = def_col(
        "character",
        values = c("W3", "W4", "W5", "W12")
      ),
      "dziedzina_kont" = def_col("character", jestNA = TRUE),
      "dyscyplina_wiodaca_kont" = def_col("character", jestNA = TRUE),
      "lp_kont" = def_col("integer")
    ),
    "p3" = list(
      "id_abs" = def_col("integer"),
      "rok_abs" = def_col("integer"),
      "rok" = def_col("numeric"),
      "miesiac" = def_col("numeric"),
      "mies_od_ukoncz" = def_col("numeric"),
      "okres" = def_col("integer"),
      "status" = def_col(
        "factor",
        values = c(
          "Tylko nauka",
          "Nauka i praca",
          "Tylko praca",
          "Bezrobocie",
          "Brak danych o aktywności"
        )
      ),
      "zmarl" = def_col("logical"),
      "brak_danych_z_zus" = def_col("logical"),
      "praca" = def_col(
        "integer",
        values = c(NA, 0L:7L),
        jestNA = TRUE
      ),
      "mlodociany" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "kont_mlodoc_prac" = def_col(
        "integer",
        values = c(NA, 1L:6L),
        jestNA = TRUE
      ),
      "bezrobocie" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "bezrobocie_staz" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "dziecko" = def_col(
        "integer",
        values = c(NA, 0L:2L),
        jestNA = TRUE
      ),
      "macierz" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "wychow" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "pomoc_spol" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "emeryt_rencista" = def_col(
        "integer",
        values = c(NA, 0L:2L),
        jestNA = TRUE
      ),
      "niepelnosprawny" = def_col(
        "integer",
        values = c(NA, 0L:4L),
        jestNA = TRUE
      ),
      "wynagrodzenie" = def_col("numeric", jestNA = TRUE),
      "wynagrodzenie_uop" = def_col("numeric", jestNA = TRUE),
      "wypadek" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "choroba" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "choroba_macierz" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "teryt_zam" = def_col("numeric", jestNA = TRUE),
      "powiat_bezrobocie" = def_col("numeric", jestNA = TRUE),
      "powiat_sr_wynagrodzenie" = def_col("numeric", jestNA = TRUE),
      "biernosc_zus" = def_col(
        "integer",
        values = c(NA, 0L:1L),
        jestNA = TRUE
      ),
      "nauka" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka2" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_szk_abs" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_bs1st" = def_col(
        "integer",
        values = c(0L:2L)
      ),
      "nauka_bs2st" = def_col(
        "integer",
        values = c(0L:2L)
      ),
      "nauka_technikum" = def_col(
        "integer",
        values = c(0L:2L)
      ),
      "nauka_lo" = def_col(
        "integer",
        values = c(0L:3L)
      ),
      "nauka_spolic" = def_col(
        "integer",
        values = c(0L:4L)
      ),
      "nauka_artystyczna" = def_col(
        "integer",
        values = c(0L:8L)
      ),
      "nauka_sspdp" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_kpsp" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_studia" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_kkz" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "nauka_kuz" = def_col(
        "integer",
        values = c(0L:1L)
      ),
      "wynagrodzenie_oryg" = def_col(
        "numeric",
        jestNA = TRUE
      ),
      "wynagrodzenie_uop_oryg" = def_col(
        "numeric",
        jestNA = TRUE
      )
    ),
    "p4" = list(
      "id_abs" = def_col("integer"),
      "rok_abs" = def_col("integer"),
      "duplikat_w_szkole" = def_col("logical"),
      "duplikat_wiele_szkol" = def_col("logical"),
      "rok_ur" = def_col("integer"),
      "plec" = def_col(
        "factor",
        values = c("K", "M")
      ),
      "id_szk" = def_col("integer"),
      "mlodoc_byl" = def_col("logical"),
      "typ_szk" = def_col(
        "factor",
        values = c(
          "Technikum",
          "Liceum ogólnokształcące",
          "Branżowa szkoła I stopnia",
          "Szkoła policealna",
          "Liceum dla dorosłych",
          "Szkoła specjalna przysposabiająca do pracy",
          "Branżowa szkoła II stopnia"
        )
      ),
      "szk_specjalna" = def_col("logical"),
      "typ_szk_mlodoc" = def_col(
        "factor",
        values = c(
          "Technikum",
          "Liceum ogólnokształcące",
          "Niemłodociani w Branżowej szkole I stopnia",
          "Młodociani w Branżowej szkole I stopnia",
          "Szkoła policealna",
          "Liceum dla dorosłych",
          "Szkoła specjalna przysposabiająca do pracy",
          "Branżowa szkoła II stopnia"
        )
      ),
      "teryt_pow_szk" = def_col("integer"),
      "nazwa_pow_szk" = def_col("character"),
      "teryt_woj_szk" = def_col("integer"),
      "nazwa_woj_szk" = def_col("character"),
      "nazwa_makroreg_szk" = def_col("character"),
      "nazwa_reg_szk" = def_col("character"),
      "nazwa_podreg_szk" = def_col("character"),
      "nts_podreg_szk" = def_col("integer"),
      "lp" = def_col("integer"),
      "kod_zaw" = def_col("integer", jestNA = TRUE),
      "nazwa_zaw" = def_col("character", jestNA = TRUE),
      "branza" = def_col("character", jestNA = TRUE),
      "kod_isced" = def_col("character", jestNA = TRUE),
      "grupa_isced" = def_col("character", jestNA = TRUE),
      "podgrupa_isced" = def_col("character", jestNA = TRUE),
      "nazwa_isced" = def_col("character", jestNA = TRUE),
      "l_prac_ucz_uop" = def_col("integer"),
      "l_prac_nucz_uop" = def_col("integer"),
      "l_prac_nucz_nuop" = def_col("integer"),
      "zawod_sr_wynagrodzenie" = def_col("numeric"),
      "abs_w_cke" = def_col("logical"),
      "abs_w_sio" = def_col("logical"),
      "abs_w_polon" = def_col("logical"),
      "abs_w_zus" = def_col("logical"),
      "matura_zdana" = def_col(
        "integer",
        values = c(0, 1)
      ),
      "dyplom_zaw" = def_col(
        "factor",
        values = c(
          NA,
          "Świadectwo czeladnicze",
          "Dyplom zawodowy",
          "Tylko certyfikat kwalifikacji",
          "Brak certyfikatów i dyplomu"
        ),
        jestNA = TRUE
      ),
      "typ_szk_kont6" = def_col(c("matrix", "array"), jestNA = TRUE),
      "typ_szk_kont18" = def_col(c("matrix", "array"), jestNA = TRUE),
      "typ_szk_kont30" = def_col(c("matrix", "array"), jestNA = TRUE),
      "typ_szk_kont42" = def_col(c("matrix", "array"), jestNA = TRUE),
      "typ_szk_kont54" = def_col(c("matrix", "array"), jestNA = TRUE),
      "branza_kont_bsii6" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dziedzina_kont6" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dziedzina_kont18" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dziedzina_kont54" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dyscyplina_kont6" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dyscyplina_kont18" = def_col(c("matrix", "array"), jestNA = TRUE),
      "dyscyplina_kont54" = def_col(c("matrix", "array"), jestNA = TRUE),
      "sr_wynagr_r0_ivkw" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r1" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r1_ikw" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r2_ikw" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r2" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r3" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_r4" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_uop_nauka_r0_wrzgru" = def_col("numeric", jestNA = TRUE),
      "sr_wynagr_uop_bez_nauki_r0_wrzgru" = def_col("numeric", jestNA = TRUE),
      "praca_nauka_r0_wrzgru" = def_col(
        "factor",
        values = c(
          "Brak pracy",
          "Praca przez część okresu",
          "Praca przez cały okres"
        )
      ),
      "praca_bez_nauki_r0_wrzgru" = def_col(
        "factor",
        values = c(
          "Brak pracy",
          "Praca przez część okresu",
          "Praca przez cały okres"
        )
      ),
      "bezrobocie_r0_wrzgru" = def_col(
        "factor",
        values = c(
          "Brak bezrobocia",
          "1 miesiąc",
          "2 miesiące",
          "3 miesiące",
          "4 miesiące"
        )
      )
    ),
    "p5" = list(
      "id_abs" = def_col("integer"),
      "rok_abs" = def_col("integer", values = 2019:2024),
      "rok" = def_col("integer"),
      "miesiac" = def_col("integer"),
      "mies_od_ukoncz" = def_col("numeric"),
      "okres" = def_col("integer"),
      "lp_pracod" = def_col("numeric"),
      "pkd_pracod" = def_col("character", jestNA = TRUE),
      "forma_zatrudnienia" = def_col("integer"),
      "mlodociany" = def_col("integer"),
      "wynagrodzenie" = def_col("numeric"),
      "wynagrodzenie_uop" = def_col("numeric", jestNA = TRUE)
    ),
    "p6" = list(
      "id_szk" = def_col("integer"),
      "szk_ma_abs" = def_col("logical"),
      "typ_szk" = def_col("character"),
      "typ_szk_sdd" = def_col("character"),
      "typ_szk_rspo" = def_col("character"),
      "publicznosc" = def_col("character"),
      "kategoria_uczniow" = def_col("character"),
      "specyfika" = def_col("character"),
      "organ_rejestrujacy_id" = def_col("integer"),
      "organ_rejestrujacy_typ" = def_col("character"),
      "organ_rejestrujacy_nazwa" = def_col("character"),
      "organ_rejestrujacy_teryt" = def_col("integer", jestNA = TRUE),
      "organ_sposob_ewidencjonowania" = def_col("character"),
      "rok_szk" = def_col("character"),
      "nazwa_szk" = def_col("character"),
      "teryt_gmi_szk" = def_col("integer"),
      "wojewodztwo_szk" = def_col("character"),
      "powiat_szk" = def_col("character"),
      "gmina_szk" = def_col("character"),
      "simc_miejsc" = def_col("integer"),
      "miejscowosc" = def_col("character"),
      "rodzaj_miejsc" = def_col("character"),
      "sym_ul" = def_col("integer", jestNA = TRUE),
      "ulica" = def_col("character", jestNA = TRUE),
      "nr_budynku" = def_col("character"),
      "nr_lokalu" = def_col("character", jestNA = TRUE),
      "pna" = def_col("character"),
      "poczta" = def_col("character"),
      "organ_prowadzacy_typ" = def_col("character"),
      "organ_prowadzacy_nazwa" = def_col("character"),
      "organ_prowadzacy_regon" = def_col("character", jestNA = TRUE),
      "organ_prowadzacy_teryt" = def_col("numeric"),
      "organ_prowadzacy_woj" = def_col("character"),
      "organ_prowadzacy_pow" = def_col("character"),
      "organ_prowadzacy_gmi" = def_col("character"),
      "miejsce_w_strukt" = def_col("character"),
      "jedn_nadrz_id" = def_col("integer", jestNA = TRUE),
      "jedn_nadrz_typ" = def_col("character", jestNA = TRUE)
    )
  ))
}
#' @title Sprawdzanie poprawności nazewnictwa tabel pośrednich
#' @description 
#' @param df obiekt-tabela pośrednia
#' @param tabela_p rodzaj tabeli pośredniej
#' @param force_zgodnosc_nazw wartość logiczna określająca czy nazwa obiektu
#' przekazana w argumencie `df` wygląda na zgodną z rodzajem tabeli pośredniej
#' np. jeśli w tym argumencie wpisano wartość "p2", to obiekt przekazany do
#' argumentu `df` powinien mieć w nazwie cyfrę 2 np. "p2" lub "tab_p2", etc.
#' @return logical
#' @export
is_wymagane_kolumny <- function(df, tabela_p = c("p1", "p2", "p3", "p4", "p5", "p6"), force_zgodnosc_nazw = TRUE) {
  stopifnot(
    is.data.frame(df),
    nrow(df) > 0L,
    "Argument `tabela_p` ma nieprawidłową wartość." = tabela_p %in% c("p1", "p2", "p3", "p4", "p5", "p6"),
    length(tabela_p) > 0L,
    is.logical(force_zgodnosc_nazw),
    force_zgodnosc_nazw %in% c(TRUE, FALSE),
    length(force_zgodnosc_nazw) == 1L
  )
  nazwa_obiektu <- deparse(substitute(df))
  
  tabela_p <- match.arg(tabela_p)

  if (force_zgodnosc_nazw) {
    numer_tabeli <- gsub("p\\D", "", tabela_p)
    
    if (!grepl(numer_tabeli, nazwa_obiektu)) {
      stop(paste0(
        "Błąd: Nazwa przekazanej tabeli to: `", nazwa_obiektu, 
        "', a zestaw kolumn i ich klas (argument `tabela_p`) ma wartość: '",
        tabela_p, ".\nAby pominąć sprawdzanie nazwy tabeli pośredniej argument `force_zgodnosc_nazw` powinien mieć wartość FALSE."
      ))
    }
  }

  mapowanie <- kolumny_tabele_posrednie()[[tabela_p]]

  brakujace_kolumny <- setdiff(names(mapowanie), names(df))
  if (length(brakujace_kolumny) > 0L) {
    warning(
      paste0(
        "Przekazana tabela nie ma wszystkich wymaganych kolumn. Dla tabeli ",
        tabela_p,
        " brakuje następujących kolumn: ",
        paste(brakujace_kolumny, collapse = ", ")
      )
    )
    return(FALSE)
  } else {
    return(TRUE)
  }
}
#' @title Sprawdzanie poprawności oczekiwanych typów zmiennych tabel pośrednich
#' @description 
#' @param df obiekt-tabela pośrednia
#' @param tabela_p rodzaj tabeli pośredniej
#' @param force_zgodnosc_nazw wartość logiczna określająca czy nazwa obiektu
#' przekazana w argumencie `df` wygląda na zgodną z rodzajem tabeli pośredniej
#' np. jeśli w tym argumencie wpisano wartość "p2", to obiekt przekazany do
#' argumentu `df` powinien mieć w nazwie cyfrę 2 np. "p2" lub "tab_p2", etc.
#' @return logical
#' @export
is_klasa_kolumny <- function(df, tabela_p = c("p1", "p2", "p3", "p4", "p5", "p6"), force_zgodnosc_nazw = TRUE) {
  stopifnot(
    is.data.frame(df),
    nrow(df) > 0L,
    tabela_p %in% c("p1", "p2", "p3", "p4", "p5", "p6"),
    length(tabela_p) > 0L,
    is.logical(force_zgodnosc_nazw),
    force_zgodnosc_nazw %in% c(TRUE, FALSE),
    length(force_zgodnosc_nazw) == 1L
  )
  nazwa_obiektu <- deparse(substitute(df))
  
  tabela_p <- match.arg(tabela_p)

  if (force_zgodnosc_nazw) {
    numer_tabeli <- gsub("\\D", "", tabela_p)
    
    if (!grepl(numer_tabeli, nazwa_obiektu)) {
      stop(paste0(
        "Błąd: Nazwa przekazanej tabeli to: `", nazwa_obiektu, 
        "', a zestaw kolumn i ich klas (argument `tabela_p`) ma wartość: '",
        tabela_p, ".\nAby pominąć sprawdzanie nazwy tabeli pośredniej argument `force_zgodnosc_nazw` powinien mieć wartość FALSE."
      ))
    }
  }
  
  mapowanie <- kolumny_tabele_posrednie()[[tabela_p]]

  zla_klasa <- c()
  for (kol in names(mapowanie)) {
    if (kol %in% names(df)) {
      if (!inherits(df[[kol]], mapowanie[[kol]]$type)) {
        warning(sprintf("Kolumna %s: oczekiwano %s, jest %s", 
                        kol, mapowanie[[kol]]$type, class(df[[kol]])))
        
        zla_klasa <- paste(zla_klasa, kol)
      }
    }
  }
  
  if (length(zla_klasa) > 0L) {
    print(paste(
      "Następujące kolumny mają inną, niż przewidziana klasę:",
      zla_klasa,
      sep = ", "
    ))
    return(FALSE)
  } else {
    return(TRUE)
  }
}
#' @title Sprawdzanie poprawności oczekiwanych wartości zmiennych tabel
#' pośrednich
#' @description 
#' @param df obiekt-tabela pośrednia
#' @param tabela_p rodzaj tabeli pośredniej
#' @param force_zgodnosc_nazw wartość logiczna określająca czy nazwa obiektu
#' przekazana w argumencie `df` wygląda na zgodną z rodzajem tabeli pośredniej
#' np. jeśli w tym argumencie wpisano wartość "p2", to obiekt przekazany do
#' argumentu `df` powinien mieć w nazwie cyfrę 2 np. "p2" lub "tab_p2", etc.
#' @return logical
#' @export
is_dozwolone_wartosci <- function(df, tabela_p = c("p1", "p2", "p3", "p4", "p5", "p6"), force_zgodnosc_nazw = TRUE) {
  stopifnot(
    is.data.frame(df),
    nrow(df) > 0L,
    tabela_p %in% c("p1", "p2", "p3", "p4", "p5", "p6"),
    length(tabela_p) > 0L,
    is.logical(force_zgodnosc_nazw),
    force_zgodnosc_nazw %in% c(TRUE, FALSE),
    length(force_zgodnosc_nazw) == 1L
  )
  nazwa_obiektu <- deparse(substitute(df))
  
  tabela_p <- match.arg(tabela_p)

  if (force_zgodnosc_nazw) {
    numer_tabeli <- gsub("\\D", "", tabela_p)
    
    if (!grepl(numer_tabeli, nazwa_obiektu)) {
      stop(paste0(
        "Błąd: Nazwa przekazanej tabeli to: `", nazwa_obiektu, 
        "', a zestaw kolumn i ich klas (argument `tabela_p`) ma wartość: '",
        tabela_p, ".\nAby pominąć sprawdzanie nazwy tabeli pośredniej argument `force_zgodnosc_nazw` powinien mieć wartość FALSE."
      ))
    }
  }
  
  mapowanie <- kolumny_tabele_posrednie()[[tabela_p]]

  niedopuszczalna_wart <- c()
  for (kol in names(mapowanie)) {
    if (!is.null(mapowanie[[kol]]$values)) {
      niedopuszczalna_wart <- setdiff(unique(df[[kol]]), mapowanie[[kol]]$values)
      if (length(niedopuszczalna_wart) > 0) {
        warning(sprintf("Kolumna %s: niedozwolone wartości: %s", 
                        kol, paste(niedopuszczalna_wart, collapse = ", ")))
        niedopuszczalna_wart <- paste(niedopuszczalna_wart, kol)
      }
    }
  }
  
  if (length(niedopuszczalna_wart) > 0L) {
    print(paste(
      "Następujące kolumny mają inną, niż przewidziana klasę:",
      niedopuszczalna_wart,
      sep = ", "
    ))
    return(FALSE)
  } else {
    return(TRUE)
  }
}