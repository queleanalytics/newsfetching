
set.seed(1313)
rm(list = ls())

# Charger les packages nécessaires
if (!requireNamespace("xml2", quietly = TRUE)) install.packages("xml2")
if (!requireNamespace("rvest", quietly = TRUE)) install.packages("rvest")
if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
library(xml2)
library(rvest)
library(httr)

# Initialisation du tableau final complet
tous_les_articles <- data.frame(
  Source = character(),
  Language = character(),
  Date = character(),
  Title = character(),
  URL = character(),
  stringsAsFactors = FALSE
)

# Configuration du User-Agent
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
date_aujourdhui <- as.character(Sys.Date())

# Fonction interne pour formater proprement les dates RSS (pubDate)
extraire_date_rss <- function(nodes) {
  dates_brutes <- xml_text(xml_find_first(nodes, ".//pubDate"))
  # Conversion standardisée YYYY-MM-DD. Si échec, retourne la date du jour.
  dates_clean <- as.character(as.Date(strptime(dates_brutes, "%a, %d %b %Y %H:%M:%S")))
  dates_clean[is.na(dates_clean)] <- date_aujourdhui
  return(dates_clean)
}

# ==============================================================================
# 1. VRT NWS (Anglais - Web Scraping)
# ==============================================================================
vrt_web_url <- "https://vrt.be"
tryCatch({
  vrt_page <- read_html(vrt_web_url)
  vrt_nodes <- html_nodes(vrt_page, "a")
  vrt_df <- data.frame(
    Source = "VRT NWS (EN)",
    Language = "EN",
    Date = date_aujourdhui,
    Title = html_text(vrt_nodes, trim = TRUE),
    URL = html_attr(vrt_nodes, "href"),
    stringsAsFactors = FALSE
  )
  vrt_clean <- vrt_df[grep("/202[0-9]/", vrt_df$URL), ]
  vrt_clean <- vrt_clean[vrt_clean$Title != "", ]
  tous_les_articles <- rbind(tous_les_articles, unique(vrt_clean))
  cat("✔ VRT NWS : ", nrow(vrt_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec VRT : ", e$message, "\n") })

# ==============================================================================
# 2. RTBF ACTUS (Français - Web Scraping)
# ==============================================================================
rtbf_web_url <- "https://rtbf.be"
tryCatch({
  rtbf_page <- read_html(rtbf_web_url)
  rtbf_nodes <- html_nodes(rtbf_page, "h3")
  rtbf_titles <- html_text(rtbf_nodes, trim = TRUE)
  rtbf_links <- html_attr(html_nodes(rtbf_page, "h3 a"), "href")
  if (length(rtbf_links) == 0) {
    rtbf_links <- html_attr(html_nodes(rtbf_page, "a:has(h3)"), "href")
  }
  valides <- rtbf_titles != ""
  rtbf_titles <- rtbf_titles[valides]
  rtbf_clean <- data.frame(
    Source = "RTBF Actus (FR)",
    Language = "FR",
    Date = date_aujourdhui,
    Title = head(rtbf_titles, length(rtbf_links)),
    URL = head(rtbf_links, length(rtbf_titles)),
    stringsAsFactors = FALSE
  )
  tous_les_articles <- rbind(tous_les_articles, unique(rtbf_clean))
  cat("✔ RTBF Actus : ", nrow(rtbf_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec RTBF : ", e$message, "\n") })



# ==============================================================================
# 3. LE SOIR (Bypass ultime via l'index européen ouvert Eurotopics)
# ==============================================================================
tryCatch({
  # Page publique de revue de presse d'Eurotopics dédiée au journal Le Soir
  lesoir_eurotopics_url <- "https://eurotopics.net"
  
  # Lecture de la page avec notre User-Agent standard
  response_euro <- GET(lesoir_eurotopics_url, ua, config(ssl_verifypeer = FALSE))
  
  if (status_code(response_euro) == 200) {
    euro_page <- read_html(content(response_euro, as = "text", encoding = "UTF-8"))
    
    # Extraction des titres et des liens d'actualités liés au Soir
    titles_nodes <- html_nodes(euro_page, ".article-title, h4, .teaser-title")
    links_nodes  <- html_nodes(euro_page, "a[href*='/fr/search/']")
    
    # Extraction du texte
    titles_ls <- html_text(titles_nodes, trim = TRUE)
    
    # Si les sélecteurs précis échouent, on prend tous les liens de la zone éditoriale
    if (length(titles_ls) == 0) {
      all_links <- html_nodes(euro_page, "a")
      titles_ls <- html_text(all_links, trim = TRUE)
      urls_ls   <- html_attr(all_links, "href")
    } else {
      urls_ls <- html_attr(html_nodes(euro_page, "a"), "href")
    }
    
    lesoir_df <- data.frame(
      Source = "Le Soir (FR)",
      Language = "FR",
      Date = date_aujourdhui,
      Title = titles_ls,
      URL = urls_ls,
      stringsAsFactors = FALSE
    )
    
    # Nettoyage strict des données pour ne garder que les lignes valides
    lesoir_clean <- lesoir_df[lesoir_df$Title != "" & !is.na(lesoir_df$URL), ]
    lesoir_clean <- lesoir_clean[grep("/fr/", lesoir_clean$URL), ]
    
    # Reconstruire les liens absolus si nécessaire
    idx_relative <- !grepl("^http", lesoir_clean$URL)
    lesoir_clean$URL[idx_relative] <- paste0("https://eurotopics.net", lesoir_clean$URL)
    lesoir_clean <- unique(lesoir_clean)
    
    if (nrow(lesoir_clean) > 0) {
      tous_les_articles <- rbind(tous_les_articles, lesoir_clean)
      cat("✔ Le Soir (FR) :", nrow(lesoir_clean), "articles récupérés (Via Eurotopics EU).\n")
    } else {
      cat("⚠ Le Soir (FR) : Connexion réussie mais structure de la page modifiée.\n")
    }
  } else {
    cat("❌ Le Soir (FR) : Eurotopics a répondu avec le code :", status_code(response_euro), "\n")
  }
}, error = function(e) { 
  cat("❌ Échec critique Le Soir via Eurotopics : ", e$message, "\n") 
})



# ==============================================================================
# 4. FRANCE 24 (Anglais - Solution 2 Scraping HTML Corrigée)
# ==============================================================================
tryCatch({
  # 1. Recréer la variable de date si elle est manquante
  date_aujourdhui <- as.character(Sys.Date())
  
  f24_web_url <- "https://france24.com"
  
  # 2. Lecture tolérante du code HTML
  f24_page <- read_html(f24_web_url)
  f24_nodes <- html_nodes(f24_page, "a")
  
  f24_df <- data.frame(
    Source = "France 24 (EN)",
    Language = "EN",
    Date = date_aujourdhui,
    Title = html_text(f24_nodes, trim = TRUE),
    URL = html_attr(f24_nodes, "href"),
    stringsAsFactors = FALSE
  )
  
  # 3. Filtrer pour ne garder que les vrais liens d'articles (contenant la date)
  f24_clean <- f24_df[grep("/202[0-9]", f24_df$URL), ]
  f24_clean <- f24_clean[f24_clean$Title != "", ]
  f24_clean <- unique(f24_clean)
  
  # 4. Reconstruire les liens absolus si nécessaire
  if (nrow(f24_clean) > 0) {
    idx_relative <- !grepl("^http", f24_clean$URL)
    f24_clean$URL[idx_relative] <- paste0("https://france24.com", f24_clean$URL)
    
    tous_les_articles <- rbind(tous_les_articles, f24_clean)
    cat("✔ France 24 : ", nrow(f24_clean), " articles récupérés (Scraping HTML).\n")
  } else {
    cat("⚠ France 24 : Connexion réussie mais aucun article trouvé avec ce filtre.\n")
  }
  
}, error = function(e) { 
  cat("❌ Échec France 24 : ", e$message, "\n") 
})


# ==============================================================================
# 5. LE MONDE (Français - Solution Scraping HTML Direct)
# ==============================================================================
tryCatch({
  # 1. S'assurer que la variable de date existe
  if (!exists("date_aujourdhui")) {
    date_aujourdhui <- as.character(Sys.Date())
  }
  
  lemonde_web_url <- "https://lemonde.fr"
  
  # 2. Lecture tolérante du code HTML de la page d'accueil
  lemonde_page <- read_html(lemonde_web_url)
  
  # 3. Extraire tous les nœuds de liens de la page
  lemonde_nodes <- html_nodes(lemonde_page, "a")
  
  lemonde_df <- data.frame(
    Source = "Le Monde (FR)",
    Language = "FR",
    Date = date_aujourdhui,
    Title = html_text(lemonde_nodes, trim = TRUE),
    URL = html_attr(lemonde_nodes, "href"),
    stringsAsFactors = FALSE
  )
  
  # 4. Filtrer : Ne garder que les liens contenant des articles d'actualité.
  # Sur Le Monde, les articles contiennent la mention "/article/" ou une date.
  lemonde_clean <- lemonde_df[grep("/article/|/en-direct/", lemonde_df$URL), ]
  lemonde_clean <- lemonde_clean[lemonde_clean$Title != "", ]
  lemonde_clean <- unique(lemonde_clean)
  
  # 5. Reconstruire les liens absolus pour les adresses relatives
  if (nrow(lemonde_clean) > 0) {
    idx_relative <- !grepl("^http", lemonde_clean$URL)
    lemonde_clean$URL[idx_relative] <- paste0("https://www.lemonde.fr", lemonde_clean$URL)
    
    tous_les_articles <- rbind(tous_les_articles, lemonde_clean)
    cat("✔ Le Monde : ", nrow(lemonde_clean), " articles récupérés (Scraping HTML).\n")
  } else {
    cat("⚠ Le Monde : Connexion réussie mais aucun article trouvé avec ce filtre.\n")
  }
  
}, error = function(e) { 
  cat("❌ Échec Le Monde : ", e$message, "\n") 
})

# ==============================================================================
# 6. RFI (Français - Solution Scraping HTML Direct Aligné)
# ==============================================================================
tryCatch({
  # 1. S'assurer que la variable de date existe
  if (!exists("date_aujourdhui")) {
    date_aujourdhui <- as.character(Sys.Date())
  }
  
  rfi_web_url <- "https://rfi.fr"
  
  # 2. Lecture tolérante du code HTML de la page d'accueil
  rfi_page <- read_html(rfi_web_url)
  
  # 3. Extraire tous les nœuds de liens de la page
  rfi_nodes <- html_nodes(rfi_page, "a")
  
  rfi_df <- data.frame(
    Source = "RFI Afrique (FR)",
    Language = "FR",
    Date = date_aujourdhui,
    Title = html_text(rfi_nodes, trim = TRUE),
    URL = html_attr(rfi_nodes, "href"),
    stringsAsFactors = FALSE
  )
  
  # 4. Filtrer : Ne garder que les liens contenant des articles d'actualité.
  # Sur RFI, les articles contiennent la mention de l'année (/2026/).
  rfi_clean <- rfi_df[grep("/202[0-9]/", rfi_df$URL), ]
  rfi_clean <- rfi_clean[rfi_clean$Title != "", ]
  rfi_clean <- unique(rfi_clean)
  
  # 5. Reconstruire les liens absolus pour les adresses relatives
  if (nrow(rfi_clean) > 0) {
    idx_relative <- !grepl("^http", rfi_clean$URL)
    rfi_clean$URL[idx_relative] <- paste0("https://www.rfi.fr", rfi_clean$URL)
    
    tous_les_articles <- rbind(tous_les_articles, rfi_clean)
    cat("✔ RFI : ", nrow(rfi_clean), " articles récupérés (Scraping HTML).\n")
  } else {
    cat("⚠ RFI : Connexion réussie mais aucun article trouvé avec ce filtre.\n")
  }
  
}, error = function(e) { 
  cat("❌ Échec RFI : ", e$message, "\n") 
})

# ==============================================================================
# 8. TIME MAGAZINE WORLD (Anglais - Scraping Direct Statique Validé)
# ==============================================================================
tryCatch({
  # S'assurer que la variable de date existe
  if (!exists("date_aujourdhui")) {
    date_aujourdhui <- as.character(Sys.Date())
  }
  
  # Page publique de couverture de l'actualité mondiale de TIME
  time_url <- "https://time.com"
  
  # 1. Lecture directe du code HTML de la page de section
  time_page <- read_html(time_url)
  
  # 2. Extraction de tous les nœuds de liens de la page
  time_nodes <- html_nodes(time_page, "a")
  titres_time <- html_text(time_nodes, trim = TRUE)
  urls_time   <- html_attr(time_nodes, "href")
  
  time_df <- data.frame(
    Source = "Time Magazine World (EN)",
    Language = "EN",
    Date = date_aujourdhui,
    Title = titres_time,
    URL = urls_time,
    stringsAsFactors = FALSE
  )
  
  # 3. Nettoyage initial : suppression des lignes vides et des titres courts
  time_clean <- time_df[!is.na(time_df$URL), ]
  time_clean <- time_clean[time_clean$Title != "" & nchar(time_clean$Title) > 15, ]
  
  # 4. FILTRE : Les articles de TIME contiennent tous un numéro d'indexation unique 
  # ou l'année dans leur URL (ex: /6123456/ ou /2026/).
  time_clean <- time_clean[grep("/[0-9]{4}/|/[0-9]{6,}/", time_clean$URL), ]
  time_clean <- unique(time_clean)
  
  if (nrow(time_clean) > 0) {
    # Reconstruire les liens absolus pour les adresses relatives
    idx_relative <- !grepl("^http", time_clean$URL)
    time_clean$URL[idx_relative] <- paste0("https://time.com", time_clean$URL[idx_relative])
    time_clean <- unique(time_clean)
    
    # Intégration définitive au tableau global de données
    tous_les_articles <- rbind(tous_les_articles, time_clean)
    cat("✔ Time Magazine World (EN) :", nrow(time_clean), "articles récupérés (Scraping HTML).\n")
  } else {
    cat("⚠ Time Magazine World : Connexion réussie mais aucun article isolé.\n")
  }
  
}, error = function(e) { 
  cat("❌ Échec critique Time Magazine World (Scraping) :", e$message, "\n") 
})

# ==============================================================================
# 9. BBC WORLD NEWS (Anglais - Flux RSS)
# ==============================================================================
bbc_rss_url <- "https://bbci.co.uk"
tryCatch({
  res_bbc <- GET(bbc_rss_url, ua)
  bbc_xml <- read_xml(content(res_bbc, as = "text", encoding = "UTF-8"))
  bbc_items <- xml_find_all(bbc_xml, "//item")
  bbc_clean <- data.frame(
    Source = "BBC World News (EN)",
    Language = "EN",
    Date = extraire_date_rss(bbc_items),
    Title = xml_text(xml_find_first(bbc_items, ".//title")),
    URL = xml_text(xml_find_first(bbc_items, ".//link")),
    stringsAsFactors = FALSE
  )
  tous_les_articles <- rbind(tous_les_articles, bbc_clean)
  cat("✔ BBC World News : ", nrow(bbc_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec BBC World News : ", e$message, "\n") })

# ==============================================================================
# 10. GOOGLE NEWS WORLD (Anglais - Flux RSS)
# ==============================================================================
google_world_url <- "https://google.com"
tryCatch({
  res_world <- GET(google_world_url, ua)
  gworld_xml <- read_xml(content(res_world, as = "text", encoding = "UTF-8"))
  gworld_items <- xml_find_all(gworld_xml, "//item")
  gworld_clean <- data.frame(
    Source = "Google News World (EN)",
    Language = "EN",
    Date = extraire_date_rss(gworld_items),
    Title = xml_text(xml_find_first(gworld_items, ".//title")),
    URL = xml_text(xml_find_first(gworld_items, ".//link")),
    stringsAsFactors = FALSE
  )
  tous_les_articles <- rbind(tous_les_articles, gworld_clean)
  cat("✔ Google News World : ", nrow(gworld_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec Google News World : ", e$message, "\n") })


# ==============================================================================
# 11. ICILOME (FR)
# ==============================================================================
icilome_web_url <- "https://icilome.com/"
tryCatch({
  icilome_page <- read_html(icilome_web_url)
  icilome_anchors <- html_nodes(icilome_page, "h2 a")
  
  icilome_titles <- html_text(icilome_anchors, trim = TRUE)
  icilome_links  <- html_attr(icilome_anchors, "href")
  
  valides <- !is.na(icilome_titles) & icilome_titles != "" & !is.na(icilome_links)
  icilome_titles <- icilome_titles[valides]
  icilome_links  <- icilome_links[valides]
  icilome_links  <- ifelse(startsWith(icilome_links, "http"), icilome_links, paste0("https://icilome.com", icilome_links))
  
  icilome_clean <- unique(data.frame(
    Source = "Icilome (FR)", Language = "FR", Date = date_aujourdhui,
    Title = icilome_titles, URL = icilome_links, stringsAsFactors = FALSE
  ))
  tous_les_articles <- rbind(tous_les_articles, icilome_clean)
  cat("✔ Icilome : ", nrow(icilome_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec Icilome : ", e$message, "\n") })

# ==============================================================================
# 12. RÉPUBLIQUE TOGOLAISE (FR)
# ==============================================================================
republicoftogo_web_url <- "https://www.republicoftogo.com/"
tryCatch({
  republicoftogo_page <- read_html(republicoftogo_web_url)
  republicoftogo_anchors <- html_nodes(republicoftogo_page, "a:has(h2)")
  
  republicoftogo_titles <- vapply(republicoftogo_anchors, function(a) {
    h <- html_node(a, "h2")
    if (is.na(h)) NA_character_ else html_text(h, trim = TRUE)
  }, FUN.VALUE = character(1))
  republicoftogo_links <- html_attr(republicoftogo_anchors, "href")
  
  valides <- !is.na(republicoftogo_titles) & republicoftogo_titles != "" & !is.na(republicoftogo_links)
  republicoftogo_titles <- republicoftogo_titles[valides]
  republicoftogo_links  <- republicoftogo_links[valides]
  republicoftogo_links  <- ifelse(startsWith(republicoftogo_links, "http"),
                                  republicoftogo_links, paste0("https://www.republicoftogo.com", republicoftogo_links))
  
  republicoftogo_clean <- unique(data.frame(
    Source = "République Togolaise (FR)", Language = "FR", Date = date_aujourdhui,
    Title = republicoftogo_titles, URL = republicoftogo_links, stringsAsFactors = FALSE
  ))
  tous_les_articles <- rbind(tous_les_articles, republicoftogo_clean)
  cat("✔ République Togolaise : ", nrow(republicoftogo_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec République Togolaise : ", e$message, "\n") })

# ==============================================================================
# 13. TOGO FIRST (FR)
# ==============================================================================
togofirst_web_url <- "https://www.togofirst.com/fr"
tryCatch({
  togofirst_page <- read_html(togofirst_web_url)
  togofirst_cartes <- html_nodes(togofirst_page, ".aidanews2_k2_art")
  
  togofirst_titles <- vapply(togofirst_cartes, function(c) {
    t <- html_node(c, "[class*='title'], [class*='head']")
    if (is.na(t)) NA_character_ else html_text(t, trim = TRUE)
  }, FUN.VALUE = character(1))
  
  togofirst_links <- vapply(togofirst_cartes, function(c) {
    a <- html_node(c, "a")
    if (is.na(a)) NA_character_ else html_attr(a, "href")
  }, FUN.VALUE = character(1))
  
  valides <- !is.na(togofirst_titles) & togofirst_titles != "" & !is.na(togofirst_links)
  togofirst_titles <- togofirst_titles[valides]
  togofirst_links  <- togofirst_links[valides]
  togofirst_links  <- ifelse(startsWith(togofirst_links, "http"),
                             togofirst_links, paste0("https://www.togofirst.com", togofirst_links))
  
  togofirst_clean <- unique(data.frame(
    Source = "Togo First (FR)", Language = "FR", Date = date_aujourdhui,
    Title = togofirst_titles, URL = togofirst_links, stringsAsFactors = FALSE
  ))
  tous_les_articles <- rbind(tous_les_articles, togofirst_clean)
  cat("✔ Togo First : ", nrow(togofirst_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec Togo First : ", e$message, "\n") })


# ==============================================================================
# 14. L'ALTERNATIVE (FR)
# ==============================================================================
lalternative_web_url <- "https://lalternative.info/"
tryCatch({
  lalternative_page <- read_html(lalternative_web_url)
  lalternative_anchors <- html_nodes(lalternative_page, "h3 a")
  
  lalternative_titles <- html_text(lalternative_anchors, trim = TRUE)
  lalternative_links  <- html_attr(lalternative_anchors, "href")
  
  valides <- !is.na(lalternative_titles) & lalternative_titles != "" & !is.na(lalternative_links)
  lalternative_titles <- lalternative_titles[valides]
  lalternative_links  <- lalternative_links[valides]
  lalternative_links  <- ifelse(startsWith(lalternative_links, "http"),
                                lalternative_links, paste0("https://lalternative.info", lalternative_links))
  
  lalternative_clean <- unique(data.frame(
    Source = "L'Alternative (FR)", Language = "FR", Date = date_aujourdhui,
    Title = lalternative_titles, URL = lalternative_links, stringsAsFactors = FALSE
  ))
  tous_les_articles <- rbind(tous_les_articles, lalternative_clean)
  cat("✔ L'Alternative : ", nrow(lalternative_clean), " articles récupérés.\n")
}, error = function(e) { cat("❌ Échec L'Alternative : ", e$message, "\n") })

# ==============================================================================
#  d'autre ajouts
# ==============================================================================


# # Initialisation du tableau final complet
# tous_les_articles <- data.frame(
#   Source = character(),
#   Language = character(),
#   Date = character(),
#   Title = character(),
#   URL = character(),
#   stringsAsFactors = FALSE
# )

# Configuration du User-Agent global et date du jour
ua <- user_agent("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36")
date_aujourdhui <- as.character(Sys.Date())

# Fonction générique de scraping HTML direct pour uniformiser le code
scraper_media_html <- function(url, source_name, lang, pattern_url, base_url = "") {
  tryCatch({
    res <- GET(url, ua, config(ssl_verifypeer = FALSE, followlocation = TRUE))
    if (status_code(res) != 200) return(NULL)
    
    page <- read_html(content(res, as = "text", encoding = "UTF-8"))
    nodes <- html_nodes(page, "a")
    
    df <- data.frame(
      Source = source_name,
      Language = lang,
      Date = date_aujourdhui,
      Title = html_text(nodes, trim = TRUE),
      URL = html_attr(nodes, "href"),
      stringsAsFactors = FALSE
    )
    
    # Nettoyage et application du filtre d'expression régulière
    df_clean <- df[!is.na(df$URL) & df$Title != "", ]
    if (!is.null(pattern_url) && pattern_url != "") {
      df_clean <- df_clean[grep(pattern_url, df_clean$URL), ]
    }
    
    # Gestion des liens relatifs
    if (nrow(df_clean) > 0 && base_url != "") {
      idx_relative <- !grepl("^http", df_clean$URL)
      df_clean$URL[idx_relative] <- paste0(base_url, df_clean$URL[idx_relative])
    }
    
    return(unique(df_clean))
  }, error = function(e) {
    cat("❌ Échec sur la source :", source_name, "(", e$message, ")\n")
    return(NULL)
  })
}

# ==============================================================================
# AJOUT DES 6 SOURCES AFRIQUE DE L'OUEST (6 à 11)
# ==============================================================================

# 6. CÔTE D'IVOIRE : Fraternité Matin (Français - Quotidien d'État)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://fratmat.info", "Fraternité Matin (CI)", "FR", "/article/|/index.php/"))

# 7. BURKINA FASO : Le Faso (Français - Référence en ligne)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://lefaso.net", "Le Faso (BF)", "FR", "article", "https://lefaso.net"))

# 8. BENIN : La Nation (Français - Quotidien national)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://lanation.bj", "La Nation (BJ)", "FR", "/actualite/|/politique/|/economie/"))

# 9. GHANA : Graphic Online (Anglais - Plus grand journal du Ghana)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://graphic.com.gh", "Graphic Online (GH)", "EN", "/news/|/business/"))

# 10. MALI : Malijet (Français - Portail d'actualité majeur)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://malijet.com", "Malijet (ML)", "FR", "a_news|actualite", "https://malijet.com"))

# 11. NIGER : Le Sahel (Français - Organe de presse national)
tous_les_articles <- rbind(tous_les_articles, scraper_media_html("https://lesahel.org", "Le Sahel (NE)", "FR", "/[0-9]{4}/[0-9]{2}/"))


# ==============================================================================
# 9. TRI ET AFFICHAGE FINAL
# ==============================================================================
tous_les_articles <- unique(tous_les_articles)

# Optionnel : Trier le tableau par date décroissante (les plus récents d'abord)
tous_les_articles <- tous_les_articles[order(tous_les_articles$Date, decreasing = TRUE), ]

cat("\n--- Structure finale du listing (", nrow(tous_les_articles), " lignes) ---\n\n")
print(head(tous_les_articles, 20))


# 1. Vérifier si les sources existent n'importe où dans le tableau
table(tous_les_articles$Source)




# ==============================================================================
# TRAITEMENT FINAL ET EXPORTATION
# ==============================================================================
tous_les_articles <- tous_les_articles[!is.na(tous_les_articles$Source), ]
tous_les_articles <- unique(tous_les_articles)
tous_les_articles <- tous_les_articles[order(tous_les_articles$Date, decreasing = TRUE), ]

cat("\n--- Distribution finale des sources cumulées : ---\n")
print(table(tous_les_articles$Source))

# Sauvegarde finale
tryCatch({
  nom_fichier <- paste0("actualites_global_", date_aujourdhui, ".csv")
  write.csv2(tous_les_articles, file = nom_fichier, row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n💾 Fichier enregistré sous :", nom_fichier, "\n📍 Dossier :", getwd(), "\n")
}, error = function(e) { cat("\n❌ Erreur d'enregistrement :", e$message, "\n") })
