# =====================================================
# Objet  : Générer, dater, commit et pousser ton site Quarto
# Auteur : Pierre Smith
# =====================================================

message("🚀 Construction du site Quarto...")

# 0) S'assurer que _quarto.yml rend vers docs/
fix_output_dir <- function(yml = "_quarto.yml") {
  if (!file.exists(yml)) stop("❌ Fichier _quarto.yml introuvable")
  y <- readLines(yml, warn = FALSE)
  has_project <- any(grepl("^\\s*project\\s*:", y))
  has_output  <- any(grepl("^\\s*output-dir\\s*:", y))
  if (!has_project) {
    y <- c("project:", "  type: website", "  output-dir: docs", y)
  } else if (!has_output) {
    i <- which(grepl("^\\s*project\\s*:", y))[1]
    y <- append(y, "  output-dir: docs", after = i)
  }
  writeLines(y, yml)
}

fix_output_dir()

# 1) Ajouter/mettre à jour la date dans le pied de page (page-footer)
add_update_date <- function(yml = "_quarto.yml") {
  y <- readLines(yml, warn = FALSE)
  today <- format(Sys.Date(), "%d/%m/%Y")
  right_line <- paste0('    right: "Dernière mise à jour : ', today, '"')
  
  has_website    <- any(grepl("^\\s*website\\s*:", y))
  has_pagefooter <- any(grepl("^\\s*page-footer\\s*:", y))
  has_footer     <- any(grepl("^\\s*footer\\s*:", y)) # au cas où, on va le migrer
  
  if (!has_website) {
    # Crée un bloc website + page-footer minimal si absent
    y <- c(
      y,
      "",
      "website:",
      "  page-footer:",
      right_line
    )
  } else {
    # On a website: ; s'il existe un ancien 'footer:', on le remplace par 'page-footer:'
    if (has_footer && !has_pagefooter) {
      y <- sub("^\\s*footer\\s*:", "  page-footer:", y)
      has_pagefooter <- TRUE
    }
    if (!has_pagefooter) {
      # insère 'page-footer:' juste avant 'format:' (ou en fin si pas trouvé)
      i <- which(grepl("^\\s*format\\s*:", y))[1]
      if (length(i) == 0 || is.na(i)) i <- length(y)
      insert_block <- c("  page-footer:", right_line)
      y <- append(y, insert_block, after = i - 1)
    } else {
      # Met à jour la ligne 'right:' dans le bloc page-footer
      # (remplace toute ligne 'right:' existante sous page-footer)
      # Si aucune 'right:' n'existe, on l'ajoute juste après page-footer:
      pf_idx <- which(grepl("^\\s*page-footer\\s*:", y))[1]
      # Cherche une ligne right: après pf_idx
      right_idx <- pf_idx + which(grepl("^\\s*right\\s*:", y[(pf_idx+1):length(y)]))[1]
      if (is.finite(right_idx)) {
        y[right_idx] <- right_line
      } else {
        y <- append(y, right_line, after = pf_idx)
      }
    }
  }
  
  writeLines(y, yml)
}

add_update_date()

# 2) Nettoyer les anciens dossiers
unlink("_site", recursive = TRUE, force = TRUE)
unlink("docs",  recursive = TRUE, force = TRUE)

# 3) Rendre le site (au premier plan)
if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")
quarto::quarto_render(as_job = FALSE)

# 4) Vérifier la génération
if (!file.exists("docs/index.html")) {
  stop("❌ Le rendu a échoué : pas de docs/index.html (vérifie _quarto.yml et les logs Quarto)")
}

message("✅ Rendu OK → docs/index.html trouvé.")

# 5) Commit & push
system("git add .")
system(paste0('git commit -m "Mise à jour du site Quarto (', format(Sys.Date(), "%d/%m/%Y"), ')"'))
system("git push")

message("🌍 Poussé sur GitHub.")
message("🔗 Vérifie sur : https://pierre-w-smith.github.io/mon_site/")
