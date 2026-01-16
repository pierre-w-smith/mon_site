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

# 1) Mettre à jour la date (optionnel)
add_update_date <- function(yml = "_quarto.yml") {
  y <- readLines(yml, warn = FALSE)
  today <- format(Sys.Date(), "%d/%m/%Y")
  right_line <- paste0('    right: "Dernière mise à jour : ', today, '"')
  
  has_website    <- any(grepl("^\\s*website\\s*:", y))
  has_pagefooter <- any(grepl("^\\s*page-footer\\s*:", y))
  
  if (!has_website) {
    y <- c(y, "", "website:", "  page-footer:", right_line)
  } else if (!has_pagefooter) {
    i <- which(grepl("^\\s*website\\s*:", y))[1]
    y <- append(y, c("  page-footer:", right_line), after = i)
  } else {
    pf_idx <- which(grepl("^\\s*page-footer\\s*:", y))[1]
    right_rel <- which(grepl("^\\s*right\\s*:", y[(pf_idx+1):length(y)]))
    if (length(right_rel) > 0) {
      y[pf_idx + right_rel[1]] <- right_line
    } else {
      y <- append(y, right_line, after = pf_idx)
    }
  }
  
  writeLines(y, yml)
}

add_update_date()

# 2) Rendre le site
if (!requireNamespace("quarto", quietly = TRUE)) install.packages("quarto")
quarto::quarto_render(as_job = FALSE)

if (!file.exists("docs/index.html")) stop("❌ Rendu échoué : docs/index.html introuvable")
message("✅ Rendu OK → docs/index.html trouvé.")

# 3) Commit & push (robuste)
message("📦 Ajout des fichiers générés...")

# Toujours se mettre à la racine git (évite les soucis de working dir)
root <- system("git rev-parse --show-toplevel", intern = TRUE)
setwd(root)

# Forcer l'ajout de docs/ (utile si .gitignore gêne)
system("git add -A _quarto.yml file")
system("git add -f docs")

# Montrer ce qui est prêt à être commité
message("🧾 Fichiers stagés :")
print(system("git diff --cached --name-only", intern = TRUE))

st <- system("git status --porcelain", intern = TRUE)

if (length(st) > 0) {
  system(paste0('git commit -m "Mise a jour du site Quarto (', format(Sys.Date(), "%d/%m/%Y"), ')"'))
  system("git push")
  message("🌍 Poussé sur GitHub.")
} else {
  message("ℹ️ Aucun changement à commit, donc rien à pousser.")
}

message("🔗 Vérifie sur : https://pierre-w-smith.github.io/mon_site/")
