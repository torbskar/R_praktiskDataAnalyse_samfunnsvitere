# install_packages.R
# Skanner alle .qmd-filer i prosjektet og installerer eventuelle manglende pakker.
# Scriptet kan sources fra index.qmd for å sikre at alle pakker alltid er tilgjengelige.
#
# Bruk fra index.qmd:
#   source("script/install_packages.R")
#
# Deretter kan hent_prosjektpakker() kalles for å vise en dynamisk pakkeoversikt.

hent_prosjektpakker <- function(proj_dir = ".") {
  # Finn alle .qmd-filer i prosjektmappen (ikke rekursivt)
  qmd_filer <- list.files(proj_dir, pattern = "\\.qmd$",
                           full.names = TRUE, recursive = FALSE)

  # Mønster for å trekke ut pakkenavn
  monster_library <- "(?<=library\\()['\"]?[a-zA-Z][a-zA-Z0-9._]*"
  monster_require <- "(?<=require\\()['\"]?[a-zA-Z][a-zA-Z0-9._]*"
  monster_ns      <- "[a-zA-Z][a-zA-Z0-9._]*(?=::)"

  alle_pakker <- character(0)

  for (fil in qmd_filer) {
    linjer <- readLines(fil, warn = FALSE)

    lib <- unlist(regmatches(linjer, gregexpr(monster_library, linjer, perl = TRUE)))
    req <- unlist(regmatches(linjer, gregexpr(monster_require, linjer, perl = TRUE)))
    ns  <- unlist(regmatches(linjer, gregexpr(monster_ns,      linjer, perl = TRUE)))

    alle_pakker <- c(alle_pakker, lib, req, ns)
  }

  # Rens opp anførselstegn og filtrer ut plassholderord og falske positiver
  alle_pakker <- gsub("['\"]", "", alle_pakker)

  ekskluder <- c(
    "pakkenavn",   # placeholder brukt i instruksjonstekst
    "following"    # XPath-akse, ikke en R-pakke
  )

  alle_pakker <- unique(alle_pakker)
  alle_pakker <- alle_pakker[!alle_pakker %in% ekskluder]
  alle_pakker <- alle_pakker[nchar(alle_pakker) > 1]

  sort(alle_pakker)
}


# ── Installer manglende pakker ──────────────────────────────────────────────

.prosjektpakker <- hent_prosjektpakker()
.installerte    <- rownames(installed.packages())
.mangler        <- .prosjektpakker[!.prosjektpakker %in% .installerte]

if (length(.mangler) > 0) {
  message(sprintf(
    "Installerer %d manglende pakke(r): %s",
    length(.mangler),
    paste(.mangler, collapse = ", ")
  ))
  install.packages(.mangler, repos = "https://cloud.r-project.org")
} else {
  message(sprintf(
    "Alle %d prosjektpakker er allerede installert.",
    length(.prosjektpakker)
  ))
}

rm(.prosjektpakker, .installerte, .mangler)
