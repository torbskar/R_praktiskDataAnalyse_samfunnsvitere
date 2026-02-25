library(tidyverse)
library(sf)

cat("=== Test 1: Lese inn shapefil ===\n")
oslo <- st_read("data/shapefiles/oslo.shp", quiet = TRUE)
cat("Rader:", nrow(oslo), "\n")
cat("Kolonner:", ncol(oslo), "\n")
cat("CRS:", st_crs(oslo)$epsg, "\n")

cat("\n=== Test 2: glimpse oslo ===\n")
glimpse(oslo)

cat("\n=== Test 3: Grunnkart ===\n")
p1 <- ggplot(oslo) + geom_sf()
cat("Enkleste kart: OK\n")

p2 <- ggplot(oslo) + geom_sf(fill = "gray95") + theme_void()
cat("Med farger og theme_void: OK\n")

kart <- ggplot(oslo) + geom_sf(fill = "gray95") + coord_sf(datum = st_crs(oslo)) + theme_void()
cat("Grunnkart lagret: OK\n")

cat("\n=== Test 4: Lese inn krimdata ===\n")
krim <- readRDS("data/shapefiles/syntetisk_krim.rds")
cat("Rader:", nrow(krim), "\n")
cat("Kolonner:", ncol(krim), "\n")
cat("Kolonnenavn:", paste(names(krim), collapse = ", "), "\n")
cat("head:\n")
print(head(krim))
cat("table:\n")
print(table(krim$krimtypekodenavn))

cat("\n=== Test 5: Punktkart ===\n")
p3 <- kart + geom_point(data = krim, aes(x = x, y = y), color = "red", alpha = 0.01, size = 0.01)
cat("Punktkart: OK\n")

cat("\n=== Test 6: Aggregering ===\n")
krimplot <- krim %>%
  group_by(x, y) %>%
  summarise(antall = n(), .groups = "drop") %>%
  mutate(antall = ifelse(antall >= 200, 200, antall))
cat("Aggregerte rader:", nrow(krimplot), "\n")

cat("\n=== Test 7: Heatmap ===\n")
p4 <- ggplot(krimplot, aes(x = x, y = y, fill = antall)) +
  geom_tile() +
  coord_sf(datum = st_crs(oslo)) +
  theme_void() +
  scale_fill_distiller(palette = "Spectral")
cat("Enkel heatmap: OK\n")

cat("\n=== Test 8: st_union ===\n")
oslo_omkr <- st_union(oslo)
cat("st_union: OK\n")

p5 <- kart +
  geom_tile(data = krimplot, aes(x = x, y = y, fill = antall), alpha = 0.75) +
  scale_fill_distiller(palette = "Spectral") +
  geom_sf(data = oslo_omkr, color = "black", fill = NA) +
  labs(fill = "Hendelser")
cat("Heatmap med grense: OK\n")

cat("\n=== Test 9: Facet etter type ===\n")
krimplot_type <- krim %>%
  group_by(x, y, krimtypekodenavn) %>%
  summarise(antall = n(), .groups = "drop") %>%
  mutate(antall = ifelse(antall >= 150, 150, antall))
cat("Aggregerte rader med type:", nrow(krimplot_type), "\n")

p6 <- kart +
  geom_tile(data = krimplot_type, aes(x = x, y = y, fill = antall), alpha = 0.75) +
  scale_fill_distiller(palette = "Spectral") +
  geom_sf(data = oslo_omkr, color = "black", fill = NA) +
  facet_wrap(~krimtypekodenavn) +
  labs(fill = "Hendelser")
cat("Facet kart: OK\n")

cat("\n=== Test 10: Lagre en figur til fil for å verifisere ===\n")
ggsave("test_kart_output.png", p5, width = 8, height = 8)
cat("Figur lagret: OK\n")

cat("\n=== ALLE TESTER BESTATT ===\n")
