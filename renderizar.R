# Renderizar todo el sitio
rmarkdown::render_site(".")

# Abrir el resultado en el Viewer de Positron
output_path <- normalizePath("docs/index.html")
if (rstudioapi::isAvailable()) {
  rstudioapi::viewer(output_path)
} else {
  message("Viewer no disponible; abriendo en el navegador por defecto.")
  if (interactive()) {
    utils::browseURL(output_path)
  }
}
