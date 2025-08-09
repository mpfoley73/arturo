# https://openaccess-api.clevelandart.org/
# Appendix B: Departments
department_choices <- c(
  "African Art", "American Painting and Sculpture", "Art of the Americas",
  "Chinese Art", "Contemporary Art", "Decorative Art and Design", 
  "Drawings", "Egyptian and Ancient Near Eastern Art", "European Painting and Sculpture",
  "Greek and Roman Art", "Indian and South East Asian Art", "Islamic Art",
  "Japanese Art", "Korean Art", "Medieval Art",
  "Modern European Painting and Sculpture", "Oceania", "Performing Arts, Music, & Film",
  "Photography", "Prints", "Textiles"
)

# https://openaccess-api.clevelandart.org/
# Appendix C: Types
type_choices <- c(
  "Amulets", "Apparatus", "Arms and Armor",
  "Basketry", "Book Binding", "Bound Volume",
  "Calligraphy", "Carpet", "Ceramic",
  "Coins", "Cosmetic Objects", "Drawing",
  "Embroidery", "Enamel", "Forgery",
  "Frame", "Funerary Equipment", "Furniture and woodwork",
  "Garment", "Glass", "Glyptic",
  "Illumination", "Implements", "Inlays",
  "Ivory", "Jade", "Jewelry",
  "Knitting", "Lace", "Lacquer",
  "Leather", "Linoleum Block", "Lithographic Stone",
  "Manuscript", "Metalwork, Miniature",
  "Miscellaneous", "Mixed Media", "Monotype",
  "Mosaic", "Musical Instrument", "Netsuke",
  "Painting", "Papyri", "Photograph",
  "Plaque", "Plate", "Portfolio",
  "Portrait Miniature", "Print", "Relief",
  "Rock crystal", "Rubbing", "Sampler",
  "Scarabs", "Sculpture", "Seals",
  "Silver", "Spindle Whorl", "Stencil",
  "Stone", "Tapestry", "Textile",
  "Time-based Media", "Tool", "Velvet",
  "Vessels", "Wood", "Woodblock"
)

# Exhibit data from prep_data.R
load("app_data.Rdata")
