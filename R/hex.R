make_hex <- function() {
  imgurl <- system.file("maths_genealogy_img.png", package = "maths.genealogy")
  hexSticker::sticker(imgurl,
                      s_x = 1.0,
                      s_y = 1.025,
                      s_width = 0.85,
                      s_height = 0.85,
                      package = "",
                      p_x = 0.6,
                      p_y = 1.45,
                      p_color = "#FFFFFF",
                      p_family = "serif",
                      p_fontface = "bold",
                      p_size = 5.0,
                      h_fill = "#1F2528",
                      h_color = "#EDEDED",
                      url = "genealogy.louisaslett.com",
                      u_size = 1.6,
                      u_family = "mono",
                      u_color = "#EDEDED",
                      u_x = 1.78,
                      u_y = 0.515,
                      u_angle = 90.0,
                      white_around_sticker = TRUE,
                      filename = file.path("inst", "maths_genealogy_hex.png"),
                      dpi = 600L)
  usethis::use_logo(file.path("inst", "maths_genealogy_hex.png"), geometry = "480x556")
}
