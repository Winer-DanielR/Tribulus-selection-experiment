# Ternary plot ####
# Here I am doing the ternary plots, I am changing the CATEGORIES plots and divide them
# into two. Based on the missing axis.
# For the Island plots I am going to use the same colors as the previous questions.
# Floreana bola naranja
# Isabela cuadrado amarillo
# Santa Cruz triangulo azul
# NO hay verde.
# 

# Check if this works with the ternary plots
plot_theme <-     theme(axis.line = element_line(linetype = "solid", size = 1), 
                        axis.title = element_text(size = 14, 
                                                  face = "bold"
                        ),
                        axis.text = element_text(size = 12, face = "bold"), 
                        axis.text.x = element_text(size = 12), 
                        plot.title = element_text(size = 16, face = "bold", hjust = 0),
                        text = element_text(family = "Noto Sans"),
                        legend.text = element_text(size = 11), 
                        legend.title = element_text(size = 12, face = "bold"),
                        legend.position = "right",
                        #panel.background = element_rect(fill = NA),
                        legend.background = element_rect(fill = NA, size = 0),
                        strip.text = element_text(size = 10, face = "bold"),
                        strip.background = element_blank(),
                        panel.spacing = unit(1, "cm")
)

## Floreana ####
Floreana_tern <- ggtern(data = Floreana_ternary_pivot,
                        aes(Uneaten_freq,
                            Eaten_freq,
                            Missing_freq, fill = Categories,
                            shape = Categories,
                            color = Categories
                        )) +
  geom_line(aes(group = Categories, color = Categories)) +
  geom_point(size = 5) + labs(title = "Floreana") +
  geom_text(aes(label = time), size = 2.5, colour = "black", fontface = "bold") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large All Spines",
                               "Large No Spines",
                               "Small All Spines",
                               "Small No Spines")) +
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + plot_theme


# Floreana_tern + limit_tern(0.5,1,0.5) # Zoom on times 0, 1
# Floreana_tern + limit_tern(0.5,0.5,1) # Zoom on times 2,3,4

Floreana_tern + theme_zoom_L(0.6) # Time 0, 1
Floreana_tern + theme_zoom_R(0.4) # Time 2, 3

# Floreana plots per categories works best.

## Isabela ####
### All  and no spines spines ####
Isabela_tern <- ggtern(data = Isabela_all_spines_pivot,
                        aes(Uneaten_freq,
                            Eaten_freq,
                            Missing_freq, fill = Categories,
                            shape = Categories,
                            color = Categories
                        )) +
  geom_line(aes(group = Categories, color = Categories)) +
  geom_point(size = 5) + labs(title = "Isabela") +
  geom_text(aes(label = time), size = 2.5, colour = "black", fontface = "bold") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large All Spines",
                               "Large No Spines",
                               "Small All Spines",
                               "Small No Spines")) +
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + plot_theme

Isabela_tern + theme_zoom_L(0.5) # Time 0, 1
Isabela_tern + theme_zoom_R(0.5) # Time 2, 3

### Lower and Upper spines #####
Isabela_tern_2 <- ggtern(data = Isabela_lower_spines_pivot,
                       aes(Uneaten_freq,
                           Eaten_freq,
                           Missing_freq, fill = Categories,
                           shape = Categories,
                           color = Categories
                       )) +
  geom_line(aes(group = Categories, color = Categories)) +
  geom_point(size = 5) + labs(title = "Isabela 2") +
  geom_text(aes(label = time), size = 2.5, colour = "black", fontface = "bold") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large Lower Spines",
                               "Large Upper Spines",
                               "Small Lower Spines",
                               "Small Upper Spines")) +
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"),
                     name = "Categories",
                     labels = c("Large Lower Spines",
                                "Large Upper Spines",
                                "Small Lower Spines",
                                "Small Upper Spines")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Categories",
                     labels = c("Large Lower Spines",
                                "Large Upper Spines",
                                "Small Lower Spines",
                                "Small Upper Spines")) + plot_theme

Isabela_tern_2 + theme_zoom_L(0.45) # Time 0, 1
Isabela_tern_2 + theme_zoom_R(0.55) # Time 2, 3


# Isabela_tern <- ggtern(data = Isabela_ternary_pivot,
#                        aes(Uneaten_freq,
#                            Eaten_freq,
#                            Missing_freq, fill = Categories,
#                            shape = Categories,
#                            color = Categories
#                        )) +
#   geom_line(aes(group = Categories, color = Categories)) +
#   geom_point(size = 2) + labs(title = "Isabela") +
#   xlab("Uneaten") +
#   ylab("Eaten") +
#   zlab("Missing") + theme_showarrows() +
#   scale_fill_manual(values = c("#D55E00",
#                                         "#004D40",
#                                         "#E69F00",
#                                         "#666666",
#                                         "#009E73",
#                                         "#56B4E9",
#                                         "#0072B2",
#                                         "#CC79A7"
#                                         
#   ), name = "Categories",
#   labels = c("Large All Spines",
#              "Large Lower Spines",
#              "Large No Spines",
#              "Large Upper Spines",
#              "Small All Spines",
#              "Small Lower Spines",
#              "Small No Spines",
#              "Small Upper Spines")) +
#   scale_color_manual(values = c("#D55E00",
#                                          "#004D40",
#                                          "#E69F00",
#                                          "#666666",
#                                          "#009E73",
#                                          "#56B4E9",
#                                          "#0072B2",
#                                          "#CC79A7"
#   ),
#   name = "Categories",
#   labels = c("Large All Spines",
#              "Large Lower Spines",
#              "Large No Spines",
#              "Large Upper Spines",
#              "Small All Spines",
#              "Small Lower Spines",
#              "Small No Spines",
#              "Small Upper Spines")) + # Set colors
#   scale_shape_manual(values = c(3, 4, 8, 21:25),
#                      name = "Categories",
#                      labels = c("Large All Spines",
#                                 "Large Lower Spines",
#                                 "Large No Spines",
#                                 "Large Upper Spines",
#                                 "Small All Spines",
#                                 "Small Lower Spines",
#                                 "Small No Spines",
#                                 "Small Upper Spines"))
# 
# 
# Isabela_tern + theme_zoom_R(1)


## Santa Cruz ####
### All  and no spines spines ####
Cruz_tern <- ggtern(data = Cruz_all_spines_pivot,
                       aes(Uneaten_freq,
                           Eaten_freq,
                           Missing_freq, fill = Categories,
                           shape = Categories,
                           color = Categories
                       )) +
  geom_line(aes(group = Categories, color = Categories)) +
  geom_point(size = 5) + labs(title = "Santa Cruz") +
  geom_text(aes(label = time), size = 2.5, colour = "black", fontface = "bold") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large All Spines",
                               "Large No Spines",
                               "Small All Spines",
                               "Small No Spines")) +
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Categories",
                     labels = c("Large All Spines",
                                "Large No Spines",
                                "Small All Spines",
                                "Small No Spines")) + plot_theme

### Lower and Upper spines #####
Cruz_tern_2 <- ggtern(data = Cruz_lower_spines_pivot,
                         aes(Uneaten_freq,
                             Eaten_freq,
                             Missing_freq, fill = Categories,
                             shape = Categories,
                             color = Categories
                         )) +
  geom_line(aes(group = Categories, color = Categories)) +
  geom_point(size = 5) + labs(title = "Santa Cruz 2") +
  geom_text(aes(label = time), size = 2.5, colour = "black", fontface = "bold") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"), name = "Categories",
                    labels = c("Large Lower Spines",
                               "Large Upper Spines",
                               "Small Lower Spines",
                               "Small Upper Spines")) +
  scale_color_manual(values = c("#f5793a", "#a95aa1", "#85c0f9", "#009e73", "#0f2080"),
                     name = "Categories",
                     labels = c("Large Lower Spines",
                                "Large Upper Spines",
                                "Small Lower Spines",
                                "Small Upper Spines")) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Categories",
                     labels = c("Large Lower Spines",
                                "Large Upper Spines",
                                "Small Lower Spines",
                                "Small Upper Spines")) + plot_theme

Cruz_tern_2 + theme_zoom_L(0.6) # Time 0, 1
Cruz_tern_2 + theme_zoom_R(0.4) # Time 2, 3





# Category Ternary Plot ####
# This plot uses the Island ternary dataset!
# The categories are:
# Large all spines
# Large Lower spines
# Large no spines
# Large upper spines
# Small All spines
# Small lower spines
# Small no spines
# Small upper spines
# 
# The idea here is to create a plot per category and use all islands on that plot
## Large all spines ####
Island_ternary_large_all <- filter(Island_ternary, !(Categories %in% c("Large_Lower_spines",
                                                                       #"Large_All_spines",
                                                                       "Large_Upper_spines",
                                                                       "Large_No_spines",
                                                                       "Small_All_spines",
                                                                       "Small_Upper_spines",
                                                                       "Small_Lower_spines",
                                                                       "Small_No_spines")))


Large_all_tern <- ggtern(data = Island_ternary_large_all,
                         aes(Uneaten_freq,
                             Eaten_freq,
                             Missing_freq, fill = Island,
                             shape = Island,
                             color = Island
                         )) +
  geom_line(aes(group = Island, color = Island), linewidth = 1.2) +
  geom_point(size = 3) + labs(title = "Large All Spines Mericarps") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#D55E00",
                                        "#E69F00",
                                        "#009E73",
                                        "#0072B2",
                                        "#56B4E9",
                                        "#CC79A7",
                                        "#666666"), name = "Islands",
                                        labels = c("Floreana",
                                                   "Isabela",
                                                   "Santa Cruz"
                                        )) +
  scale_color_manual(values = c("#D55E00",
                                         "#E69F00",
                                         "#009E73",
                                         "#0072B2",
                                         "#56B4E9",
                                         "#CC79A7",
                                         "#666666"),
                                         name = "Islands",
                     labels = c("Floreana",
                                "Isabela",
                                "Santa Cruz"
                     )) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Islands",
                     labels = c("Floreana",
                                "Isabela",
                                "Santa Cruz"))


Large_all_tern + theme_zoom_L(0.5)

## Large NO spines ####
Island_ternary_large_no <- filter(Island_ternary, !(Categories %in% c("Large_Lower_spines",
                                                                      "Large_All_spines",
                                                                      "Large_Upper_spines",
                                                                      #"Large_No_spines",
                                                                      "Small_All_spines",
                                                                      "Small_Upper_spines",
                                                                      "Small_Lower_spines",
                                                                      "Small_No_spines")))


Large_no_tern <- ggtern(data = Island_ternary_large_no,
                        aes(Uneaten_freq,
                            Eaten_freq,
                            Missing_freq, fill = Island,
                            shape = Island,
                            color = Island
                        )) +
  geom_line(aes(group = Island, color = Island), linewidth = 1.2) +
  geom_point(size = 3) + labs(title = "Large No Spines Mericarps") +
  xlab("Uneaten") +
  ylab("Eaten") +
  zlab("Missing") + theme_showarrows() +
  scale_fill_manual(values = c("#D55E00",
                                        "#E69F00",
                                        "#009E73",
                                        "#0072B2",
                                        "#56B4E9",
                                        "#CC79A7",
                                        "#666666"), name = "Islands",
                                        labels = c("Floreana",
                                                   "Isabela",
                                                   "Santa Cruz"
                                        )) +
  scale_color_manual(values = c("#D55E00",
                                         "#E69F00",
                                         "#009E73",
                                         "#0072B2",
                                         "#56B4E9",
                                         "#CC79A7",
                                         "#666666"),
                                         name = "Islands",
                     labels = c("Floreana",
                                "Isabela",
                                "Santa Cruz"
                     )) + # Set colors
  scale_shape_manual(values = c(21:24),
                     name = "Islands",
                     labels = c("Floreana",
                                "Isabela",
                                "Santa Cruz"))

Large_no_tern + theme_zoom_L(0.5)
Large_no_tern + theme_zoom_R(0.5)
