pkgs = c('cowplot', 'data.table', 'extrafont', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)
# font_import()
loadfonts(device = "win", quiet = TRUE)
theme_set(theme_bw())

data.dir = paste0(getwd(), '/data/')
gfx.dir = paste0(getwd(), '/graphics/')

# Functions ---------------------------------------------------------------
title_search = function(query){
  paste0(data.dir, 'imdb/imdb-tv-title-basics.csv') %>% 
    fread() %>% 
    filter(grepl(pattern = query, x = primaryTitle, ignore.case = TRUE)) %>% 
    select(tconst, primaryTitle, startYear)
}

show_data = function(id){
  # psych.id = "tt0491738"
  episodes = paste0(data.dir, 'imdb/imdb-title-episodes.tsv') %>% 
    read_tsv(na = "\\N", quote = "") %>% 
    filter(parentTconst == id) %>% 
    na.omit() %>% 
    rename(season = seasonNumber, episode = episodeNumber) %>% 
    arrange(season, episode)
  show = list(
    episodes,
    paste0(data.dir, 'imdb/imdb-title-basics.tsv') %>% 
      read_tsv(na = "\\N", quote = "") %>% 
      filter(tconst %in% episodes$tconst),
    paste0(data.dir, "imdb/imdb-titles-ratings.tsv") %>% 
      read_tsv(na = "\\N", quote = "") %>% 
      filter(tconst %in% episodes$tconst) %>% 
      na.omit() %>% 
      unique()) %>% 
    reduce(inner_join) %>% 
    select(season, episode, 
           title = primaryTitle, year = startYear, 
           rating = averageRating, votes = numVotes)
  gc(); gc(reset = TRUE)
  return(show)
}

plot_ratings = function(data, title, font = "Arial",
                        color = ""){
  show_ratings = data %>% 
    # mutate(season = factor(season, levels = 1:max(season)),
    #        episode = factor(episode, levels = max(episode):1)) %>% 
    ggplot(aes(x = season, 
               y = episode, 
               fill = rating)) +
    geom_tile(color = 'black', size = 1) +
    geom_text(aes(label = rating), 
              color = "white",
              family = font, fontface = "bold") +
    scale_x_continuous(position = 'top',
                       breaks = unique(data$season),
                       expand = c(0, 0)) +
    scale_y_reverse(breaks = unique(data$episode),
                    expand = c(0, 0)) +
    # scale_fill_viridis_c() +
    scale_fill_gradientn(
      colours = c("firebrick4", "goldenrod2", "green4"),
      values = scales::rescale(c(4, 7.5, 10)),
      limits = c(4, 10)) +
    labs(x = 'Season', y = 'Episode', fill = 'Rating') +
    theme_minimal() +
    theme(text = element_text(family = font,
                              face = 'bold'),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'none')
  return(show_ratings)
}

# Psych ---------------------------------------------------------------
psych = show_data("tt0491738")
plot_grid(
  ggdraw() + 
    draw_image(paste0(data.dir, "images/logo-psych.png")),
  plot_ratings(psych, font = "American Typewriter"),
  ncol = 1,
  rel_heights = c(.2, 1)) +
  theme(plot.margin = margin(t = 6, r = 125, b = 6, l = 125)) +
  ggsave(paste0(gfx.dir, 'imdb-psych-ratings.png'),
         height = 7, width = 7, dpi = 600)


# Mentalist ---------------------------------------------------------------
mental = show_data("tt1196946")
plot_grid(
  ggdraw() + 
    draw_image(paste0(data.dir, "images/logo-mentalist.png")) +
    theme(
      plot.margin = margin(t = -80, r = -80, b = -80, l = -80)
    ),
  plot_ratings(mental),
  ncol = 1,
  rel_heights = c(.1, 1)) +
  theme(plot.margin = margin(t = 6, r = 120, b = 6, l = 120)) +
  ggsave(paste0(gfx.dir, 'imdb-mentalist-ratings.png'),
         height = 8, width = 7, dpi = 600)

# House -------------------------------------------------------------------
house = show_data("tt0412142")
house = rbind(
  house %>% 
    mutate(episode = ifelse(season == 6 & episode < 16,
                            episode + 1, episode),
           title = ifelse(season == 6 & episode == 2,
                          "Broken: Part 2", title)),
  house %>% filter(season == 6, episode == 1) %>% 
    mutate(title = "Broken: Part 1")) %>% 
  arrange(season, episode)

plot_grid(
  ggdraw() + 
    draw_image(paste0(data.dir, "images/logo-house.svg")) +
    theme(
      plot.margin = margin(t = -10, r = -20, b = -10, l = -20)
    ),
  plot_ratings(house),
  ncol = 1,
  rel_heights = c(.1, 1)) +
  theme(plot.margin = margin(t = 6, r = 120, b = 6, l = 120)) +
  ggsave(paste0(gfx.dir, 'imdb-house-ratings.png'),
         height = 8, width = 7, dpi = 600)


# Mr. Robot ---------------------------------------------------------------
robot = show_data("tt4158110")
plot_grid(
  ggdraw() +
    draw_image(paste0(data.dir, 'images/logo-mrrobot.svg') %>% 
                 magick::image_read() %>% 
                 magick::image_rotate(degrees = 270)) +
    theme(plot.margin = margin(t = -6, r = -48, b = -6, l = -40)),
  plot_ratings(robot, font = 'Roboto Mono') +
    theme(text = element_text(color = 'grey88'),
          axis.text = element_text(color = 'grey88')),
  ncol = 2,
  rel_widths = c(.5, 1)) +
  theme(plot.background = element_rect(fill = 'black'),
        plot.margin = margin(t = 6, r = 6, b = 6, l = 0)) +
  ggsave(paste0(gfx.dir, 'imdb/imdb-mrrobot-ratings.png'),
         height = 6, width = 3.5, dpi = 600)

# The Walking Dead --------------------------------------------------------
twd = show_data('tt1520211')
plot_ratings(twd)

# Game of Thrones ---------------------------------------------------------
got = show_data('tt0944947')
plot_ratings(got)
