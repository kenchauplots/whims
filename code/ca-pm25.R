pkgs = c('av', 'data.table', 'extrafont', 'gganimate', 'leaflet',
         'R.utils', 'RPostgreSQL', 'sf', 'tidyverse')
for(p in pkgs) require(p, character.only = TRUE)
rm(p, pkgs)
loadfonts(quiet = TRUE)

code.dir = paste0(getwd(), '/code/')
data.dir = paste0(getwd(), '/data/')
gfx.dir = paste0(getwd(), '/graphics/')

# CA hexagons -------------------------------------------------------------
ca = st_read(paste0(data.dir, 'shapes/california.shp'),
             quiet = TRUE) %>% 
  st_cast(., 'POLYGON') %>% 
  mutate(area = st_area(.) %>% as.numeric()) %>% 
  group_by(county) %>% 
  filter(area == max(area)) %>% 
  ungroup() %>% 
  select(county, area, geometry)

# PM2.5 -------------------------------------------------------------------
dates = seq(as.Date("2020-08-01"), as.Date("2020-09-10"), "days")

ca.pm25 = paste0(data.dir, 'epa/epa-ca-pm25-2020.csv') %>% 
  fread() %>% 
  mutate(date = as.Date(Date, format = '%m/%d/%Y'),
         stusps = 'CA', county = toupper(COUNTY)) %>% 
  filter(date %in% dates) %>% 
  select(site_id = `Site ID`, lon = SITE_LONGITUDE, lat = SITE_LATITUDE,
         POC, date, pm25 = `Daily Mean PM2.5 Concentration`,
         stusps, county) %>% 
  group_by(site_id, date) %>% 
  filter(POC == min(POC), pm25 >= 0) %>% ungroup() %>% 
  select(-POC)

ca.pm25.daily = lapply(dates,
       function(x) ca.pm25 %>% 
         select(-date, -pm25) %>% 
         unique() %>% 
         mutate(date = x) %>% 
         select(site_id, date, everything())) %>% 
  reduce(rbind) %>% 
  full_join(x = ., y = ca.pm25) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 4326) %>% 
  st_transform(crs = 32610) %>% 
  cbind(., st_coordinates(.) %>% data.table()) %>% 
  rename(site_x = X, site_y = Y) %>% 
  st_drop_geometry() %>% 
  arrange(date, site_id) %>% 
  data.table()

# Plot --------------------------------------------------------------------
ca.notes = data.table(
  note_x = c(375e3, 870e3, 485e3, 775e3, 985e3), 
  note_y = c(3.7e6, 3.73e6, 4.16e6, 4.37e6, 4.17e6),
  note = c("Data: EPA AirData", # "Data: VIIRS Nightfire V3.0",
           "LA", "SF", "SAC", "BFD"))

system.time({
  p = ca.pm25.daily %>%
    mutate(label_x = 375e3, label_y = 3.77e6,
           date_text = format(date, format = '%b %d'),
           alpha = ifelse(is.na(pm25), 0, 1)) %>% 
    # filter(date >= '2020-09-05') %>%
    ggplot() +
    geom_sf(data = ca %>% st_transform(crs = 32610), 
            size = .5, color = 'gray24', alpha = 0) +
    geom_point(aes(x = site_x, y = site_y, color = pm25, alpha = alpha),
               size = 4) +
    # geom_sf(aes(fill = n), size = .5, color = 'grey24') +
    geom_text(aes(x = label_x, y = label_y, label = date_text), 
              color = 'gray88', size = 20, hjust = 'left',
              family = 'Anonymous Pro') +
    geom_text(data = ca.notes,
              aes(x = note_x, y = note_y, label = note), 
              color = 'gray88', size = 8, hjust = 'left',
              family = 'Anonymous Pro') +
    guides(alpha = FALSE) +
    annotate(
      geom = "curve",
      x = 800e3, xend = 640e3,
      y = 4.35e6, yend = 4.27e6,
      size = .8, curvature = -.35, color = 'grey88',
      arrow = arrow(length = unit(0, "mm"))) +
    annotate(
      geom = "curve", 
      x = 1010e3, xend = 872e3,
      y = 4.15e6, yend = 3.92e6,
      size = .8, curvature = -.35, color = 'grey88',
      arrow = arrow(length = unit(0, "mm"))) +
    scale_color_gradientn(
      name = "Daily\nPM2.5",
      colours = c("green", "goldenrod1", "firebrick2", "brown4", "maroon4"),
      values = scales::rescale(c(0, 50, 100, 250, 380)),
      limits = c(0, 380)) +
    theme(legend.background = element_rect(fill = 'black'),
          legend.text = element_text(color = 'gray88', size = 20,
                                     family = 'Anonymous Pro', face = 'bold'),
          legend.title = element_text(color = 'gray88', size = 24,
                                      family = 'Anonymous Pro', face = 'bold'),
          legend.position = c(.95, .95),
          legend.justification = c(1, 1),
          legend.key.height = unit(1.8, 'cm'),
          legend.key.width = unit(1, 'cm'),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'black'),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = 'black')) +
    transition_time(date)
    # transition_events(day, enter_length = 1, exit_length = 1)
  a = animate(p, renderer = av_renderer(paste0(gfx.dir, 'vnf/ca-2020-pm25-daily.mp4')),
              width = 916, height = 1000, res = 100, 
              fps = 36, duration = 16)
})
utils::browseURL(paste0(gfx.dir, 'vnf/ca-2020-pm25-daily.mp4'))
system.time({
  ca.pm25.end.dat = rbind(
    ca.pm25.daily %>% filter(date == '2020-09-10') %>% 
      mutate(date_text = format(date, format = '%b %d')),
    ca.pm25.daily %>% filter(date == '2020-09-10') %>% 
      mutate(date_text = format(date, format = '%b %d'),
             date = date + 1)) %>%
    mutate(label_x = 375e3, label_y = 3.77e6,
           alpha = ifelse(is.na(pm25), 0, 1))
  p = ca.pm25.end.dat %>% 
    ggplot() +
    geom_sf(data = ca %>% st_transform(crs = 32610), 
            size = .5, color = 'gray24', alpha = 0) +
    geom_point(aes(x = site_x, y = site_y, color = pm25, alpha = alpha),
               size = 4) +
    # geom_sf(aes(fill = n), size = .5, color = 'grey24') +
    geom_text(aes(x = label_x, y = label_y, label = date_text), 
              color = 'gray88', size = 20, hjust = 'left',
              family = 'Anonymous Pro') +
    geom_text(data = ca.notes,
              aes(x = note_x, y = note_y, label = note), 
              color = 'gray88', size = 8, hjust = 'left',
              family = 'Anonymous Pro') +
    guides(alpha = FALSE) +
    annotate(
      geom = "curve",
      x = 800e3, xend = 640e3,
      y = 4.35e6, yend = 4.27e6,
      size = .8, curvature = -.35, color = 'grey88',
      arrow = arrow(length = unit(0, "mm"))) +
    annotate(
      geom = "curve", 
      x = 1010e3, xend = 872e3,
      y = 4.15e6, yend = 3.92e6,
      size = .8, curvature = -.35, color = 'grey88',
      arrow = arrow(length = unit(0, "mm"))) +
    scale_color_gradientn(
      name = "Daily\nPM2.5",
      colours = c("green", "goldenrod1", "firebrick2", "brown4", "maroon4"),
      values = scales::rescale(c(0, 50, 100, 250, 380)),
      limits = c(0, 380)) +
    theme(legend.background = element_rect(fill = 'black'),
          legend.text = element_text(color = 'gray88', size = 20,
                                     family = 'Anonymous Pro', face = 'bold'),
          legend.title = element_text(color = 'gray88', size = 24,
                                      family = 'Anonymous Pro', face = 'bold'),
          legend.position = c(.95, .95),
          legend.justification = c(1, 1),
          legend.key.height = unit(1.8, 'cm'),
          legend.key.width = unit(1, 'cm'),
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.background = element_rect(fill = 'black'),
          panel.grid = element_blank(),
          plot.background = element_rect(fill = 'black')) +
    transition_time(date)
  a = animate(p, renderer = av_renderer(paste0(gfx.dir, 'vnf/ca-2020-pm25-end.mp4')),
              width = 916, height = 1000, res = 100, 
              fps = 36, duration = 1)
})
utils::browseURL(paste0(gfx.dir, 'vnf/ca-2020-pm25-end.mp4'))

# ca.notes = data.table(
#   note_x = c(375e3, 870e3, 485e3, 775e3, 985e3), 
#   note_y = c(3.60e6, 3.73e6, 4.16e6, 4.37e6, 4.17e6),
#   note = c("@kenchauplots", "LA", "SF", "SAC", "BFD"))