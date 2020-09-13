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

ca.hex = ca %>% 
  st_transform(crs = 32610) %>% 
  st_make_grid(cellsize = 15200, # 10750 for 100km2
               offset = c(-134800, 3597500), # 3595000 for 100km2
               square = FALSE) %>% 
  st_sf() %>% 
  mutate(hex_id = 1:nrow(.)) %>% 
  select(hex_id, geometry)

# ggplot() +
#   geom_sf(data = ca) +
#   geom_sf(data = ca %>% 
#             st_transform(crs = 32610) %>% 
#             st_make_grid(cellsize = 15200, 
#                          offset = c(-134800, 3597500),
#                          square = FALSE) %>% 
#             st_sf(), alpha = 0)

# VNF data ----------------------------------------------------------------
dates = seq(as.Date("2020-05-01"), as.Date("2020-09-10"), "days")
vnf.ca.2020 = paste0(data.dir, 'vnf/vnf-ca-2020.rds') %>% 
  readRDS()
# vnfv30.url.pfx = c(
#   "https://eogdata.mines.edu/wwwdata/viirs_products/vnf/v30//VNF_npp_d",
#   "_noaa_v30-ez.csv.gz")
# 
# # name of columns to collect from data set
# vnf.cols = c('date_mscan', 'lon_gmtco', 'lat_gmtco', 'temp_bb', 'temp_bkg',
#              'esf_bb', 'rhi', 'rh', 'area_pixel', 'area_bb', 'cloud_mask')
# # list of dates by year
# dates = seq(as.Date("2020-05-01"), as.Date("2020-09-10"), "days")
# # empty list for daily data
# vnf.ca.2020 = list()
# # loop to download and process
# system.time({
#   for(s in 1:length(dates)){
#     # prep 
#     # starts timer to track download time
#     start = Sys.time()
#     
#     # creates URL and GZ names, with date cutoff between V21 CLASS and V30 GRAVITE
#     # V30 GRAVITE version
#     url.name = paste0(vnfv30.url.pfx[1], 
#                       gsub('-', '', dates[s]), vnfv30.url.pfx[2])
#     gz.name = paste0(data.dir, 'vnf/raw/', 'VNF_npp_d', 
#                      gsub('-','', dates[s]), vnfv30.url.pfx[2])
#     # get CSV name for unzipped file
#     csv.name = gsub(".gz", "", gz.name)
#     
#     # download & process 
#     # try to download the file, error handler in case of no data
#     tryCatch({
#       # message indicating date being downloaded
#       cat("[", as.character(dates[s]),'] download. ', sep='')
#       
#       # downloads the file 
#       download.file(url.name, gz.name, method='auto', quiet=T)
#       # unzips GZ, keeps the CSV, removes the GZ
#       gunzip(gz.name, overwrite=TRUE)
#       # process
#       cat('process. ', sep='')
#       
#       # process the file 
#       vnf.ca.2020[[s]] = fread(csv.name) %>% 
#         rename_all(tolower) %>% # rename all columns to lowercase for convenience
#         select(all_of(vnf.cols)) %>% # collect relevant columns
#         na_if(999999) %>% # replace 999999 as missing
#         # filter(!is.na(temp_bb)) %>% # keep those not missing temperature
#         # generate unique VNF_ID and valid date-format date column
#         mutate(vnf_id = paste0('VNF', 
#                                gsub('-','',dates[s]),
#                                sprintf('%06d', 1:nrow(.))),
#                date=as.Date(substr(date_mscan,1,10), format="%Y/%m/%d")) %>% 
#         # collect and rename certain columns, and drop a few
#         select(vnf_id, date, lon=lon_gmtco, lat=lat_gmtco,
#                everything(), -date_mscan) %>% 
#         na.omit(cols = c('lon', 'lat')) %>% 
#         # make into data.table
#         data.table() %>% 
#         st_as_sf(coords = c('lon', 'lat'), crs = 4326, remove = FALSE)
#       vnf.ca.2020[[s]] = suppressMessages(
#         st_join(x = vnf.ca.2020[[s]],
#                 y = ca %>% select(-everything()),
#                 left = FALSE))
#       
#       # delete CSV file
#       file.remove(csv.name)
#       
#       # report time took t to download and process
#       cat(difftime(Sys.time(), start, units='secs') %>% ceiling(),
#           " secs.\r", sep='')
#     }, error = function(e){
#       print(e)
#     })
#   }
#   vnf.ca.2020 = vnf.ca.2020 %>% reduce(rbind)
# })
# saveRDS(vnf.ca.2020,
#         paste0(data.dir, 'vnf/vnf-ca-2020.rds'))
#
# vnf.conn = dbConnect(drv = dbDriver("PostgreSQL"), 
#                      host = "localhost",
#                      dbname = "vnf", port = 57463, 
#                      user = "kchau", password = "TSb)a-Vj8Gq)x!=5_b?+GhY]")
# 
# system.time({
#   vnf.ca.2020 = st_read(
#     vnf.conn,
#     query = "select vnf.*
#           from vnf, us_counties us
#           where st_contains(us.geom, vnf.geom) and
#                 us.state = 'CALIFORNIA' and
#                 extract(year from date) = 2020;")
# })

# Filter to Hexes ---------------------------------------------------------
ca.hex.vnf = st_join(
  x = ca.hex,
  y = vnf.ca.2020 %>% 
    st_transform(crs = 32610) %>% 
    select(vnf_id, date),
  left = FALSE) %>% 
  st_drop_geometry() %>% 
  group_by(hex_id, date) %>% 
  summarize(n = n(), .groups = 'drop')

system.time({
  ca.hex.vnf.daily = lapply(dates,
                            function(x) ca.hex %>% 
                              st_drop_geometry() %>% 
                              mutate(date = x) %>% 
                              select(hex_id, date)) %>% 
    reduce(rbind) %>% 
    full_join(x = ., y = ca.hex.vnf, by = c('hex_id', 'date')) %>% 
    mutate(n = ifelse(is.na(n), 0, n)) %>% 
    inner_join(x = ca.hex,
               y = .)
})

# Daily -------------------------------------------------------------------
ca.notes = data.table(
  note_x = c(375e3, 375e3, 375e3, 870e3, 485e3, 775e3, 985e3), 
  note_y = c(3.7e6, 3.66e6, 3.62e6, 3.73e6, 4.16e6, 4.37e6, 4.17e6),
  note = c("Data: VIIRS Nightfire",
           "1 Hexagon = 50,000 acres", 
           "@kenchauplots",
           "LA", "SF", "SAC", "BFD"))
system.time({
  p = ca.hex.vnf.daily %>% 
    mutate(label_x = 375e3, label_y = 3.77e6) %>% 
    mutate(date_text = format(date, format = '%b %d')) %>% 
    filter(date >= '2020-08-01') %>%
    # filter(date >= '2020-09-09') %>% 2ca
    ggplot() +
    geom_sf(aes(fill = n), size = .5, color = 'grey24') +
    geom_text(aes(x = label_x, y = label_y, label = date_text), 
              color = 'gray88', size = 20, hjust = 'left',
              family = 'Anonymous Pro') +
    geom_text(data = ca.notes,
              aes(x = note_x, y = note_y, label = note), 
              color = 'gray88', size = 8, hjust = 'left',
              family = 'Anonymous Pro') +
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
    scale_fill_gradientn(
      name = "Nighttime\nfires",
      colours = c("black", "firebrick1", "navajowhite"),
      values = scales::rescale(c(0, 20, 60)),
      limits = c(0, 60)) +
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
  a = animate(p, renderer = av_renderer(paste0(gfx.dir, 'vnf/ca-2020-fires-daily.mp4')),
              width = 916, height = 1000, res = 100, 
              fps = 36, duration = 16)
})
utils::browseURL(paste0(gfx.dir, 'vnf/ca-2020-fires-daily.mp4'))

system.time({
  ca.vnf.total.dat = rbind(
    full_join(
      x = ca.hex,
      y = ca.hex.vnf.daily %>% 
        filter(date >= '2020-08-01') %>% 
        st_drop_geometry() %>% data.table() %>% 
        group_by(hex_id) %>% summarize(n = sum(n)) %>% arrange(-n) %>%
        mutate(date = max(ca.hex.vnf$date) + 1,
               label_x = 375e3, label_y = 3.77e6,
               date_text = "Total")),
    full_join(
      x = ca.hex,
      y = ca.hex.vnf.daily %>% 
        filter(date >= '2020-08-01') %>% 
        st_drop_geometry() %>% data.table() %>% 
        group_by(hex_id) %>% summarize(n = sum(n)) %>% arrange(-n) %>%
        mutate(date = max(ca.hex.vnf$date) + 2,
               label_x = 375e3, label_y = 3.77e6,
               date_text = "Total")))
  p = ca.vnf.total.dat %>% 
    ggplot() +
    geom_sf(aes(fill = n), size = .5, color = 'grey24') +
    geom_text(aes(x = label_x, y = label_y, label = date_text), 
              color = 'gray88', size = 20, hjust = 'left',
              family = 'Anonymous Pro') +
    geom_text(data = ca.notes,
              aes(x = note_x, y = note_y, label = note), 
              color = 'gray88', size = 8, hjust = 'left',
              family = 'Anonymous Pro') +
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
    scale_fill_gradientn(
      name = "Nighttime\nfires",
      colours = c("black", "firebrick1", "navajowhite"),
      values = scales::rescale(c(0, 50, 300)),
      limits = c(0, 300)) +
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
  a = animate(p, renderer = av_renderer(paste0(gfx.dir, 'vnf/ca-2020-fires-end.mp4')),
              width = 916, height = 1000, res = 100, 
              fps = 36, duration = 1)
})
utils::browseURL(paste0(gfx.dir, 'vnf/ca-2020-fires-end.mp4'))

# Yearly --------------------------------------------------------------------
full_join(x = ca.hex %>% st_transform(crs = 32610),
          y = ca.hex.vnf %>% 
            filter(month(date) > 4) %>% 
            group_by(hex_id) %>% 
            summarize(n = n())) %>% 
  mutate(n = ifelse(is.na(n), 0, n)) %>% 
  ggplot() + 
  geom_sf(aes(fill = n), 
          size = .5, color = 'grey24') +
  scale_fill_gradientn(
    name = "Hotspots",
    colours = c("black", "firebrick3", "navajowhite"),
    values = scales::rescale(c(0, 35, 110)),
    limits = c(0, 110)) +
  theme(legend.background = element_rect(fill = 'black'),
        legend.text = element_text(color = 'white', size = 18,
                                   family = 'Anonymous Pro', face = 'bold'),
        legend.title = element_text(color = 'white', size = 20,
                                    family = 'Anonymous Pro', face = 'bold'),
        legend.position = c(.95, .95),
        legend.justification = c(1, 1),
        legend.key.height = unit(2, 'cm'),
        legend.key.width = unit(1, 'cm'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = 'black'),
        panel.grid = element_blank(),
        plot.background = element_rect(fill = 'black')) +
  ggsave(filename = paste0(gfx.dir, 'ca-2020-fires-total.png'),
         height = 10, width = 9.15, dpi = 450)




# test codes --------------------------------------------------------------
# ca.notes = data.table(
#   note_x = c(375e3, 375e3, 870e3, 485e3, 775e3, 985e3), 
#   note_y = c(3.65e6, 3.60e6, 3.73e6, 4.16e6, 4.37e6, 4.17e6),
#   note = c("1 Hexagon = 50,000 acres", "@kenchauplots",
#            "LA", "SF", "SAC", "BFD"))
# ca.notes = data.table(
#   note_x = c(375e3, 375e3, 870e3, 485e3, 775e3, 985e3), 
#   note_y = c(3.68e6, 3.63e6, 3.73e6, 4.16e6, 4.37e6, 4.17e6),
#   note = c("1 Hexagon = 50,000 acres", "@kenchauplots",
#            "LA", "SF", "SAC", "BFD"))
# 
# ca.hex.vnf.daily %>% 
#   mutate(label_x = 375e3, label_y = 3.75e6) %>% 
#   mutate(date_text = format(date, format = '%b %d')) %>% 
#   filter(date == '2020-07-01') %>%
#   ggplot() +
#   geom_sf(
#     data = ca.hex,
#     # data = ca %>% st_transform(crs = 32610),
#     alpha = 0, size = .5, color = 'grey24') +
#   geom_text(aes(x = label_x, y = label_y, label = date_text), 
#             color = 'gray88', size = 20, hjust = 'left',
#             family = 'Anonymous Pro') +
#   geom_text(data = ca.notes,
#             aes(x = note_x, y = note_y, label = note), 
#             color = 'gray88', size = 8, hjust = 'left',
#             family = 'Anonymous Pro') +
#   annotate(
#     geom = "curve", 
#     x = 800e3, xend = 640e3,
#     y = 4.35e6, yend = 4.27e6,
#     size = .8, curvature = -.35, color = 'grey88',
#     arrow = arrow(length = unit(0, "mm"))) +
#   annotate(
#     geom = "curve", 
#     x = 1010e3, xend = 872e3,
#     y = 4.15e6, yend = 3.92e6,
#     size = .8, curvature = -.35, color = 'grey88',
#     arrow = arrow(length = unit(0, "mm"))) +
#   theme(axis.text = element_blank(),
#         axis.ticks = element_blank(),
#         panel.background = element_rect(fill = 'black'),
#         panel.grid = element_blank(),
#         plot.background = element_rect(fill = 'black'))

