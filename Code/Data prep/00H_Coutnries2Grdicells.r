setwd("C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange")
        gpkg_file <- "C:\\Users\\basti\\Documents\\GitHub\\Mangroves_ClimateChange\\Data\\input\\grid_nighlights_spatial.gpkg"
        grid <- st_read(gpkg_file)
        glimpse(grid)
        ggplot(grid) + geom_sf()
        countries <- ne_countries(scale = "medium", returnclass = "sf")
        grid_with_countries <- st_join(grid, countries)
        
        duplicated_ids <- duplicated( grid_with_countries$id)
                if(any(duplicated_ids)) {
                duplicated_ids_list <-  grid_with_countries$id[duplicated_ids]
                print(unique(duplicated_ids_list))
                } else {
                print("No duplicated ids found")
                }


                grid_with_countries$intersection_area <- 0  # Initialize the column

                for(i in      duplicated_ids_list ) {
                intersection <- st_intersection(grid$geom[i], countries$geometry)
                grid_with_countries$intersection_area[which(grid_with_countries$id %in% i)] <- st_area(intersection)
                
                }
                        
       glimpse( grid_with_countries)

       grid_with_largest_area <- grid_with_countries %>%
        group_by(id) %>%
        filter(intersection_area == max(intersection_area)) %>%
        ungroup()

        glimpse(grid_with_largest_area )
        grid_with_countries <- grid_with_largest_area
        grid_with_countries %>% filter(name_en=="Netherlands")

        grid_with_countries %>% st_drop_geometry() %>% as.data.frame() %>% select(name_en,iso_a3) %>% group_by(name_en) %>% summarise(count=n()) %>% as.data.frame()
                
        
        
        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(id %in%  duplicated_ids_list),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(id %in%  duplicated_ids_list),
                aes(label = name_en), size = 2, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 
                
                ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(sovereignt=="France"),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(sovereignt == "France"),
                aes(label = name_en), size = 2, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 

        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(sovereignt=="Netherlands"),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(id==329),
                aes(label = name_en), size = 5, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 

        grid_with_countries %>% filter(name_en == "France") %>% select(name_en,iso_a3,id)

        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(id %in% c(337,338,343)),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(name_en=="France",id %in% c(337,338,343)),
                aes(label = name_en), size = 5, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 

        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(name_en=="France",id %in% c(358,359,360,361,362)),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(name_en=="France",id %in% c(358,359,360,361,362)),
                aes(label = iso_a3), size = 5, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 

        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(id %in% c(181,182,197,289,323,324,329)),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(id %in% c(181,182,197,289,323,324,329)), #lbeled as GBR in final results
                aes(label = iso_a3), size = 5, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 

grid_with_countries %>% filter(id %in% c(181,182,197,289,323,324,329)) %>% as.data.frame()

        ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(name_en=="France"),color="red") +
        geom_sf_label(data = grid_with_countries %>% filter(name_en=="France",id %in% c(622)),
                aes(label = name_en), size = 5, colour = "blue", 
                nudge_x = 2, nudge_y = 2,label.size=0,fill=NA) 


        grid_with_countries %>% filter(name_en == "France",id %in% c(337,338,343)) %>% select(name_en,iso_a3,id)
        grid_with_countries <- grid_with_countries %>% 
                                mutate(iso_a3 = if_else(id %in% c(358,359,360,361,362),"GUF",iso_a3)) #French Guiana
        grid_with_countries <- grid_with_countries %>% 
                                mutate(iso_a3 = if_else(id %in% c(622),"MYT",iso_a3)) #Mayotte
        grid_with_countries <- grid_with_countries %>% 
                                mutate(iso_a3 = if_else(id %in% c(337),"MAF",iso_a3)) #Saint Martin
        grid_with_countries <- grid_with_countries %>% 
                                mutate(iso_a3 = if_else(id %in% c(338),"GLP",iso_a3)) #Guadeloupe
        grid_with_countries <- grid_with_countries %>% 
                                mutate(iso_a3 = if_else(id %in% c(343),"MTQ",iso_a3)) #Martinique

        
        grid_with_countries %>% st_drop_geometry() %>% as.data.frame() %>% select(name_en,iso_a3) %>% group_by(iso_a3) %>% summarise(count=n()) %>% as.data.frame()


           ggplot(grid) + geom_sf()+
        geom_sf(data = grid_with_countries %>% filter(is.na(iso_a3)),color="red")       


glimpse(grid_with_countries)


grid_with_countries %>% filter(iso_a3==-99) %>% as.data.frame()

grid_with_countries_nogeom <- grid_with_countries %>% st_drop_geometry() %>% dplyr::select(id,iso_a3)
#write.csv(grid_with_countries_nogeom,"Data/grid_with_specific_countries_adm0_iso.csv")



    fp <- 'Data\\eez_v11.gpkg'
    eez_gpkg <- st_read(fp)
    eez.spatial <- as(eez_gpkg, 'Spatial')
    eez_countries <- levels(factor(eez_gpkg$TERRITORY1))
    class(eez_gpkg)
    glimpse(eez_gpkg)

grid_with_countries_na <- grid_with_countries %>% filter(is.na(iso_a3))

invalid_grid <- !st_is_valid(grid_with_countries_na)
invalid_eez <- !st_is_valid(eez_gpkg)

# Repair invalid geometries
if(any(invalid_grid)) {
  grid_with_countries_na[invalid_grid] <- st_make_valid(grid_with_countries_na[invalid_grid])
}

if(any(invalid_eez)) {
  eez_gpkg[invalid_eez,] <- st_make_valid(eez_gpkg[invalid_eez,])
}


grid_with_countries_na_eez <- st_join(grid_with_countries_na, eez_gpkg)
glimpse(grid_with_countries_na_eez)
grid_with_countries_na_eez %>% filter(is.na(ISO_TER1))

grid_with_countries_na_eez$iso_a3<- grid_with_countries_na_eez$ISO_TER1



duplicated_ids <- duplicated( grid_with_countries_na_eez$id)
                if(any(duplicated_ids)) {
                duplicated_ids_list <-  grid_with_countries_na_eez$id[duplicated_ids]
                print(unique(duplicated_ids_list))
                } else {
                print("No duplicated ids found")
                }


                grid_with_countries_na_eez$intersection_area <- 0  # Initialize the column
                glimpse(grid_with_countries_na_eez)
                crs_grid_with_countries_na_eez <- st_crs(grid_with_countries_na_eez)
                for(i in      duplicated_ids_list ) {
                    # Transform CRS of eez_gpkg to match grid_with_countries_na_eez
                    st_crs(eez_gpkg) == crs_grid_with_countries_na_eez
                     eez_gpkg <- st_transform(eez_gpkg, crs_grid_with_countries_na_eez )
                    # }
                    intersection <- st_intersection(grid_with_countries_na_eez$geom[which(grid_with_countries_na_eez$id==i)][1], eez_gpkg)
                    grid_with_countries_na_eez$intersection_area[which(grid_with_countries_na_eez$id %in% i)] <- st_area(intersection)
                
                }
       glimpse(grid_with_countries_na_eez)

       grid_with_largest_area <- grid_with_countries_na_eez %>%
        group_by(id) %>%
        filter(intersection_area == max(intersection_area)) %>%
        ungroup()

        glimpse(grid_with_largest_area )
        grid_with_countries_na_eez_norepeated <- grid_with_largest_area
        









grid_with_countries_Nona <- grid_with_countries %>% filter(!is.na(iso_a3))

glimpse(grid_with_countries_Nona)
glimpse(grid_with_countries_na_eez_norepeated)

grid_with_countries_na_eez_norepeated2 <- grid_with_countries_na_eez_norepeated %>% dplyr::select(colnames(grid_with_countries_Nona))
grid_with_countries_ALL <- rbind(grid_with_countries_na_eez_norepeated2,grid_with_countries_Nona)

glimpse(grid_with_countries_ALL)



grid_with_countries_ALL <- grid_with_countries_ALL %>% st_drop_geometry() %>% dplyr::select(id,iso_a3) %>% arrange(id)

write.csv(grid_with_countries_ALL,"Data/grid_with_specific_countries_FINAL.csv")
