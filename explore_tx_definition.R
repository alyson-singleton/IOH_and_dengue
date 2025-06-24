
dengue_data_w_buffers_sf <- st_as_sf(dengue_data_w_buffers,
                                     coords = c("longitude", "latitude"),
                                     crs = 4326)

dengue_data_w_buffers_sf <- dengue_data_w_buffers_sf %>%
  group_by(key) %>%
  summarize(total_cases = sum(monthly_cases),
            onekm=max(onekm),
            twokm=max(twokm),
            threekm=max(threekm),
            fourkm=max(fourkm),
            fivekm=max(fivekm),
            tenkm=max(tenkm),
            fifteenkm=max(fifteenkm),
            twentykm=max(twentykm),
            thirtykm=max(thirtykm),
            fortykm=max(fortykm),
            #population = max(population),
            #mean_temp = mean(mean_temp),
            #sum_precip = sum(sum_precip),
            #urban = max(urban),
            #ag = max(ag),
            all_cutoffs = max(all_cutoffs),
            cluster = max(clust))
mapview(dengue_data_w_buffers_sf, zcol="all_cutoffs")
