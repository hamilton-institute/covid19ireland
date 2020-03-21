df <- all_tables %>% 
  map("counties") %>% 
  bind_rows() %>% 
  mutate(published = rep(
    unlist(all_tables %>% map("published")), 
    times = c(25, 25, 23)
  )) %>% 
  mutate(`Number of Cases` = 
           as.numeric(`Number of Cases`)) %>% 
  setNames(c("county", "cases", "total", "pub")) %>% 
  mutate(cases = ifelse(is.na(cases), 5, cases)) %>% 
  dplyr::filter(!is.na(cases)) %>% 
  group_by(county) %>% 
  mutate(change = cases - lead(cases, default = 0), 
         change = change/lead(cases, default = 1), 
         change = ifelse(change > 1, 0, change)) %>% 
  mutate(label = scales::percent(round(change, 1))) %>% 
  ungroup() 

pdf("plots/counties_ireland.pdf", 
    width = 12, height = 10)
df %>%   
ggplot(aes(x = county, y = cases, group = county)) +
  geom_point(aes(colour = pub), size = 6, alpha = 0.75) +
  geom_line(aes(colour = pub)) +
  geom_text(
    data = df %>% filter(cases > 20),  
    aes(label = label),
            hjust = +0.5, vjust = -2, size = 3) +
  theme_bw(18) +
  theme(
    legend.position = "bottom"
  ) +
  coord_flip() +
  labs(
    title "Number of cases evolution per county, with % of daily increase"
    colour = "Date Published", x = "County", 
       y = "Number of Cases") 

dev.off()