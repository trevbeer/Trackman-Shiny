library(sportyR)

bip <- season %>%
  filter(PitchCall == 'InPlay', HitType != 'Bunt', abs(Bearing) < 50) %>% 
  select(Bearing, Distance, Angle, ExitSpeed, HitType, PlayResult)

bip_converted <- bip %>% 
  summarize(x = round(Distance * cos(Bearing * pi / 180), 3),
            y = round(Distance * sin(Bearing * pi / 180), 3),
            hit_type = HitType,
            launch_angle = round(Angle, 3),
            exit_velo = round(ExitSpeed, 3),
            play_result = PlayResult,
            new_x = Distance * cos(Bearing * pi / 180),
            new_y = Distance * sin(Bearing * pi / 180)) %>% 
  select(new_x, new_y, hit_type, launch_angle, exit_velo, play_result)
mid <- mean(bip_converted$exit_velo, na.rm = T)
spray_chart <- geom_baseball(league = 'MLB') +
  geom_point(data = bip_converted,
             aes(new_y, new_x,
                 color = exit_velo)) +
  scale_color_gradient(low = 'gold', high = 'blue')
spray_chart

duce <- season %>% 
  filter(Batter == 'Duce Gourson', PitchCall == 'InPlay', HitType != 'Bunt', abs(Bearing) < 50) %>% 
  select(Bearing, Distance, Angle, ExitSpeed, HitType, PlayResult, HitSpinRate) %>% 
  summarize(x = round(Distance * cos(Bearing * pi / 180), 3),
            y = round(Distance * sin(Bearing * pi / 180), 3),
            hit_type = HitType,
            launch_angle = round(Angle, 3),
            exit_velo = round(ExitSpeed, 3),
            play_result = PlayResult,
            spin_rate = round(HitSpinRate, 2),
            new_x = Distance * cos(Bearing * pi / 180),
            new_y = Distance * sin(Bearing * pi / 180)) %>% 
  select(new_x, new_y, hit_type, launch_angle, exit_velo, play_result, spin_rate)
duce_spray <- geom_baseball(league = 'MLB') +
  geom_point(data = duce,
             aes(x = new_y, y = new_x,
                 color = exit_velo), size = 5)