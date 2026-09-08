
# tarea 04 ----------------------------------------------------------------


# data transformation -----------------------------------------------------

library(nycflights13)
library(tidyverse)
library(lubridate)

flights
glimpse(flights)

flights |>
  filter(dest == "IAH") |> 
  group_by(year, month, day) |> 
  summarize(
    arr_delay = mean(arr_delay, na.rm = TRUE)
  )

flights |> 
  filter(dep_delay > 120)
flights |> 
  filter(month == 1 & day == 1)
flights |> 
  filter(month == 1 | month == 2)
flights |> 
  filter(month %in% c(1, 2))
jan1 <- flights |> 
  filter(month == 1 & day == 1)

flights |> 
  filter(month = 1)
flights |> 
  filter(month == 1 | 2)

flights |> 
  arrange(year, month, day, dep_time)
flights |> 
  arrange(desc(dep_delay))

flights |> 
  distinct()
flights |> 
  distinct(origin, dest)
flights |> 
  distinct(origin, dest, .keep_all = TRUE)
flights |>
  count(origin, dest, sort = TRUE)


# excercises --------------------------------------------------------------

# 1.
flights |> filter(arr_delay >= 120)
flights |> filter(dest %in% c("IAH", "HOU"))
flights |> filter(carrier %in% c("UA", "AA", "DL"))
flights |> filter(month %in% 7:9)
flights |> filter(arr_delay > 120, dep_delay <= 0)
flights |> filter(dep_delay >= 60, dep_delay - arr_delay > 30)

# 2.
flights |> arrange(desc(dep_delay))
flights |> arrange(dep_time)

# 3.
flights |> arrange(distance / air_time)
flights |> arrange(desc(distance / air_time))

# 4.
flights |> 
  count(year, month, day) |> 
  nrow() == 365

# 5.
flights |> arrange(desc(distance)) |> head(1)
flights |> arrange(distance) |> head(1)

# 6.
# no importa el orden pero si cambia la eficiencia de los resultados
# si se usa primero arrange() analiza todo los datos, siendo una operacion mas larga
# para luego con filter() descartar

flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60
  )
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .before = 1
  )
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    speed = distance / air_time * 60,
    .after = day
  )
flights |> 
  mutate(
    gain = dep_delay - arr_delay,
    hours = air_time / 60,
    gain_per_hour = gain / hours,
    .keep = "used"
  )

flights |> 
  select(year, month, day)
flights |> 
  select(year:day)
flights |> 
  select(!year:day)
flights |> 
  select(where(is.character))
flights |>
  select(starts_with("abc"))
flights |> 
  select(tail_num = tailnum)

flights |> 
  rename(tail_num = tailnum)

flights |> 
  relocate(time_hour, air_time)
flights |> 
  relocate(year:dep_time, .after = time_hour)
flights |> 
  relocate(starts_with("arr"), .before = dep_time)

# excercises
# 1.
# se espera que actuen igual, pero no es asi ya que no son el mismo formato

# 2.
flights |> select(dep_time, dep_delay, arr_time, arr_delay)
flights |> select(4, 6, 7, 9)
flights |> select(starts_with("dep_"), starts_with("arr_"))
flights |> select(contains("dep_"), contains("arr_"))
flights |> select(matches("^(dep|arr)_(time|delay)$"))
cols <- c("dep_time", "dep_delay", "arr_time", "arr_delay")
flights |> select(any_of(cols))
flights |> select(all_of(cols))
flights |> select(dep_time:arr_delay)
flights |> select(ends_with("time"), ends_with("delay")) |> 
  select(dep_time, dep_delay, arr_time, arr_delay) 
flights |> select(-year, -month, -day, -sched_dep_time, -sched_arr_time, 
                  -carrier, -flight, -tailnum, -origin, -dest, 
                  -air_time, -distance, -hour, -minute, -time_hour)

# 3.
# solo mantiene una copia de lo seleccionado ignorando a los duplicados

# 4. 
# any_of() selecciona columnas de los vectores que le indicamos 
# nos sirve porque de otra manera nos daria un error y todo el comando seria fallido

# 5. 
flights |> select(contains("TIME"))
# nos muestra todas las columnas relacionadas con time 
# por default esta libreria es insensible a las mayusculas 
# para cambiar el default:
flights |> select(contains("TIME", ignore.case = FALSE))

# 6.
flights |> 
  rename(air_time_min = air_time) |> 
  relocate(air_time_min)

# 7.
# da error porque esta mal el orden en pipe
# select() nos reduce a solo una columna

flights |> 
  filter(dest == "IAH") |> 
  mutate(speed = distance / air_time * 60) |> 
  select(year:day, dep_time, carrier, flight, speed) |> 
  arrange(desc(speed))
arrange(
  select(
    mutate(
      filter(
        flights, 
        dest == "IAH"
      ),
      speed = distance / air_time * 60
    ),
    year:day, dep_time, carrier, flight, speed
  ),
  desc(speed)
)
flights1 <- filter(flights, dest == "IAH")
flights2 <- mutate(flights1, speed = distance / air_time * 60)
flights3 <- select(flights2, year:day, dep_time, carrier, flight, speed)
arrange(flights3, desc(speed))

flights |> 
  group_by(month)
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay)
  )
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE)
  )
flights |> 
  group_by(month) |> 
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    n = n()
  )
daily <- flights |>  
  group_by(year, month, day)
daily
daily_flights <- daily |> 
  summarize(n = n())
daily_flights <- daily |> 
  summarize(
    n = n(), 
    .groups = "drop_last"
  )
daily |> 
  ungroup()
daily |> 
  ungroup() |>
  summarize(
    avg_delay = mean(dep_delay, na.rm = TRUE), 
    flights = n()
  )
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = month
  )
flights |> 
  summarize(
    delay = mean(dep_delay, na.rm = TRUE), 
    n = n(),
    .by = c(origin, dest)
  )

# excercises
# 1.
flights |> 
  group_by(carrier) |> 
  summarize(
    avg_arr_delay = mean(arr_delay, na.rm = TRUE),
    n = n()
  ) |> 
  arrange(desc(avg_arr_delay))
# el peor delay F9
flights |> 
  group_by(carrier, dest) |> 
  summarize(n = n(), .groups = "drop") |> 
  arrange(dest)
# no se puede encontrar los efectos, ya que los datos y movimientos no son lineales

# 2.
flights |> 
  group_by(dest) |> 
  slice_max(dep_delay, n = 1) |> 
  relocate(dest, dep_delay)

# 3.
flights |> 
  group_by(hour) |> 
  summarize(dep_delay = mean(dep_delay, na.rm = TRUE)) |> 
  ggplot(aes(x = hour, y = dep_delay)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Average Departure Delay by Hour of Day",
    x = "Hour of Day",
    y = "Average Departure Delay (minutes)"
  )

# 4.
# nos devuelve todas las columnas excepto las indicadas

# 5.
# es una forma mas resumida de group_by, summarize, ungroup
# si esta seteado para TRUE, nos devuleve las mas frecuentes primero

# 6.
# a.
# nos va a devulver el mismo dataset
df <- tibble(
  x = 1:5,
  y = c("a", "b", "a", "a", "b"),
  z = c("K", "K", "L", "L", "K")
)
df |>
  group_by(y)

# b.
# nos reordenaria, primero las de a y lugo b
df |>
  arrange(y)
# es diferente porque cambia el orden de las filas

# c.
# lo que va a ocurrir es que todos los datos van a quedar en 2 columnas y e mean_x
df |> 
  group_by(y) |> 
  summarize(mean_x = mean(x))
# el pipe lo que hace es reducir la cantidad de datos

# d.
# agrupara siguiendo las combinaciones de x e y
df |> 
  group_by(y, z) |> 
  summarize(mean_x = mean(x))
# el pipe agrupa la data con la combinacion de las dos variables
# el mensaje nos dice que mas alla de haber agrupado, sigue organizado como lo estaban los grupos
# comenzando con la y

# e.
# va a reproducir lo mismo que el inciso anterior 
df |> 
  group_by(y, z) |> 
  summarize(mean_x = mean(x), .groups = "drop")
# el pipe hace lo mismo que en el d.
# la diferencia con d. es que antes seguian agrupando por y
# mientras que ahora no estan agrupados 

# f.
# el primer pipe agrupa por y y z, y los unta en una fila con promedio de x
# el segundo pipe, agrega una nueva columna manteniendo 5 filas originales
df |> group_by(y, z) |> summarize(mean_x = mean(x))
df |> group_by(y, z) |> mutate(mean_x = mean(x))
# summarize() agrega, junta cada grupo en 1 sola fila y descarta x y cualquier columna qie no sea de resumen 
# mutate() hace lo mismo, pero sin unir 
# summarize() cambia la forma de los datos
# mutate() mantiene la forma original


# keys --------------------------------------------------------------------

# 1.
# la relacion se da mediante origin en la columna weather
# en el diagrama se conectan weather con airports

# 2.
# si tuviera registro se podria agragar una conexion adicional entre wweather y flights

# 3.
weather |> 
  count(year, month, day, hour, origin) |> 
  filter(n > 1)

# 4.
special_days <- tibble(
  year = c(2013, 2013, 2013),
  month = c(12, 12, 1),
  day = c(24, 25, 1),
  holiday = c("Christmas Eve", "Christmas Day", "New Year's Day")
)
flights |> 
  left_join(special_days, join_by(year, month, day))

# 5. 
# no entendi lo que pedia el inciso 


# basic joins -------------------------------------------------------------

# 1. 
worst_hours <- flights |> 
  filter(!is.na(dep_delay)) |> 
  group_by(year, month, day, hour) |> 
  summarize(
    avg_delay = mean(dep_delay + arr_delay, na.rm = TRUE),
    n = n(),
    .groups = "drop"
  ) |> 
  arrange(desc(avg_delay)) |> 
  slice_head(n = 48)
worst_hours_weather <- flights |> 
  filter(!is.na(dep_delay)) |> 
  group_by(year, month, day, hour, origin) |> 
  summarize(avg_delay = mean(dep_delay, na.rm = TRUE), n = n(), .groups = "drop") |> 
  arrange(desc(avg_delay)) |> 
  slice_head(n = 48) |> 
  left_join(weather, join_by(year, month, day, hour, origin))

# 2.
top_dest <- flights |>
  count(dest, sort = TRUE) |>
  head(10)
flights |> 
  semi_join(top_dest, join_by(dest))

# 3.
flights |> 
  anti_join(weather, join_by(year, month, day, hour, origin))

# 4.
flights |> 
  anti_join(planes, join_by(tailnum)) |> 
  count(carrier, sort = TRUE)

# 5.
planes_carriers <- flights |> 
  distinct(tailnum, carrier) |> 
  group_by(tailnum) |> 
  summarize(carriers = paste(sort(unique(carrier)), collapse = ", "), n_carriers = n_distinct(carrier)) |> 
  right_join(planes, join_by(tailnum))
planes_carriers |> select(tailnum, carriers, n_carriers, everything())

# 6.
flights |> 
  left_join(
    airports |> select(faa, lat, lon), 
    join_by(origin == faa)
  ) |> 
  rename(origin_lat = lat, origin_lon = lon) |> 
  left_join(
    airports |> select(faa, lat, lon), 
    join_by(dest == faa)
  ) |> 
  rename(dest_lat = lat, dest_lon = lon)
# es mas facil renombrar antes del join 

# 7.
avg_delay_by_dest <- flights |> 
  group_by(dest) |> 
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n(), .groups = "drop")

avg_delay_by_dest |> 
  inner_join(airports, join_by(dest == faa)) |> 
  ggplot(aes(x = lon, y = lat, color = avg_delay, size = n)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Retraso promedio de llegada por aeropuerto de destino",
    x = "Longitud",
    y = "Latitud",
    color = "Retraso\npromedio (min)",
    size = "N° de vuelos"
  )
avg_delay_by_dest |> 
  inner_join(airports, join_by(dest == faa)) |> 
  filter(lon > -140) |>  # excluye Hawaii/Alaska si distorsionan el mapa
  ggplot(aes(x = lon, y = lat, color = avg_delay, size = n)) +
  borders("state") +
  geom_point(alpha = 0.7) +
  coord_quickmap()

# 8.
flights |> 
  filter(month == 6, day == 13) |> 
  group_by(dest) |> 
  summarize(avg_delay = mean(arr_delay, na.rm = TRUE), n = n(), .groups = "drop") |> 
  inner_join(airports, join_by(dest == faa)) |> 
  ggplot(aes(x = lon, y = lat, color = avg_delay, size = n)) +
  borders("state") +
  geom_point(alpha = 0.8) +
  coord_quickmap() +
  scale_color_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0) +
  labs(
    title = "Retrasos de llegada por destino — 13 de junio de 2013",
    color = "Retraso\npromedio (min)",
    size = "N° de vuelos"
  )
# los retrasos son explicados por una serie de tormentas que hubieron esa noche, verificado por google