# tarea 03
# instalar paquetes 
install.packages("tidyverse")
library(tidyverse)
install.packages("palmerpenguins")
library(palmerpenguins)
install.packages("ggthemes")
library(ggthemes)

penguins
glimpse(penguins)
?penguins
# ? abre la pestaña de help

ggplot(data = penguins)
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y =body_mass_g)
) +
  geom_point()

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point()
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = species)
) +
  geom_point() +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species)) +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(mapping = aes(color = species, shape = species)) +
  geom_smooth(method = "lm")
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point(aes(color = species, shape = species)) +
  geom_smooth(method = "lm") +
  labs(
    title = "Body mass and flipper length",
    subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
    x = "Flipper length (mm)", y = "Body mass (g)",
    color = "Species", shape = "Species"
  ) +
  scale_color_colorblind()

# Exercises

# 1. 344 filas y 8 columnas
# 2. largo del pico medido en milimetros
# 3.
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE) +
  labs(x = "Largo del pico (mm)", y = "Profundidad del pico (mm)",
       title = "Bill depth vs. bill length")
# 4. 
ggplot(penguins, aes(x = species, y = bill_depth_mm)) +
  geom_boxplot(na.rm = TRUE)
# boxplot es una mejor opcion

# 5.
ggplot(data = penguins) + 
  geom_point()
# da error porque falta el argumento

# 6.
# indica que hacer con las filas que tienen NA
# el default es FALSE
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE) +
  labs(x = "Bill length (mm)", y = "Bill depth (mm)")

# 7.
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE) +
  labs(
    x = "Bill length (mm)",
    y = "Bill depth (mm)",
    caption = "Data come from the palmerpenguins package."
  )

# 8.
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = bill_depth_mm)) +
  geom_smooth()

# 9.
# x -> largo de la aleta ; y -> masa muscular ; color por isla
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g, color = island)
) +
  geom_point() +
  geom_smooth(se = FALSE)

# 10.
ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point() +
  geom_smooth()

ggplot() +
  geom_point(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  ) +
  geom_smooth(
    data = penguins,
    mapping = aes(x = flipper_length_mm, y = body_mass_g)
  )
# se ven iguales porque usan las mismas variables y ejes

ggplot(
  data = penguins,
  mapping = aes(x = flipper_length_mm, y = body_mass_g)
) +
  geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()
penguins |> 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g)) + 
  geom_point()

ggplot(penguins, aes(x = species)) +
  geom_bar()
ggplot(penguins, aes(x = fct_infreq(species))) +
  geom_bar()

ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 200)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 20)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_histogram(binwidth = 2000)
ggplot(penguins, aes(x = body_mass_g)) +
  geom_density()

# 1.4.3 Excersise
# 1.
ggplot(penguins, aes(y = species)) +
  geom_bar()
# cambian los ejes

# 2. 
ggplot(penguins, aes(x = species)) +
  geom_bar(color = "red")

ggplot(penguins, aes(x = species)) +
  geom_bar(fill = "red")
# el mejor es el segundo, usando fill se nota mas la diferencia 

# 3. 
# bins controla en cuantos intervalos se divide el rango de la variable 
# continua para construir el histograma

# 4. 
ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.5)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.1)

ggplot(diamonds, aes(x = carat)) +
  geom_histogram(binwidth = 0.01)
# el mas revelador es el 0.01

ggplot(penguins, aes(x = species, y = body_mass_g)) +
  geom_boxplot()
ggplot(penguins, aes(x = body_mass_g, color = species)) +
  geom_density(linewidth = 0.75)
ggplot(penguins, aes(x = body_mass_g, color = species, fill = species)) +
  geom_density(alpha = 0.5)
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar()
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill") +
  labs(y = "proportion")
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = island))
ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point(aes(color = species, shape = species)) +
  facet_wrap(~island)

# excercises

# 1.
?mpg
# categorical -> manufacturer, model, trans, drv, fl, class
# numerical -> displ, year, cyl, cty, hwy
glimpse(mpg)
# toda la informacion

# 2.
# Base
ggplot(mpg, aes(x = displ, y = hwy)) +
  geom_point()

# Color
ggplot(mpg, aes(x = displ, y = hwy, color = cyl)) +
  geom_point()

# Size
ggplot(mpg, aes(x = displ, y = hwy, size = cyl)) +
  geom_point()

# Color y size juntos
ggplot(mpg, aes(x = displ, y = hwy, color = cyl, size = cyl)) +
  geom_point()

# Shape (error o warning)
ggplot(mpg, aes(x = displ, y = hwy, shape = cyl)) +
  geom_point()

# el color para categorical usa coleres discretos, mientras que para
# numerica, usa una paleta de un mismo color

# con numerical, el tamaño varia segun el valor
# en categorical, asigna tamaños pero no es tan funcional 

# con numerical, shape no funciona, y solo esta pensado para categorical

# 3.
ggplot(mpg, aes(x = displ, y = hwy, linewidth = cyl)) +
  geom_point()

# 4. 
# funciona sin problemas

# 5. 
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point(na.rm = TRUE)
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm)) +
  geom_point(na.rm = TRUE) +
  facet_wrap(~ species)
# a diferencia de geom_point, faceting separa por especie y no por color 

# 6.
ggplot(
  data = penguins,
  mapping = aes(
    x = bill_length_mm, y = bill_depth_mm, 
    color = species, shape = species
  )
) +
  geom_point() +
  labs(color = "Species")
# para arreglarlo poner el mismo titulo en ambos aes
ggplot(
  data = penguins,
  mapping = aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)
) +
  geom_point() +
  labs(color = "Species", shape = "Species")

# 7.
ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar(position = "fill")
# respondemos el porcentaje de especie por  isla 
ggplot(penguins, aes(x = species, fill = island)) +
  geom_bar(position = "fill")
# responde el porcentaje de isla en la que aparece cada especie
# representan lo mismo pero cambiando las variables

ggplot(penguins, aes(x = flipper_length_mm, y = body_mass_g)) +
  geom_point()
ggsave(filename = "penguin-plot.png")

# excercises
# 1.
ggplot(mpg, aes(x = class)) +
  geom_bar()
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.png")
# el ultimo es guardado asi, porque esta indicado con ggsave

# 2.
ggplot(mpg, aes(x = cty, y = hwy)) +
  geom_point()
ggsave("mpg-plot.pdf")
# cambiar png por pdf
# para saber, usar ?ggsave
?ggsave
