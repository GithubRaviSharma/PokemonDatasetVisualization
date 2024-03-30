install.packages("ggplot2")
install.packages("tidyverse")

library(tidyverse)
library(ggplot2)
library(dplyr)

pokemon_data<- read_csv("D:/RProjectPokemon/pokemon.csv")
head(Pokemon)



#plot1
ggplot(data = pokemon_data, aes(x = against_dragon, y = defense)) +
  geom_point() +
  labs(title = "Scatter Plot for  Attack vs Defense", x = "Attack Against Dragon Types", y = "Defense")

#plot2
ggplot(data = pokemon_data) +
  geom_histogram(mapping = aes(x = hp), 
                 binwidth = 5,  
                 fill = "Red",  
                 color = "black"  
  ) +
  labs(title = "Distribution of HitPoints of Various Pokémons", 
       x = "HitPoints", 
       y = "Frequency",
  ) 

#plot3
ggplot(data = pokemon_data) +
  geom_bar(mapping = aes(x = is_legendary, fill = factor(is_legendary))) +
  labs(title = "Frequency of Legendary Pokémons", 
       x = "Legendary Pokemon", 
       y = "Number of Pokemons"
  ) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme_minimal()

#plot 4
ggplot(data = pokemon_data) +
  geom_bar(mapping = aes(x = type1, fill = factor(is_legendary))) +
  labs(title = "Number of Legendary Pokemon by Type", 
       x = "Type of the pokemon", 
       y = "Total Number"
  ) +
  scale_fill_manual(values = c("skyblue", "orange")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  

#plot 5
ggplot(data = Pokemon) +
  geom_point(mapping = aes(x = hp, y = sp_attack)) +
  labs(
    title = "Scatterplot between HP and Special Attack",
    x = "HitPoints",
    y = "Special Attack"
  ) +
  theme_minimal()

#plot6
ggplot(data = pokemon_data) +
  geom_bar(mapping = aes(x = "", fill = type1)) +
  coord_polar(theta = "y") +
  labs(title = "Percentage Distribution of Various Pokémon Types", fill = "Type") +
  theme_minimal()

#plot7
library(ggplot2)
library(tidyverse)
Pokemon$is_legendary <- as.factor(Pokemon$is_legendary)
ggplot(data = Pokemon) +
  geom_boxplot(mapping = aes(x = generation, y = sp_attack, fill = is_legendary)) +
  labs(title = "Box Plot of Special Attack V/s Generation ",
       x = "Generation of Pokemon",
       y = "Special Attack",
       fill = "Legendary") +
  theme_minimal()

#Plot8
ggplot(data = pokemon_data) +
  geom_density(mapping = aes(x = hp, fill = factor(is_legendary)), alpha = 0.5) +
  labs(
    title = "Density Plot of HitPoints for Legendary and Non-Legendary Pokémons",
    x = "HitPoints",
    fill = "Legendary"
  ) +
  theme_minimal()

#plot9
ggplot(data = pokemon_data) +
  geom_point(mapping = aes(x = sp_attack, y = sp_defense, color = factor(is_legendary))) +
  labs(
    title = "Scatter Plot of Special Attack V/s Special Defense",
    x = "Special Attack",
    y = "Special Defense",
    color = "Legendary"
  ) +
  scale_color_manual(values = c("blue", "red")) +  
  theme_minimal()

#plot10
average_male_percentage <- pokemon_data %>%
  group_by(type1) %>%
  summarise(average_male_percentage = mean(percentage_male, na.rm = TRUE))

ggplot(data = average_male_percentage) +
  geom_bar(mapping = aes(x = type1, y = average_male_percentage), stat = "identity", fill = "skyblue") +
  labs(title = "Average Male Percentage for Each Pokémon Type",
       x = "Type",
       y = "Average Male Percentage") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot11
ggplot(data = pokemon_data, aes(x = type1, y = capture_rate)) +
  geom_point(position = position_jitter(width = 0.3, height = 0), color = "pink") +
  labs(title = "Scatter Plot of the Capture Rate for Each Pokémon Type",
       x = "Pokemon Type",
       y = "Capture Rate") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot12
ggplot(data = pokemon_data) +
  geom_density(mapping = aes(x = speed), fill = "orange", alpha = 0.7) +
  labs(title = "Density Plot of Attack of Different Pokémon", x = "Attack") +
  theme_minimal()

#plot13
library(GGally)
data(pokemon_data)
pokemon_data$is_legendary <- factor(pokemon_data$is_legendary)
ggpairs(pokemon_data, columns = 5:10, ggplot2::aes(color = is_legendary)) +
  ggtitle("Scatterplot Matrix of Columns 5 to 10 with Is Legendary Color Coding")

#plot14
library(ggplot2)
library(tidyverse)
average_values <- pokemon_data %>%
  group_by(type1) %>%
  summarise(
    average_egg_steps = mean(base_egg_steps, na.rm = TRUE),
    average_happiness = mean(base_happiness, na.rm = TRUE),
    average_base_total = mean(base_total, na.rm = TRUE)
  )

average_values_long <- average_values %>%
  pivot_longer(cols = c(average_egg_steps, average_happiness, average_base_total),
               names_to = "Variable", values_to = "Value")
ggplot(data = average_values_long, aes(x = type1, y = Value, color = Variable, group = Variable)) +
  geom_line() +
  geom_point() +
  labs(title = "Average Base Egg Steps, Base Happiness, and Base Total for Each Pokémon Type",
       x = "Type",
       y = "Average Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#plot15
ggplot(data = pokemon_data) +
  geom_density(mapping = aes(x = base_total, fill = factor(is_legendary)), alpha = 0.5, color = "black") +
  scale_fill_manual(values = c("red", "blue")) +  
  labs(
    title = "Density Plot of HitPoints for Legendary and Non-Legendary Pokémon",
    x = "HitPoints",
    fill = "Legendary"
  ) +
  theme_minimal()

#plot16
library(ggplot2)
library(gridExtra)

type_1_poke <- ggplot(data = pokemon_data, aes(x = type1)) +
  geom_bar(fill = "skyblue", alpha = 0.8, stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Distribution Based on Type-1") +
  coord_flip()

type_2_poke <- ggplot(data = pokemon_data, aes(x = type2)) +
  geom_bar(fill = "skyblue", alpha = 0.8, stat = "count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  ggtitle("Distribution Based on Type-2") +
  coord_flip()

grid.arrange(type_1_poke, type_2_poke, ncol = 2)

#plot17
library(ggplot2)
library(ggrepel)
library(dplyr)

pokemon_data$is_legendary <- as.factor(pokemon_data$is_legendary)

ggplot(data = pokemon_data, aes(sp_attack, sp_defense)) +
  geom_point(aes(color = is_legendary), alpha = 0.8) +
  scale_color_manual(values = c("darkblue", "red")) +  
  ggtitle("Special Defense V/s Special Attack Characteristics") +
  ggrepel::geom_label_repel(
    data = subset(pokemon_data, attack > 150 | defense > 150 | attack < 25),
    aes(label = name),
    box.padding = 0.35,
    point.padding = 0.5,
    segment.color = 'grey50'
  )

#plot18
library(ggplot2)
library(RColorBrewer)

hm.palette <- colorRampPalette(rev(brewer.pal(5, 'RdYlBu')), space='Lab')

ggplot(data = pokemon_data, aes(x = type1, y = type2)) +
  geom_tile(aes(fill = generation)) +
  ggtitle("Non-Legendary Pokemon: Type1 and Type2 - Attributes") +
  scale_fill_gradientn(colors = hm.palette(100)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  coord_equal()


#plot19
library(ggplot2)
library(dplyr)
unique_types <- unique(pokemon$type1)
colors <- c("red", "blue", "green", "purple", "orange", "pink", "brown", "grey", "yellow", "cyan", "magenta", "pink", "turquoise", "salmon", "red", "blue", "maroon", "navy")

if (length(colors) != length(unique_types)) {
  stop("Number of colors must match the number of unique types.")
}
pokemon %>%
  group_by(type1) %>%
  summarise(number = n()) %>%
  ggplot(aes(x = reorder(type1, number), y = number, fill = type1)) +
  geom_bar(stat = 'identity') +
  labs(x = "Types of Pokemons", y = "Numbers of Pokemons", title = "Number of Pokemons by their Type") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none") +
  coord_flip() +
  geom_text(aes(label = number), hjust = -0.1, vjust = 0.4) +
  scale_fill_manual(values = setNames(colors, unique_types))


#plot20
library(tidyverse)

pokemon_data %>%
  gather(., key, value, hp, speed, defense, attack, sp_attack, sp_defense) %>%
  group_by(., type1, key) %>%
  summarise(., Stat = as.integer(mean(value))) %>%
  ggplot(., aes(y = type1, x = key)) + 
  geom_tile(aes(fill = Stat)) +
  theme_bw() + 
  theme(legend.position = 'bottom') +
  geom_text(aes(label = Stat), color = 'white', size = 3) +
  labs(x = 'Stats Category', y = 'Pokemon Type', title = 'Heatmap Distribution of Different Pokemon Stats by their Types') +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme(legend.position = "none")







