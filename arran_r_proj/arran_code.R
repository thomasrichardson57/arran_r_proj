#----Packages----
library(ggplot2) # plots
library(readxl) # reading in excel sheets
library(tidyr) # pivot data from wide to long
library(dplyr) # create data frames while cross referencing other objects (SBL)
library(viridis) # colour blind friendly colours
library(ggbreak) # add breaks on some of the axis scales
library(patchwork) #combine plots
library(purrr) # functional category mapping
#----Import the data----

# birds
birds.samples <- as.data.frame(read_xlsx('data/birds.xlsx', sheet = 1))
birds.samples$eventDate <- as.numeric(birds.samples$eventDate)

birds.occ <- as.data.frame(read_xlsx('data/birds.xlsx', sheet = 2))
birds.occ$eventDate <- as.numeric(birds.occ$eventDate)

birds.occ$site <- as.factor(substr(birds.occ$eventID, 1, 1))


category_mapping <-list(
  carnivore=c('Golden eagle','Common raven', 'Common kestrel'),
  granivore=c('Wood pigeon'),
  insectivore=c('Long tailed tit','Skylark','Meadow pipit',
                'Blue tit','European robin','Pied wagtail',
                'Eurasion wren', 'Coal tit',
                'Unidentified thrush','Song thrush', 'Treecreeper'),
  generalist = c('Western jackdaw', 'Carrion crow')
)

# Create an inverted mapping from species to categories.
species_to_category <- unlist(lapply(names(category_mapping), function(category) {
  setNames(rep(category, length(category_mapping[[category]])), category_mapping[[category]])
}))

# Add new category column to dataframe
birds.occ <- birds.occ %>%
  mutate(category = species_to_category[Commonname])

south.birds <- birds.occ[birds.occ$site == 'S', ]
north.birds <- birds.occ[birds.occ$site == 'N', ]


# bats
bats.samples.joe <- as.data.frame(read_xlsx('data/bat.joe.xlsx', sheet = 1))
bats.samples.gina <- as.data.frame(read_xlsx('data/bat.joe.gina.ben.xlsx', sheet = 1))
bats.samples <- rbind(bats.samples.joe, bats.samples.gina)
# colnames(bats.samples) <- colnames(birds.samples)

bats.samples$eventDate <- as.numeric(bats.samples$eventDate)

bats.occ.joe <- as.data.frame(read_xlsx('data/bat.joe.xlsx', sheet = 2))
bats.occ.gina <- as.data.frame(read_xlsx('data/bat.joe.gina.ben.xlsx', sheet = 2))
bats.occ <- rbind(bats.occ.joe, bats.occ.gina)
# colnames(birds.samples) <- colnames(birds.occ)

bats.occ$eventDate <- as.numeric(bats.occ$eventDate)

bats.occ$site <- as.factor(substr(bats.occ$eventID, 1, 1))

south.bats <- bats.occ[bats.occ$site == 'S', ]
north.bats <- bats.occ[bats.occ$site == 'N', ]

# fresh water invert

water.inverts.samples <- as.data.frame(read_xlsx('data/kick.sample.xlsx', sheet = 1))
# colnames(water.inverts.samples) <- colnames(birds.samples)

water.inverts.occ <- as.data.frame(read_xlsx('data/kick.sample.xlsx', sheet = 2))
# colnames(water.inverts.occ) <- colnames(birds.occ)
names(water.inverts.occ)[16] <- 'BMWP'

water.inverts.occ$site <- as.factor(substr(water.inverts.occ$eventID, 1, 1))
water.inverts.occ$stream <- as.factor(substr(water.inverts.occ$eventID, 7, 7))
water.inverts.occ$streamID <- as.factor(water.inverts.occ$site : water.inverts.occ$stream)

south.water.inverts <- water.inverts.occ[water.inverts.occ$site == 'S', ]
north.water.inverts <- water.inverts.occ[water.inverts.occ$site == 'N', ]

# mammals

mammals.samples <- as.data.frame(read_xlsx('data/mammals.xlsx', sheet = 1))
# colnames(mammals.samples) <- colnames(birds.samples)

mammals.occ <- as.data.frame(read_xlsx('data/mammals.xlsx', sheet = 2))
# colnames(mammals.occ) <- colnames(birds.occ)

mammals.occ$site <- as.factor(substr(mammals.occ$eventID, 1, 1))

south.mammals <- mammals.occ[mammals.occ$site == 'S', ]
north.mammals <- mammals.occ[mammals.occ$site == 'N', ]

# moths 

moths.samples <- as.data.frame(read_xlsx('data/moths.xlsx', sheet = 1))
# colnames(moths.samples) <- colnames(birds.samples)

moths.occ <- as.data.frame(read_xlsx('data/moths.xlsx', sheet = 2))
# colnames(moths.occ) <- colnames(birds.occ)

moths.occ$site <- as.factor(substr(moths.occ$eventID, 1, 1))

south.moths <- moths.occ[moths.occ$site == 'S', ]
north.moths <- moths.occ[moths.occ$site == 'N', ]

# plants

plants.samples <- as.data.frame(read_xlsx('data/plants.xlsx', sheet = 1))
# colnames(plants.samples) <- colnames(birds.samples)

plants.occ <- as.data.frame(read_xlsx('data/plants.xlsx', sheet = 2))
# colnames(plants.occ) <- colnames(birds.occ)

plants.occ$site <- as.factor(substr(plants.occ$eventID, 1, 1))

south.plants <- plants.occ[plants.occ$site == 'S', ]
north.plants <- plants.occ[plants.occ$site == 'N', ]

# pit falls and sweep nets

net.pit.samples <- as.data.frame(read_xlsx('data/sweep.pit.fall.xlsx', sheet = 1))
# colnames(net.pit.samples) <- colnames(birds.samples)

net.pit.occ <- as.data.frame(read_xlsx('data/sweep.pit.fall.xlsx', sheet = 2))
# colnames(net.pit.occ) <- colnames(birds.occ)

net.pit.occ$site <- as.factor(substr(net.pit.occ$eventID, 1, 1))

south.net.pit <- net.pit.occ[net.pit.occ$site == 'S', ]
north.net.pit <- net.pit.occ[net.pit.occ$site == 'N', ]

# all the data as one

all.samples <- as.data.frame(read_xlsx('data/all.xlsx', sheet = 1))
# colnames(all.samples) <- colnames(birds.samples)

all.occ <- as.data.frame(read_xlsx('data/all.xlsx', sheet = 2))
# colnames(all.occ) <- colnames(birds.occ)
all.occ$site <- as.factor(substr(all.occ$eventID, 1, 1))

south.all <- all.occ[all.occ$site == 'S', ]
north.all <- all.occ[all.occ$site == 'N', ]
#----Richness----

#### Richness Data frame ####

# create a data frame with the richness of the different taxa, and
# the different sites

all.unique.spp <- unique(all.occ$scientificName)
richness.all <- length(all.unique.spp)
richness.all

south.all <- all.occ[all.occ$site == 'S', ]
S.richness.all <- length(unique(south.all$scientificName))
S.richness.all

north.all <- all.occ[all.occ$site == 'N', ]
N.richness.all <- length(unique(north.all$scientificName))
N.richness.all

rich.fig.mat.all <- data.frame(
  site = c('N', 'S'),
  Total = c(N.richness.all, S.richness.all)
)
rich.fig.mat.all$site <- as.factor(rich.fig.mat.all$site)

# add columns for each taxa

rich.fig.mat.all$birds <- 0
rich.fig.mat.all$birds <- c(length(unique(north.birds$scientificName)),
                        length(unique(south.birds$scientificName)))

rich.fig.mat.all$bats <- 0
rich.fig.mat.all$bats <- c(length(unique(north.bats$scientificName)),
                        length(unique(south.bats$scientificName)))


rich.fig.mat.all$mammals <- 0
rich.fig.mat.all$mammals <- c(length(unique(north.mammals$scientificName)),
                        length(unique(south.mammals$scientificName)))


rich.fig.mat.all$moths <- 0
rich.fig.mat.all$moths <- c(length(unique(north.moths$scientificName)),
                        length(unique(south.moths$scientificName)))

rich.fig.mat.all$net.pit <- 0
rich.fig.mat.all$net.pit <- c(length(unique(north.net.pit$scientificName)),
                        length(unique(south.net.pit$scientificName)))

rich.fig.mat.all$plants <- 0
rich.fig.mat.all$plants <- c(length(unique(north.plants$scientificName)),
                        length(unique(south.plants$scientificName)))

rich.fig.mat.all$water.inverts <- 0
rich.fig.mat.all$water.inverts <- c(length(unique(north.water.inverts$scientificName)),
                        length(unique(south.water.inverts$scientificName)))

# help from ELM - create a long form dataframe for plotting easier

rich.long.all <- pivot_longer(
  rich.fig.mat.all,
  cols = -site,
  names_to = 'Taxon',
  values_to = 'Richness'
)
rich.long.all

#### All taxa ####
# 
# p.all.rich <- ggplot(rich.long.all, aes(x = site, y = Richness,
#                       color = Taxon, 
#                       group = Taxon)) +
#   scale_fill_viridis_d(option = 'E') +
#   geom_point() +
#   geom_line() + 
#   labs(x = 'Proposed Site', y = 'Baseline Species Richness') +
#   coord_cartesian(ylim = c(0,100)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw() 
# p.all.rich
# 
# ggsave('figs/Richness_plot.png', dpi = 1000)

#----Scottish Biodiversity List----

# avoid disbturbing
SBL.ter.all <- read_xls('data/Scottish_Biodiversity_List.xls', sheet = 3)
SBL.ter.avoid.all <- subset(SBL.ter.all, SBL.ter.all$`Avoid negative impacts`== 'Yes')

# threatened per IUCN category
SBL.ter.red.list <- subset(SBL.ter.all, SBL.ter.all$`Threatened species` == 'Red')
SBL.ter.amber.list <- subset(SBL.ter.all, SBL.ter.all$`Threatened species` == 'Amber')

#### All Avoid ####

# create a df with only the species that appear in the SBL 'avoid disturbing' list
avoid.all <- all.occ %>%
  semi_join(SBL.ter.avoid.all, 
            by = c('scientificName' = 'Scientific Name'))

# compare between the sites
avoid.all.S <- subset(avoid.all, avoid.all$site == 'S')
avoid.all.N <- subset(avoid.all, avoid.all$site == 'N')

length(unique(avoid.all.S$scientificName))
length(unique(avoid.all.N$scientificName))

unique(avoid.all.S$scientificName)
unique(avoid.all.N$scientificName)

unique(avoid.all.S$Commonname)
unique(avoid.all.N$Commonname)

#### All IUCN Threatened ####

# red
redlist.all <- all.occ %>%
  semi_join(SBL.ter.red.list, 
            by = c('scientificName' = 'Scientific Name'))

redlist.N <- subset(redlist.all, redlist.all$site == 'N')
redlist.S <- subset(redlist.all, redlist.all$site == 'S')


# amber
amberlist.all <- all.occ %>%
  semi_join(SBL.ter.amber.list, 
            by = c('scientificName' = 'Scientific Name'))

amberlist.N <- subset(amberlist.all, amberlist.all$site == 'N')
amberlist.S <- subset(amberlist.all, amberlist.all$site == 'S')


# Some taxa in SBL are classified by the two letter 
# codes (e.g., CR = critically endangered), not by red or amber list so need to filter
# slightly differently

# Critically Endangered, CR
sbl.CR <- subset(SBL.ter.all, SBL.ter.all$`Threatened species` == 'CR')
all.CR.N <- north.all %>%
  semi_join(sbl.CR,
            by = c('scientificName' = 'Scientific Name'))
all.CR.S <- south.all %>%
  semi_join(sbl.CR,
            by = c('scientificName' = 'Scientific Name'))

# Endangered, EN
sbl.EN <- subset(SBL.ter.all, SBL.ter.all$`Threatened species` == 'EN')
all.EN.N <- north.all %>%
  semi_join(sbl.EN,
            by = c('scientificName' = 'Scientific Name'))
all.EN.S <- south.all %>%
  semi_join(sbl.EN,
            by = c('scientificName' = 'Scientific Name'))
#----SBL + Div + ... combi graph----
## sch1
sch_1 <- read_xlsx('data/schedule_1.xlsx', sheet = 1)
head(sch_1)

sch_1_names <- sch_1$`Current taxon name`

sch_1_occ_birds <- birds.occ %>%
  filter(scientificName %in% sch_1_names)
head(sch_1_occ_birds)

# split n vs s

sch1_birds_N <- sch_1_occ_birds[sch_1_occ_birds$site == 'N',]
sch1_birds_S <- sch_1_occ_birds[sch_1_occ_birds$site == 'S',]

combi.sbl.rich <- data.frame(
  site = as.factor(c('N', 'S')),
  a.richness = c(length(unique(north.all$scientificName)), 
               length(unique(south.all$scientificName))),
  b.sbl_avoid = c(length(unique(avoid.all.N$scientificName)), 
                length(unique(avoid.all.S$scientificName))),
  c.red_listed = c(length(unique(redlist.N$scientificName)),
                 length(unique(redlist.S$scientificName))),
  d.amber_listed = c(length(unique(amberlist.N$scientificName)),
                   length(unique(amberlist.S$scientificName))),
  f.sch1 = c(length(unique(sch1_birds_N$scientificName)),
             length(unique(sch1_birds_S$scientificName))),
  g.birds = c(length(unique(north.birds$scientificName)),
            length(unique(south.birds$scientificName))),
  h.bats = c(length(unique(north.bats$scientificName)),
           length(unique(south.bats$scientificName))),
  i.moths = c(length(unique(north.moths$scientificName)),
            length(unique(south.moths$scientificName))),
  j.water_inverts = c(length(unique(north.water.inverts$Commonname)),
                     length(unique(south.water.inverts$Commonname))),
  k.terrestrial = c(length(unique(north.net.pit$scientificName)),
                  length(unique(south.net.pit$scientificName))),
  l.plants = c(length(unique(north.plants$scientificName)),
               length(unique(south.plants$scientificName)))
)
combi.sbl.rich

# convert to long (ELM help)

combi.sbl.rich.long <- as.data.frame(
  combi.sbl.rich %>%
  pivot_longer(
    cols = c(colnames(combi.sbl.rich[-1])),
    names_to = 'measure', 
    values_to = 'value'
  )
)
combi.sbl.rich.long

combi.sbl.rich.long$measure <- as.factor(combi.sbl.rich.long$measure)
combi.sbl.rich.long$site <- as.factor(combi.sbl.rich.long$site)


# combi.sbl.rich.long$measure <- relevel(combi.sbl.rich.long$measure, 'richness')

p1 <- ggplot(combi.sbl.rich.long, aes(measure, value, fill = site)) +
  geom_bar(stat = 'identity', position = 'dodge', col = 'black') +
  scale_y_continuous(breaks = seq(0, 80, 5),
                     limits = c(0,82)) + 
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North', 'S' = 'South')) + 
  # theme_bw() +
  geom_text(aes(label = value), 
                        position = position_dodge(width = 0.9), 
                        vjust = -0.5,
            size = 3) + 
  scale_y_break(c(25,70)) +
  geom_hline(yintercept = 0) +
  theme_bw() +
  theme(axis.text.y.right = element_blank(),
        axis.ticks.y.right = element_blank(),
        axis.title.y.right = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  scale_x_discrete(labels = c(
    'a.richness' = 'Overall', 'b.sbl_avoid' = 'SBL Avoid Disturbing',
    'c.red_listed' = 'IUCN Red Listed', 'd.amber_listed' = 'IUCN Amber Listed',
    'f.sch1'= 'Schedule 1 listed',
    'g.birds' = 'Birds', 'h.bats' = 'Bats', 'i.moths' = 'Moths', 
    'j.water_inverts' = 'Fresh Water Invertebrates', 
    'k.terrestrial' = 'Terrestrial Invertebrates',
    'l.plants' = 'Plants')) +
  labs(x = NULL, y = 'Species Richness')

p1

ggsave('figs/combi.sbl.plot.png', dpi = 1000)

#----Birds----

#### Functional Groups ####

func_birds <- data.frame(
  site = c('N', 'S'),
  carnivores = c(length(which(north.birds$category == 'carnivore')),
                 length(which(south.birds$category == 'carnivore'))),
  insectivores = c(length(which(north.birds$category == 'insectivore')),
                   length(which(south.birds$category == 'insectivore'))),
  granivores = c(length(which(north.birds$category == 'granivore')),
                   length(which(south.birds$category == 'granivore'))),
  generalists = c(length(which(north.birds$category == 'generalist')),
                  length(which(south.birds$category == 'generalist')))
)
func_birds

func_birds_long <- as.data.frame(
  func_birds %>%
  pivot_longer(cols = -site, names_to = "func", values_to = "count")
)
func_birds_long

func_birds_long$site <- as.factor(func_birds_long$site)
func_birds_long$func <- as.factor(func_birds_long$func)
func_birds_long$count <- as.numeric(func_birds_long$count)

# # func_birds_long[8,3] <- NA
# library(tidyverse)
# 
# # Ensure all categories are present for each site
# categories <- c("carnivore", "insectivore", "granivore", "generalist")
# sites <- c("N", "S")
# func_birds_long <- expand.grid(site = sites, category = categories) %>%
#   left_join(func_birds_long, by = c("sites", "categories")) %>%
#   mutate(count = ifelse(is.na(count), 0, count))


ggplot(func_birds_long, aes(x = "", y = count, fill = func)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar(theta = "y") +
  facet_wrap(~ site) +
  labs(title = "Bird Categories by Site")


#### Richness ####
S.richness.birds <- length(unique(south.birds$scientificName))
S.richness.birds

N.richness.birds <- length(unique(north.birds$scientificName))
N.richness.birds

rich.fig.mat.birds <- data.frame(
  site = c('North', 'South'),
  Total = c(N.richness.birds, S.richness.birds)
)
rich.fig.mat.birds$site <- as.factor(rich.fig.mat.birds$site)
# 
# rich.long.birds <- pivot_longer(
#   rich.fig.mat.birds,
#   cols = -site,
#   values_to = 'Richness'
# )
# rich.long.birds
# 
# rich.fig.birds <- ggplot(rich.long.birds, aes(site, Richness)) +
#   geom_point()+
#   labs(x = 'Proposed Site', y = 'Baseline Bird Species Richness') +
#   coord_cartesian(ylim = c(10,14)) +
#   geom_hline(yintercept = 0, linetype = 2) +
#   theme_bw()
# rich.fig.birds
# 
# ggsave('figs/Richness_plot_birds.png', dpi = 1000)

#### SBL + Sch1 ####


## sbl

# subset sbl list to just bird spp
sbl_birds <- SBL.ter.avoid.all[SBL.ter.avoid.all$`Taxon group` == 'bird',]

avoid.birds <- birds.occ %>%
  semi_join(sbl_birds, 
            by = c('scientificName' = 'Scientific Name'))

sbl_birds_names <- avoid.birds$scientificName

sb.avoid.birds.N <- north.birds %>%
  filter(scientificName %in% sbl_birds_names)
sb.avoid.birds.S <- south.birds %>%
  filter(scientificName %in% sbl_birds_names)

## graph

# build a dataframe for ggplot

birds_fig_df <- data.frame(
  site = c('N', 'S'),
  richness = c(length(unique(north.birds$scientificName)),
               length(unique(south.birds$scientificName))),
  sbl_avoid = c(length(unique(sb.avoid.birds.N$scientificName)),
                length(unique(sb.avoid.birds.S$scientificName))),
  sch_1 = c(length(unique(sch1_birds_N$scientificName)),
            length(unique(sch1_birds_S$scientificName)))
)
birds_fig_df

# transform to long code for ggplot
birds_fig_df_long <- as.data.frame(birds_fig_df %>%
  pivot_longer(
    cols = c(richness, sbl_avoid, sch_1),
    names_to = 'measure', 
    values_to = 'value'
  )
)

#### plot ####

p2 <- ggplot(birds_fig_df_long, aes(site, value, fill = measure)) + 
  geom_bar(stat = 'identity', position = 'dodge', col = 'black') +
  scale_fill_viridis_d(option = 'E') + 
  labs(x = 'Site', y = 'N') + 
  theme_bw() +
  geom_hline(yintercept = 0) + 
  coord_cartesian(ylim = c(0,15)) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5)
p2

ggsave('figs/bird_div_plot.png', dpi = 1000)

#----Inverts----

#### spp richness ####

# N
n.terres.invert.spp.rich <- length(unique(north.net.pit$scientificName))
n.terres.invert.spp.rich

n.water.invert.spp.rich <- length(unique(north.water.inverts$scientificName))
n.water.invert.spp.rich

n.moths.spp.rich <- length(unique(north.moths$scientificName))
n.moths.spp.rich

n.invert.spp.rich <- sum(n.terres.invert.spp.rich, n.water.invert.spp.rich,
                         n.moths.spp.rich)
n.invert.spp.rich
# S
s.terres.invert.spp.rich <- length(unique(south.net.pit$scientificName))
s.terres.invert.spp.rich

s.water.invert.spp.rich <- length(unique(south.water.inverts$scientificName))
s.water.invert.spp.rich

s.moths.spp.rich <- length(unique(south.moths$scientificName))
s.moths.spp.rich

s.invert.spp.rich <- sum(s.terres.invert.spp.rich, s.water.invert.spp.rich,
                         s.moths.spp.rich)
s.invert.spp.rich

#### order richness ####

# N
n.terres.inver.order.rich <- length(unique(north.net.pit$Order))
n.terres.inver.order.rich

n.water.invert.order.rich <- length(unique(north.water.inverts$Order))
n.water.invert.order.rich

n.moths.order.richness <- length(unique(north.moths$scientificName)) 
n.moths.order.richness

n.invert.order.rich <- sum(n.terres.inver.order.rich, n.water.invert.order.rich,
                           n.moths.order.richness)
n.invert.order.rich

# S
s.terres.inver.order.rich <- length(unique(south.net.pit$Order))
s.terres.inver.order.rich

s.water.invert.order.rich <- length(unique(south.water.inverts$Order))
s.water.invert.order.rich

s.moths.order.richness <- length(unique(south.moths$scientificName)) 
s.moths.order.richness

s.invert.order.rich <- sum(s.terres.inver.order.rich, s.terres.inver.order.rich,
                           s.moths.order.richness)
s.invert.order.rich

#### plot ####

# df
#####
# invert_fig_df <- data.frame(
#   site = c('N', 'S'),
#   total.spp = c(n.invert.spp.rich, s.invert.spp.rich),
#   total.order = c(n.invert.order.rich, s.invert.order.rich),
#   water.spp = c(n.water.invert.spp.rich, s.water.invert.spp.rich),
#   water.order = c(n.water.invert.order.rich, s.water.invert.order.rich),
#   terr.spp = c(n.terres.invert.spp.rich, s.terres.invert.spp.rich),
#   terr.order
# )
# invert_fig_df
# invert_fig_df_long <- as.data.frame(invert_fig_df %>%
#                                      pivot_longer(
#                                        cols = c( spp, order),
#                                        names_to = 'taxonomic_resolution', 
#                                        values_to = 'richness'
#                                      )
# )
# invert_fig_df_long
# 
# ggplot(invert_fig_df_long, aes(site, richness, fill = taxonomic_resolution)) + 
#   geom_bar(stat = 'identity', position = 'dodge') + 
#   scale_fill_viridis_d(option = 'E') + 
#   theme_bw() +
#   geom_hline(yintercept = 0) +
#   coord_cartesian(ylim = c(0,50)) +
#   geom_text(aes(label = richness), 
#             position = position_dodge(width = 0.9), 
#             vjust = -0.5)
# 
# ggsave('figs/inverts_div_plot.png', dpi = 1000)
#####
invert_df_long <- data.frame(
  site = c('N', 'S',
           'N', 'S',
           'N', 'S',
           'N', 'S',
           'N', 'S',
           'N', 'S',
           'N', 'S',
           'N', 'S'),
  value = c(n.invert.spp.rich, s.invert.spp.rich, 
            n.invert.order.rich, s.invert.order.rich,
            n.water.invert.spp.rich, s.water.invert.spp.rich,
            n.water.invert.order.rich, s.water.invert.order.rich,
            n.terres.invert.spp.rich, s.terres.invert.spp.rich,
            n.terres.inver.order.rich, n.terres.inver.order.rich,
            n.moths.spp.rich, s.moths.spp.rich,
            n.moths.order.richness, s.moths.order.richness),
  resolution = c('Species', 'Species',
                 'Order', 'Order',
                 'Species', 'Species',
                 'Order', 'Order',
                 'Species', 'Species',
                 'Order', 'Order',
                 'Species', 'Species',
                 'Order', 'Order'),
  group = c('Total', 'Total',
            'Total', 'Total',
            'Fresh water', 'Fresh water',
            'Fresh water', 'Fresh water',
            'Terrestrial','Terrestrial',
            'Terrestrial','Terrestrial',
            'Moth traps', 'Moth traps',
            'Moth traps', 'Moth traps')
)

p3 <- ggplot(invert_df_long, aes(resolution, value, fill = site)) +
  geom_bar(stat = 'identity', position = 'dodge', col = 'black') + 
  geom_hline(yintercept = 0) +
  facet_wrap(~group) + 
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North', 'S' = 'South'))+
  geom_text(aes(label = value), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5) +
  coord_cartesian(ylim = c(0,50)) +
  scale_y_continuous(breaks = seq(0, 50, 10)) +
  theme_bw() +
  theme(legend.position = 'top') +
  labs(x = 'Taxonomic Resolution', y = 'Richness')
p3

ggsave('figs/inverts_combi_div_plot.png', dpi = 1000)

#----Mammals----

#### richness ####

n.mammal.rich <- length(unique(north.mammals$scientificName))
n.mammal.rich

s.mammal.rich <- length(unique(south.mammals$scientificName))
s.mammal.rich

#### sbl ####

sbl_mammals <- SBL.ter.avoid.all[SBL.ter.avoid.all$`Taxon group` == 'land mammal',]

avoid.mammals <- mammals.occ %>%
  semi_join(sbl_mammals, 
            by = c('scientificName' = 'Scientific Name'))

#----Plants----

#### SBL ####
sbl.plants <- rbind(subset(SBL.ter.all, SBL.ter.all$`Main group` == 'Vascular plants'), 
  subset(SBL.ter.all, SBL.ter.all$`Main group` == 'Non vascular plants'))

sbl.avoid.plants <- subset(sbl.plants, sbl.plants$`Avoid negative impacts` == 'Yes')                       

avoid.plants <- plants.occ %>%
  semi_join(sbl.avoid.plants, 
            by = c('scientificName' = 'Scientific Name')) # 0 on avoid list

#### IUCN ####

# plants are done by two letter codes, not coloured lists as in birds and mammals
# on the SBL

sbl.CR.plants <- rbind(
  subset(sbl.CR, sbl.CR$`Main group` == 'Vascular plants'),
  subset(sbl.CR, sbl.CR$`Main group` == 'Non vascular plants')
  )

sbl.EN.plants <- rbind(
  subset(sbl.EN, sbl.EN$`Main group` == 'Vascular plants'),
  subset(sbl.EN, sbl.EN$`Main group` == 'Non vascular plants')
  )

north.plants.CR <- north.plants %>%
  semi_join(sbl.CR.plants, 
            by = c('scientificName' = 'Scientific Name')) # none!
north.plants.EN <- north.plants %>%
  semi_join(sbl.EN.plants, 
            by = c('scientificName' = 'Scientific Name')) # none!
south.plants.CR <- south.plants %>%
  semi_join(sbl.CR.plants, 
            by = c('scientificName' = 'Scientific Name')) # none!
south.plants.EN <- south.plants %>%
  semi_join(sbl.EN.plants, 
            by = c('scientificName' = 'Scientific Name')) # none!
# none on either list!


# avoid.all <- all.occ %>%
#   semi_join(SBL.ter.avoid.all, 
#             by = c('scientificName' = 'Scientific Name'))

#### Richness ####

plant.rich.df <- data.frame(
  site = c('N', 'S'),
  spp = c(length(unique(north.plants$scientificName)),
          length(unique(south.plants$scientificName))),
  order = c(length(unique(north.plants$Order)),
            length(unique(south.plants$Order)))
)
plant.rich.df

plant.rich.df_long <- 
  as.data.frame(plant.rich.df %>%
                  pivot_longer(
                    cols = c(spp, order),
                    names_to = 'measure',
                    values_to = 'value'
                  ))
plant.rich.df_long

# plot

p4 <- ggplot(plant.rich.df_long, aes(site, value, fill = measure)) +
  geom_bar(stat = 'identity', position = 'dodge', col = 'black') +
  scale_fill_viridis_d(option = 'E') +
  theme_bw() +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = value), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5)
p4

ggsave('figs/plants_richness.png', dpi = 1000)

#### Taxonomic distribution ####

plant_taxa <- data.frame(
  order = as.factor(rep(unique(plants.occ$Order), 2)),
  site = as.factor(c(rep('N', 18), rep('S', 18)))
)


plants.occ$Order <- as.factor(plants.occ$Order)


plant_taxa$n <- rep(0, length(plant_taxa))
i <- 1
for (i in 1:nrow(plant_taxa)){
 # plant_taxa$n[i] <- sum(which(plants.occ))
  plant_taxa$n[i] <- sum(plants.occ$Order == plant_taxa$order[i] & plants.occ$site == plant_taxa$site[i], na.rm = T)
  i <- i + 1
}
plant_taxa

# plot

p.plant <- ggplot(plant_taxa, aes(order, n, fill = site)) + 
  geom_bar(stat = 'identity', position = 'dodge', col = 'black') + 
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North', 'S' = 'South'))+ 
  geom_hline(yintercept = 0) +
  theme_bw() + 
  labs(x = 'Order', y = 'Count') +
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top') +
  geom_text(aes(label = n), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  coord_cartesian(ylim = c(0,60))
p.plant

ggsave('figs/plant_order.png', dpi = 1000)

#----FWI----

#### diversity by site ####

wet.invert.site.richness <- as.data.frame(
  water.inverts.occ %>%
  group_by(eventID) %>%
  summarise(richness = n_distinct(Commonname))
  )

wet.invert.site.richness[9,] <- 'N_total'
wet.invert.site.richness[9,2] <- length(unique(north.water.inverts$Commonname))


wet.invert.site.richness[10,] <- 'S_total'
wet.invert.site.richness[10,2] <- length(unique(south.water.inverts$Commonname))


wet.invert.site.richness$site <- 
  as.factor(substr(wet.invert.site.richness$eventID, 1,1))
wet.invert.site.richness$richness <- 
  as.numeric(wet.invert.site.richness$richness)

p5 <- ggplot(wet.invert.site.richness, aes(eventID, richness, fill = site)) +
  geom_bar(stat = 'identity', col = 'black') + 
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North', 'S' = 'South'))+
  theme_bw()  +
  scale_x_discrete(labels = c(
    'ND4FI_A' = 'Downstream NE', 'ND4FI_B' = 'Downstream NW',
    'NU4FI_A' = 'Upstream NE', 'NU4FI_B'= 'Upstream NW',
    'SD4FI_A' = 'Downstream SE', 'SD4FI_B' = 'South Bog',
    'SU4FI_A' = 'Upstream SE', 'SU4FI_B' = 'Upstream of South Bog',
    'N_total' = 'N Total', 'S_total' = 'S Total'
  ))+
  theme(axis.text.x = element_text(angle = 30, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top') + 
  geom_hline(yintercept = 0) +
  labs(x = 'Site', y = 'Unique Common Names') +
  coord_cartesian(ylim = c(0,15)) +
  geom_text(aes(label = richness), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) 
p5

ggsave('figs/fwi_div.png', dpi = 1000)


#### Diversity by Stream ####
wet.invert.stream.richness <- as.data.frame(
  water.inverts.occ %>%
    group_by(streamID) %>%
    summarise(richness = n_distinct(Commonname))
)
wet.invert.stream.richness$streamID <- 
  as.character(wet.invert.stream.richness$streamID)

wet.invert.stream.richness[5,] <- 'N_total'
wet.invert.stream.richness[5,2] <- length(unique(north.water.inverts$Commonname))


wet.invert.stream.richness[6,] <- 'S_total'
wet.invert.stream.richness[6,2] <- length(unique(south.water.inverts$Commonname))

wet.invert.stream.richness$streamID <- 
  as.factor(wet.invert.stream.richness$streamID)

wet.invert.stream.richness$site <- 
  as.factor(substr(wet.invert.stream.richness$streamID, 1,1))
wet.invert.stream.richness$richness <- 
  as.numeric(wet.invert.stream.richness$richness)


p6 <- ggplot(wet.invert.stream.richness, aes(streamID, richness, fill = site)) +
  geom_bar(stat = 'identity', col = 'black') + 
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                      labels = c('N' = 'North', 'S' = 'South'))+
  theme_bw()  +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'top',
        axis.text.x = element_text(angle = 30, hjust = 1)) + 
  geom_hline(yintercept = 0) +
  labs(x = 'Stream', y = 'Species (Common name) Richness',
       title = 'A') +
  scale_x_discrete(labels = c('N:A' = 'Northwest', 'N:B' = 'Northeast',
                              'N_total' = 'North Overall', 
                              'S:A' = 'Southeast', 'S:B' = 'Southwest (Bog)',
                              'S_total' = 'South Overall')) +
  coord_cartesian(ylim = c(0,15)) +
  geom_text(aes(label = richness), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) 
p6

#### BMWP Scores ####

# hist
ggplot(water.inverts.occ, aes(BMWP)) + 
  geom_histogram(aes(fill = streamID), position = 'dodge',
                 binwidth = 1) + 
  scale_fill_viridis_d() +
  theme_minimal() + 
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)

# calculate ASPT for each stream
fwi.cols.check <- c('BMWP')

fwi.bmwp.clean <- water.inverts.occ[complete.cases(
  water.inverts.occ[fwi.cols.check]),]

ASPT.scores <- c(sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'N:A'], 
                     na.rm = T) / 
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'N:A']),
                 sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'N:B'], 
                     na.rm = T) / 
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'N:B']),
                 sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'S:A'], 
                     na.rm = T) / 
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'S:A']),
                 sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'S:B'], 
                     na.rm = T) / 
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$streamID == 'S:B']),
                 sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$site == 'N'], na.rm = T) /
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$site == 'N']),
                 sum(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$site == 'S'], na.rm = T) /
                   length(fwi.bmwp.clean$BMWP[fwi.bmwp.clean$site == 'S']))
ASPT.scores

# make a fig df

fwi.aspt <- data.frame(
  stream = as.factor(c('N:A', 'N:B', 'S:A', 'S:B', 'N_total', 'S_total')),
  aspt = ASPT.scores
)
fwi.aspt

# plot it

rounded.aspt <- sprintf('%.2f', ASPT.scores)
fwi.aspt$site <- as.factor(substr(fwi.aspt$stream, 1, 1))


p7 <- ggplot(fwi.aspt, aes(stream, aspt, fill = site)) +
  geom_bar(stat = 'identity', col = 'black') +
  guides(fill = guide_legend(title = 'Proposed Site')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North', 'S' = 'South'))+
  theme_bw() + 
  geom_hline(yintercept = 0) +
  labs(x = 'Stream', y = 'ASPT Score', title = 'B') +
  scale_x_discrete(labels = c('N:A' = 'Northwest', 'N:B' = 'Northeast',
                   'N_total' = 'North Overall', 
                   'S:A' = 'Southeast', 'S:B' = 'Southwest (Bog)',
                   'S_total' = 'South Overall')) +
  geom_text(aes(label = rounded.aspt), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  coord_cartesian(ylim = c(0,7)) +
  scale_y_continuous(breaks = seq(0, 7, 1))
  
p7 

ggsave('figs/aspt_scores_by_steam.png', dpi = 1000)  

comb_inv_plot <- (p6 + p7)  +
  plot_layout(guides = 'collect') &
  theme(legend.position = 'bottom') &
  labs(x = "Stream") +
  theme(axis.title.x = element_text())
comb_inv_plot

ggsave('figs/comb_invert_plot.png', dpi = 1000)

#----Bats-----

bats_obs_north <- sum(length(north.bats$eventID))
bats_obs_north

bats_obs_south <- sum(length(south.bats$eventID))
bats_obs_south

unique(north.bats$Commonname)
unique(south.bats$Commonname)

bat_obs_df <- data.frame(
  site <- c('N','S'),
  obs_count <- c(bats_obs_north, bats_obs_south)
)
bat_obs_df
colnames(bat_obs_df) <- c('site', 'obs_count')
bat_obs_df$site <- as.factor(bat_obs_df$site)

p_bats_activity <- 
  ggplot(bat_obs_df, aes(site, obs_count)) + 
  geom_bar(stat  ='identity')
p_bats_activity

#----NBN----

#### import area reports ####
nbn_south <- read.csv('data/south_nbn.csv', header = F)
head(nbn_south)

nbn_north <- read.csv('data/north_nbn.csv', header = F)
head(nbn_north)

nbn_area <- read.csv('data/area_nbn.csv', header = F)
head(nbn_area)

#### prune for useful rows ####
nbn_df <- data.frame(
  Site = as.factor(c('N', 'S', 'Area')),
  Invasive_Species = c(nbn_north[12, 2], nbn_south[12, 2], nbn_area[12, 2]),
  Scottish_Biodiversity_List = c(nbn_north[15, 2], nbn_south[15, 2], nbn_area[15, 2]),
  RSPB_Priority_List = c(nbn_north[16, 2], nbn_south[16, 2], nbn_area[16, 2]),
  Badgers = c(nbn_north[30, 2], nbn_south[30, 2], nbn_area[30, 2]),
  UK_Biodiverity_Action_Plan = c(nbn_north[34, 2], nbn_south[34, 2], nbn_area[34, 2]), # UK biodiversity action plan
  IUCN_amber_birds = c(nbn_north[35, 2], nbn_south[35, 2], nbn_area[35, 2]),
  IUCN_red_birds = c(nbn_north[38, 2], nbn_south[38, 2], nbn_area[38, 2]),
  BBS_CR = c(nbn_north[53, 2], nbn_south[53, 2], nbn_area[53, 2]), # Breeding bird survey critically endangered
  BBS_EN = c(nbn_north[56, 2], nbn_south[56, 2], nbn_area[56, 2]),
  BBS_VU = c(nbn_north[61, 2], nbn_south[61, 2], nbn_area[61, 2]),
  WCA_Schedule_5 = c(nbn_north[71, 2], nbn_south[71, 2], nbn_area[71, 2]),
  WCA_Schedule_1_I_II = c(nbn_north[81, 2], nbn_south[81, 2], nbn_area[81, 2]),# Part 1 and 2 combined
  WCA_Schedule_9_Invasives = c(nbn_north[50, 2], nbn_south[50, 2], nbn_area[50, 2]) 
)

nbn_long_df <- as.data.frame(nbn_df %>%
  pivot_longer(
    cols = -Site,          
    names_to = "category", 
    values_to = "count"    
  )
)
nbn_long_df$count <- as.numeric(nbn_long_df$count)

# totals per site, ELM help

# Calculate the totals for the North and South sites
north_totals <- sum(nbn_long_df$count[nbn_long_df$Site == "N"])
south_totals <- sum(nbn_long_df$count[nbn_long_df$Site == "S"])

# Create data frames for the totals
north_df <- data.frame(Site = "N", category = "Total_N", count = north_totals)
south_df <- data.frame(Site = "S", category = "Total_S", count = south_totals)

# Combine the originals with the totals
nbn_long_df <- rbind(nbn_long_df, north_df, south_df)

#### plot ####

p_nbn <- ggplot(nbn_long_df, aes(category, count, fill = Site)) +
  geom_bar(stat = 'identity', position = 'dodge', col = 'black')  +
  guides(fill = guide_legend(title = 'NBN Search Area')) +  
  scale_fill_viridis_d(option = 'E',
                       labels = c('N' = 'North Site', 'S' = 'South Site',
                                  'Area' = 'Lochranza Area'))+
  theme_bw() + 
  labs(x = 'Category Retrieved from NBN', y = 'Observations from NBN Category') +
  geom_hline(yintercept = 0) +
  geom_text(aes(label = count), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5,
            size = 3) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 30, hjust = 1)) +
  # coord_cartesian(ylim = c(0,45)) + 
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  geom_vline(xintercept = seq(0.5, length(unique(nbn_long_df$category)) - 0.5, by = 1), 
             linetype = "dashed", color = "grey")
  
p_nbn

ggsave('figs/nbn_plot.png', dpi = 1000)


















