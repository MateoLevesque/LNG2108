#### SETUP ####
library(tidyverse)
library(tidytext)

mots <- "ce sont difusé à travers les langues"
mots

# Standardisation et nettoyage
mots <- mots |>
  str_to_lower()
mots <- str_replace_all(mots, pattern = "[:punct:]", replacement = "")
mots
##### REGLES DE TRANSCRIPTION #####
##### MOTS UNIQUES (EXEPTIONS)
mots <- str_replace_all(mots, pattern = "monsieur", replacement = "MSWSJø")
mots <- str_replace_all(mots, pattern = "yeux", replacement = "ZJø")
mots <- str_replace_all(mots, pattern = "hier", replacement = "IJAIr")
mots <- str_replace_all(mots, pattern = "cactus", replacement = "KAKTYS")
mots <- str_replace_all(mots, pattern = "temps", replacement = "Tɑ̃")

#### VOYELLES, SEMI-VOYELLES ET NASALES ####
## Gestion des diacritiques + quelques terminaisons
# E
mots <- str_replace_all(mots, pattern = "ées|ée|és$|és(?= )|é|ers$|ers(?= )|er$|er(?= )|^et$|(?<= )et(?= )|(?<=i)ed$|(?<=i)ed(?= )|ai$|ai(?= )|ez$|ez(?= )", replacement = "E")
# i
mots <- str_replace_all(mots, pattern = "î|ï|ies$|ie$|ies(?= )|ie(?= )|it$|it(?= )|its$|its(?= )|is$|is(?= )", replacement = "i")
# AI
mots <- str_replace_all(mots, pattern = "ès$|ès(?= )|è|êts$|êts(?= )|ê|ë|aient$|aient(?= )|aix$|aix(?= )|aî|ai(?!l)|ei(?!l)|ects$|ect$|ects(?= )|ect(?= )|est$|est(?= )|et$|et(?= )|^es$|(?<= )es(?= )", replacement = "AI")
# Y
mots <- str_replace_all(mots, pattern = "û|üe|ü|ues$|ues(?= )|ue$|ue(?= )|uts$|uts(?= )|ut$|ut(?= )|us$|us(?= )", replacement = "Y")
# c deveient s devant eau
mots <- str_replace_all(mots, pattern = "c(?=e)", replacement = "s")
# o
mots <- str_replace_all(mots, pattern = "ô|eaux$|eaux(?= )|aulx$|aulx(?= )|ault$|ault(?= )|aux$|aux(?= )|eau|au", replacement = "o")
# a (anterieur)
mots <- str_replace_all(mots, pattern = "à|as$|as(?= )", replacement = "a")
# a (posterieur)
mots <- str_replace_all(mots, pattern = "â", replacement = "ɑ")
# ɔ
mots <- str_replace_all(mots, pattern = "o(?=m)", replacement = "ɔ")
# Y (u -> Y)
mots <- str_replace_all(mots, pattern = "ù|u", replacement = "Y")

## Gestion des marques complexe
# U
mots <- str_replace_all(mots, pattern = "oYts$|oYts(?= )|oYt$|oYt(?= )|oYs$|oYs(?= )|oYps$|oYp$|oYps(?= )|oYp(?= )|oY", replacement = "U")
# OE
mots <- str_replace_all(mots, pattern = "oeY|eY(?=[rl])|œY|oe", replacement = "OE")
# ø
mots <- str_replace_all(mots, pattern = "eYx|eY", replacement = "ø")
# WaJ
mots <- str_replace_all(mots, pattern = "oy", replacement = "WaJ")
# Wa
mots <- str_replace_all(mots, pattern = "oix$|oix(?= )|oigts$|oigts(?= )|oigt$|oigt(?= )|oi|Ua", replacement = "Wa")
# ɥi
mots <- str_replace_all(mots, pattern = "Yi", replacement = "ɥi")
## Gestion des voyelles nasales + N (intervocalique)
# N
mots <- str_replace_all(mots, pattern = "(?<=[AEIOYUaɑeioɔø])n(?=[AEIOYUaɑeioɔø])|nn", replacement = "N")
# Wɛ̃
mots <- str_replace_all(mots, pattern = "Wans|Wang|Wan", replacement = "Wɛ̃")
# Jɛ̃
mots <- str_replace_all(mots, pattern = "iens$|iens(?= )|ien", replacement = "Jɛ̃")
# Jɛ
mots <- str_replace_all(mots, pattern = "ie(?=N)", replacement = "JAI")
# ɛ̃
mots <- str_replace_all(mots, pattern = "AIn(?!es)|AIn(?!e)|ins$|ins(?= )|eins$|eins(?= )|ein|in|im(?=[bmp])|ym(?=[bmp])", replacement = "ɛ̃")
# œ̃
mots <- str_replace_all(mots, pattern = "Yn(?!es)|Yn(?!e)", replacement = "œ̃")
# ɔ̃
mots <- str_replace_all(mots, pattern = "onts$|onts(?= )|ont$|ont(?= )|ons$|ons(?= )|ong$|ong(?= )|on(?![nh])|ɔm(?![me])", replacement = "ɔ̃")
# T
mots <- str_replace_all(mots, pattern = "t(?=[AEIOYUWaɑeioɔø])", replacement = "T")
# z
mots <- str_replace_all(mots, pattern = "(?<=[AEIOYUWaɑeioɔø])s(?=ent)", replacement = "z")
# mɑ̃
mots <- str_replace_all(mots, pattern = "ment$|ment(?= )", replacement = "mɑ̃")
# dɑ̃
mots <- str_replace_all(mots, pattern = "dent$|dent(?= )", replacement = "dɑ̃")
# pɑ̃
mots <- str_replace_all(mots, pattern = "pent$|pent(?= )", replacement = "pɑ̃")
# apocope ent
mots <- str_replace_all(mots, pattern = "ent$|ent(?= )", replacement = "")
# ɑ̃
mots <- str_replace_all(mots, pattern = "ands$|ands(?= )|and$|and(?= )|ants$|ants(?= )|ant$|ant(?= )|ang$|ang(?= )|ans$|ans(?= )|amps$|amps(?= )|amp$|amp(?= )|am(?=[bmp])|em(?=[bmp])|an(?!n)|ents$|ents(?= )|en", replacement = "ɑ̃")
#### CONSONNES ####
# ɲ
mots <- str_replace_all(mots, pattern = "gn", replacement = "ɲ") # ? gnes$|gnes(?= )|gne$|gne(?= )|
# G
mots <- str_replace_all(mots, pattern = "g(?=[aorɔ̃ɔUml])|gY|gɥ|gg", replacement = "G")
# ʒ
mots <- str_replace_all(mots, pattern = "g|j", replacement = "ʒ")
# ʃ
mots <- str_replace_all(mots, pattern = "ch", replacement = "ʃ")
# il (ill apres v)
mots <- str_replace_all(mots, pattern = "(?<=v)ill|ils$|ils(?= )", replacement = "il")
# AI
mots <- str_replace_all(mots, pattern = "(?<=^[lmsTDtd])es$|(?<= [lmsTDtd])es(?= )|et(?=T)", replacement = "AI")
# ɔ
mots <- str_replace_all(mots, pattern = "ot(?=T)", replacement = "ɔ")
# SW
mots <- str_replace_all(mots, pattern = "(?<=^[klmsTDʒn])e$|(?<= [klmsTDʒn])e(?= )|(?<=^[klmsTDʒn])e(?= )|(?<= [klmsTDʒn])e$", replacement = "SW")
# ph devient f\
mots <- str_replace_all(mots, "ph", "f")
# quelques apocopes
mots <- str_replace_all(mots, pattern = "h|es$|es(?= )|ts$|ds$|t$|d$|ts(?= )|ds(?= )|t(?= )|d(?= )", replacement = "")
# AI (Transfer de e vers AI devant [lrsx])
mots <- str_replace_all(mots, pattern = "e(?=[rsx])|(?<!v)e(?=l)|(?<=s)e(?=p)|(?<=[ʒv])e(?=k)", replacement = "AI")
# AIJ
mots <- str_replace_all(mots, pattern = "eill|eils|eil", replacement = "AIJ")
# øil
mots <- str_replace_all(mots, pattern = "(?<=ø)ill|(?<=a)il$|(?<=a)il(?= )", replacement = "J")
# iJ
mots <- str_replace_all(mots, pattern = "ill", replacement = "iJ")
# sJ
mots <- str_replace_all(mots, pattern = "Ti(?=ɔ̃$)|Ti(?=ɔ̃ )|Ti(?=ɔ)|Ti(?=ɔ)", replacement = "sJ")
# J
mots <- str_replace_all(mots, pattern = "(?<=OE)il|i(?=[ɔ̃ɑ̃AEøo])", replacement = "J")
mots <- str_replace_all(mots, pattern = "(?<=[Ua])iJ", replacement = "J")
# Wi
mots <- str_replace_all(mots, pattern = "(?<!G)Ui", replacement = "Wi")
# d'autres apocopes
mots <- str_replace_all(mots, pattern = "e$|e(?= )|(?<=[rl])s$|(?<=[rl])s(?= )|(?<=wa)x", replacement = "")
# z
mots <- str_replace_all(mots, pattern = "(?<=[AEIOYUWaɑeioɔø])s(?=[AEIOYUWaɑeioɔø])", replacement = "z")
# s
mots <- str_replace_all(mots, pattern = "ss|c(?=[AEeiøɛ̃j])|ç", replacement = "s")
# i
mots <- str_replace_all(mots, pattern = "y", replacement = "i")
# k
mots <- str_replace_all(mots, pattern = "qY|qɥ|ch(?=l)|c(?!h)|q", replacement = "k")
# x devient s
mots <- str_replace_all(mots, pattern = "(?<=Di|si)x$|(?<=Di|si)x(?= )", replacement = "s")
# ks
mots <- str_replace_all(mots, pattern = "x", replacement = "ks")
# consonnes doubles restantes


mots <- str_replace_all(mots, pattern = "pp", replacement = "p")
mots <- str_replace_all(mots, pattern = "mm", replacement = "m")
mots <- str_replace_all(mots, pattern = "ll", replacement = "l")
mots <- str_replace_all(mots, pattern = "rr", replacement = "ʁ")
mots <- str_replace_all(mots, pattern = "r", replacement = "ʁ")
mots <- str_replace_all(mots, pattern = "ff", replacement = "f")
mots <- str_replace_all(mots, pattern = "bb", replacement = "b")
mots <- str_replace_all(mots, pattern = "kk", replacement = "k")
mots <- str_replace_all(mots, pattern = "TT|tt", replacement = "t")
# SW
mots <- str_replace_all(mots, pattern = "e", replacement = "SW")
# epenthese de SW apres k seul
mots <- str_replace_all(mots, pattern = "^k$|(?<= )k(?= )", replacement = "kSW")
mots <- str_replace_all(mots, pattern = "^d$|(?<= )d(?= )", replacement = "dSW")

#### FIN DE LA TRANSCRIPTION ####

#### Transfer des MAJUSCULE en minuscule ####
### MAJ -> symbole
mots <- str_replace_all(mots, pattern = "OE", replacement = "œ")
mots <- str_replace_all(mots, pattern = "SW", replacement = "ə")
mots <- str_replace_all(mots, pattern = "AI", replacement = "ɛ")

### MAJ -> min
mots <- mots |>
  str_to_lower()

mots
