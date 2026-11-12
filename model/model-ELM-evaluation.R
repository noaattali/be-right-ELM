# Assessing how different projection patterns can result from the meaning 
# differences between know and be right.
# Exploring predictions of a computational cognitive model of the projection of 
# BEL and CC of be right from negated declaratives
# Builds on the reasoning-based analysis of the projection of BEL and CC of know from negated declaratives in Scontras & Tonhauser 2025, 2026. 

# load required libraries
library(jsonlite)
library(tidyverse)
library(rwebppl)
library(ggh4x) # nested facets
library(scales) # in ggplot

# set working directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
theme_set(theme_bw())

source('helpers.R')

# read in the  model
model <- read_file("model_ELM.wppl")
model

model_id <- "model_ELM"

# evaluate the model
eval_webppl <- function(command) {
  webppl(paste(model,command,sep="\n"))
}

# define the utterances
utterances = c("pos-say-dinner",
               "neg-say-dinner",
               "pos-beright-dinner",
               "neg-beright-dinner",
               "pos-bare-dinner",
               "neg-bare-dinner",
               "pos-think-dinner",
               "neg-think-dinner",
               "pos-know-dinner",
               "neg-know-dinner")
utterances

# define the qudBias (from context)
qudBias = c("BEL?", "CC?","COMMIT?")
qudBias

#define the quds
quds = c("belQUD","ccQUD","commitQUD")
quds

# define the world states
states = c("{BEL: 1, CC: 1, COMMIT: 1}",
           "{BEL: 1, CC: 1, COMMIT: 0}",
           "{BEL: 1, CC: 0, COMMIT: 1}",
           "{BEL: 1, CC: 0, COMMIT: 0}",
           "{BEL: 0, CC: 1, COMMIT: 1}",
           "{BEL: 0, CC: 1, COMMIT: 0}",
           "{BEL: 0, CC: 0, COMMIT: 1}",
           "{BEL: 0, CC: 0, COMMIT: 0}")

states

# define the plausible private assumptions
cgs = c("[{BEL: 1, CC: 1, COMMIT: 1}, {BEL: 1, CC: 1, COMMIT: 0}, {BEL: 1, CC: 0, COMMIT: 1}, {BEL: 1, CC: 0, COMMIT: 0}, {BEL: 0, CC: 1, COMMIT: 1}, {BEL: 0, CC: 1, COMMIT: 0}, {BEL: 0, CC: 0, COMMIT: 1}, {BEL: 0, CC: 0, COMMIT: 0}]",
        "[{BEL: 1, CC: 1, COMMIT: 1}, {BEL: 0, CC: 1, COMMIT: 1}, {BEL: 1, CC: 1, COMMIT: 0}, {BEL: 0, CC: 1, COMMIT: 0}]",
        "[{BEL: 1, CC: 0, COMMIT: 1}, {BEL: 0, CC: 0, COMMIT: 1}, {BEL: 1, CC: 0, COMMIT: 0}, {BEL: 0, CC: 0, COMMIT: 0}]",
        "[{BEL: 1, CC: 1, COMMIT: 1}, {BEL: 1, CC: 0, COMMIT: 1}, {BEL: 1, CC: 1, COMMIT: 0}, {BEL: 1, CC: 0, COMMIT: 0}]",
        "[{BEL: 0, CC: 1, COMMIT: 1}, {BEL: 0, CC: 0, COMMIT: 1}, {BEL: 0, CC: 1, COMMIT: 0}, {BEL: 0, CC: 0, COMMIT: 0}]",
        "[{BEL: 1, CC: 1, COMMIT: 1}, {BEL: 0, CC: 1, COMMIT: 1}, {BEL: 1, CC: 0, COMMIT: 1}, {BEL: 0, CC: 0, COMMIT: 1}]",
        "[{BEL: 1, CC: 1, COMMIT: 0}, {BEL: 0, CC: 1, COMMIT: 0}, {BEL: 1, CC: 0, COMMIT: 0}, {BEL: 0, CC: 0, COMMIT: 0}]"
)
cgs


# run the speaker ----

# input to speaker: state, cg, qud
# we only input those state/cg combinations that are compatible with one another

# create data frame of all possible state/cg combinations
tmp = data.frame(state = character(), cg = character())
tmp

#produce all combinations of s and cg
for (s in states) {
  for (cg in cgs) {
    tmp = tmp %>% 
      add_row(state = s, cg = cg)
  }
}
tmp

# filter out those rows where the state is not in the cg
stateCG = tmp %>%
  filter(mapply(grepl, tmp$state, tmp$cg, fixed=TRUE))
#view(stateCG)
stateCG # these are the 12 combinations shown in the tables

nrow(stateCG)

#### call the speaker ----

S = data.frame(state = character(), commonGround = character(), qud = character(), utterance = character(), prob = numeric())
S

for (n in 1:nrow(stateCG)) {
  print(stateCG$state[[n]])
  print(stateCG$cg[[n]])
  for (q in quds) {
    S_tmp = eval_webppl(paste("speaker(",stateCG$state[[n]],",",stateCG$cg[[n]],",'",q,"')",sep=""))
    for (i in 1:nrow(S_tmp)) {
      S = S %>% 
        add_row(state = stateCG$state[[n]], commonGround = stateCG$cg[[n]], qud = q, utterance = S_tmp$support[i], prob = S_tmp$prob[i])
    }
  }
}
S

S <- S %>%
  mutate(model = model_id)

# save raw speaker data
write_csv(S, file = paste0("data/S_raw_", model_id, ".csv"))

#### clean up the speaker data ----

# read raw speaker
S = read_csv(file = paste0("data/S_raw_", model_id, ".csv"))

# create a more compact state column for plotting
table(S$state)
S$state = gsub("\\{","",S$state)
S$state = gsub("\\}","",S$state)
S$state = gsub(" ", "",S$state)
table(S$state)

# code the commonGround column into private assumptions  
S <- S %>% 
  mutate(CC = case_when(!grepl("CC: 0", commonGround) ~ "CC:1",
                        !grepl("CC: 1", commonGround) ~ "CC:0",
                        TRUE ~ "CC:?"))
S <- S %>%
  mutate(BEL = case_when(!grepl("BEL: 0", commonGround) ~ "BEL:1",
                         !grepl("BEL: 1", commonGround) ~ "BEL:0",
                         TRUE ~ "BEL:?"))

S <- S %>%
  mutate(COMMIT = case_when(!grepl("COMMIT: 0", commonGround) ~ "COMMIT:1",
                            !grepl("COMMIT: 1", commonGround) ~ "COMMIT:0",
                            TRUE ~ "COMMIT:?"))
table(S$BEL,S$CC,S$COMMIT)

S$pa = paste(S$BEL,S$CC,S$COMMIT,sep=" ")
table(S$pa)

# recode the utterance names
S = S %>%
  mutate(utterance = recode(utterance, 
                            "neg-bare-dinner" = "negated bare", 
                            "pos-bare-dinner" = "bare",
                            "pos-think-dinner" = "think",
                            "neg-think-dinner" = "negated think",
                            "pos-beright-dinner" = "be right",
                            "neg-beright-dinner" = "negated be right",
                            "pos-know-dinner" = "know",
                            "neg-know-dinner" = "negated know",
                            "pos-say-dinner" = "say",
                            "neg-say-dinner" = "negated say"))

# recode the QUD names
S = S %>%
  mutate(qud = recode(qud, "ccQUD" = "CC? QUD", "belQUD" = "BEL? QUD", "commitQUD" = "COMMIT? QUD"))

# save cleaned up speaker data
write_csv(S, file = paste0("data/S_", model_id, ".csv"))

# run the pragmatic listener ----

# input to PL: utterance, qudBias

#### call PL ----

PL = data.frame(utterance = character(), qudBias = character(), 
                state.CC = numeric(), state.BEL = numeric(), prob = numeric())
PL

for (u in utterances) {
  print(u)
  for (q in qudBias) {
    print(q)
    PL_tmp = eval_webppl(paste("pragmaticListener('",u,"','",q,"')",sep=""))
    for (i in 1:nrow(PL_tmp)) {
      PL = PL %>% 
        add_row(utterance = u, qudBias = q,
                state.CC = PL_tmp$CC[i],
                state.BEL = PL_tmp$BEL[i],
                prob = PL_tmp$prob[i])
    }
  }
}
PL
nrow(PL)

PL %>%
  group_by(utterance, qudBias) %>%
  summarise(total_prob = sum(prob), n_rows = n(), .groups = "drop")

PL <- PL %>%
  mutate(model = model_id)

write_csv(PL, file = paste0("data/PL_", model_id, ".csv"))

# read model data
PL = read_csv(paste0("data/PL_", model_id, ".csv"))
nrow(PL)

# make long format to be able to aggregate state.CC and state.BEL
PL2 = PL %>% pivot_longer(
  cols = state.CC:state.BEL,
  names_to = c("state"),
  values_to = c("trueFalse"))
PL2
nrow(PL2)

write_csv(PL2, file = paste0("data/PL2_", model_id, ".csv"))

# now keep rows where state.BEL=1 or state.CC=1
PL2 = PL2 %>%
  filter(trueFalse == 1) %>%
  filter(utterance == "neg-beright-dinner") %>%
  mutate(utterance = recode(utterance, "neg-beright-dinner" = "neg-beright")) %>%
  droplevels() %>%
  rename("qud" = "qudBias") %>%
  mutate(state = recode(state, "state.BEL" = "BEL", "state.CC" = "CC")) %>%
  mutate(qud = recode(qud, "BEL?" = "BEL? QUD", "CC?" = "CC? QUD", "COMMIT?" = "COMMIT? QUD"))
PL2

# plot 
ggplot(data=PL2, aes(x=state, y=prob)) +
  geom_bar(stat = "identity",width = 0.3, fill="black",color = "black") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  facet_wrap(. ~ qud) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("Inferences") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 

# plot for just BEL
ggplot(data=PL2[PL2$state == "BEL",], aes(x=qud, y=prob)) +
  geom_bar(stat = "identity",width = 0.3, fill="black",color = "black") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("QUD") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 

# plot for just CC
ggplot(data=PL2[PL2$state == "CC",], aes(x=qud, y=prob)) +
  geom_bar(stat = "identity",width = 0.3, fill="black",color = "black") +
  theme(legend.position="top") +
  theme(axis.text.y = element_text(size=10)) +
  #theme(axis.title.x=element_blank()) +
  ylab("Predicted probability") +
  xlab("QUD") +
  scale_y_continuous(limits = c(-.1,1.1),breaks = c(0,0.2,0.4,0.6,0.8,1.0), labels = c("0",".2",".4",".6",".8","1")) 



######### Figure 4: projection predictions for be right under CC? and know under BEL?

PL2_model_ELM <- read_csv("data/PL2_model_ELM.csv")

PL2_plot = PL2_model_ELM %>%
  filter(trueFalse == 1) %>%
  filter(utterance %in% c("neg-know-dinner",
                          "neg-beright-dinner")) %>%
  mutate(utterance = recode(utterance, 
                            "neg-know-dinner" = "negated know",
                            "neg-beright-dinner" = "negated be right")) %>%
  droplevels() %>%
  rename("qud" = "qudBias") %>%
  mutate(state = recode(state, "state.BEL" = "BEL",
                        "state.CC" = "CC")) %>%
  mutate(qud = recode(qud, 
                      "BEL?" = "BEL? QUD", 
                      "CC?" = "CC? QUD", 
                      "COMMIT?" = "COMMIT? QUD")) %>%
  group_by(model, utterance, qud, state) %>%
  summarise(prob = sum(prob), .groups = "drop")

PL2_plot %>%
  count(model, qud, state)

PL2_plot %>%
  dplyr::filter(
    qud != "COMMIT? QUD", 
    (utterance == "negated know" & qud == "BEL? QUD") |
      (utterance == "negated be right" & qud == "CC? QUD")) %>%
  ggplot(aes(x=utterance, y=prob, fill=state)) +
  theme_bw(base_size = 16)+
  geom_col(position = "dodge", width = 0.5, color = "black") +
  # theme(legend.position="top", strip.text = element_blank()) +
  theme(strip.text = element_blank()) +
  # theme(axis.text.y = element_text(size=10)) +
  # Facet by QUD but suppress showing the dropped/empty QUD/Predicate levels:
  facet_wrap(. ~ qud, scales = "free_x") +
  scale_fill_manual(values = c("CC" = "#E69F00",
                               "BEL" = "#CC79A7")) +
  labs(
    x = "utterance",
    y = "predicted projection",
    fill = "inferences"
  ) +
  scale_y_continuous(limits = c(-.1,1.1),
                     breaks = c(0,0.2,0.4,0.6,0.8,1.0),
                     labels = c("0",".2",".4",".6",".8","1")) 

ggsave("graphs/qualitativePredictions-model-simplified-wide.pdf",height=2.5,width=6)



######## Appendix figure A: explore speaker behavior for be right under CC? ######

S <- read_csv("data/S_model_ELM.csv")

# Full readable labels for the different private assumptions, in order of CC, BEL, COMMIT, none
# Two different kinds of labels:
# Version that writes it out in words:
pa_levels <- c(
  "CC is true",
  "CC is false",
  "BEL is true",
  "BEL is false",
  "COMMIT is true",
  "COMMIT is false",
  "no assumptions")

# The most explicit/accurate version:
pa_levels_2 <- c(
  "CC:1-BEL:?-COMMIT:?",
  "CC:0-BEL:?-COMMIT:?",
  "CC:?-BEL:1-COMMIT:?",
  "CC:?-BEL:0-COMMIT:?",
  "CC:?-BEL:?-COMMIT:1",
  "CC:?-BEL:?-COMMIT:0",
  "CC:?-BEL:?-COMMIT:?")

# Organize dataset for the plot
S_neg_beright <- S %>%
  # Focus on speaker choices for negated be right utterances under the CC? QUD
  filter(utterance == "negated be right", qud %in% c("CC? QUD")) %>%
  # We need fuller readable labels for the different private assumptions
  # The most explicit/accurate version
  # mutate(private_assumption = case_when(
  #     CC  == "CC:1"  ~ "CC:1-BEL:?-COMMIT:?",
  #     CC  == "CC:0"  ~ "CC:0-BEL:?-COMMIT:?",
  #     BEL == "BEL:1" ~ "CC:?-BEL:1-COMMIT:?",
  #     BEL == "BEL:0" ~ "CC:?-BEL:0-COMMIT:?",
  #     COMMIT == "COMMIT:1" ~ "CC:?-BEL:?-COMMIT:1",
  #     COMMIT == "COMMIT:0" ~ "CC:?-BEL:?-COMMIT:0",
  #     TRUE           ~ "CC:?-BEL:?-COMMIT:?")
  #     )  %>%
  # Version that writes it out in words
  mutate(private_assumption = case_when(
    CC  == "CC:1"  ~ "CC is true",
    CC  == "CC:0"  ~ "CC is false",
    BEL == "BEL:1" ~ "BEL is true",
    BEL == "BEL:0" ~ "BEL is false",
    COMMIT == "COMMIT:1" ~ "COMMIT is true",
    COMMIT == "COMMIT:0" ~ "COMMIT is false",
    TRUE           ~ "no assumptions")
  )  %>%
  # Order the private assumptions
  mutate(private_assumption = factor(private_assumption, levels = rev(pa_levels))) %>%
  # Order CC/BEL/COMMIT within the description of each world
  mutate(state_BEL = str_extract(state, "(?<=BEL:)\\d"),
         state_CC = str_extract(state, "(?<=CC:)\\d"),
         state_COMMIT = str_extract(state, "(?<=COMMIT:)\\d"),
         observed_world = paste0("COMMIT:", state_COMMIT,",BEL:", state_BEL,",CC:", state_CC)) %>%
  # The speaker code only has state/private-assumption combinations that are compatible
  # Create every combination in order to display the full set of assumptions in each literally-possible world
  select(qud, observed_world, private_assumption, prob) %>%
  complete(qud, observed_world, private_assumption = factor(rev(pa_levels),levels = rev(pa_levels)))


# Plot speaker prob by private assumption per each world state
S_neg_beright %>%
  ggplot(aes(x = prob, y = private_assumption)) +
  geom_col(width = 0.75, fill= "black") +
  facet_grid(rows = vars(observed_world)) +
  scale_x_continuous(labels = label_number()) +
  labs(x = "probability", 
       y = "Private assumptions",
       title = "S1 for negated be right under CC? QUD") +
    theme_bw(base_size = 13) +
    theme(legend.position = "none",
          strip.background = element_rect(fill = "white", color = "black"),
          # Control the vertical gaps between facets:
          panel.spacing.y = unit(0.3, "lines"),
          axis.text.y = element_text(size = 9),
          axis.title.y = element_text(size = 12))

ggsave("graphs/Figure_A_S1_neg_beright_CC_QUD.pdf",height=12,width=6)


######## Appendix figure B: listener behavior for be right under CC? ######

L1_neg_beright <- eval_webppl("pragmaticListenerFull('neg-beright-dinner','CC?')")

head(L1_neg_beright$cg)

classify_assumption <- function(cg) {
  
  # No assumptions: all 8 worlds remain possible
  if (nrow(cg) == 8) {
    return("no assumptions")
  }
  
  # Check whether one dimension is fixed
  if (length(unique(cg$CC)) == 1) {
    return(ifelse(unique(cg$CC) == 1,
                  "CC is true",
                  "CC is false"))
  }
  
  if (length(unique(cg$BEL)) == 1) {
    return(ifelse(unique(cg$BEL) == 1,
                  "BEL is true",
                  "BEL is false"))
  }
  
  if (length(unique(cg$COMMIT)) == 1) {
    return(ifelse(unique(cg$COMMIT) == 1,
                  "COMMIT is true",
                  "COMMIT is false"))
  }
  
  return("unclassified")
}

L1_neg_beright <- L1_neg_beright %>%
  mutate(private_assumption = map_chr(cg, classify_assumption),
         observed_world = paste0("COMMIT:", COMMIT, ",BEL:", BEL, ",CC:", CC),
         private_assumption = factor(private_assumption, levels = rev(pa_levels)))

L1_neg_beright %>%
  ggplot(aes(x = prob, y = private_assumption)) +
  geom_col(width = 0.75, fill= "black") +
  facet_grid(rows = vars(observed_world)) +
  scale_x_continuous(labels = label_number()) +
  labs(x = "probability", 
       y = "Private assumptions",
       title = "L1 for negated be right under CC? QUD") +
  theme_bw(base_size = 13)

ggsave("graphs/Figure_B_L1_neg_beright_CC_QUD.pdf",height=12,width=6)



