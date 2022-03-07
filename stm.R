# install/load libraries
pacman::p_load(here, readr, stringr, tibble, ggplot2, stm, dplyr, tidytext, grid, gridExtra, tidystm, gg.gap, ggraph, igrpah, visNetwork)

# load data
oped_corp <- read_csv("op_eds_1999_2019.csv")

# structural topic modeling
oped_corp_subset <- oped_corp %>%
  filter(Year != 2020,
         Province != "y.t",
         Province != "n.s",
         Province != "n.b")

total_words_id <- oped_corp_subset %>%
  unnest_tokens(word, Full_Text, to_lower = TRUE) %>%
  filter(!str_detect(word, "^[0-9]+")) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = "word") %>%
  group_by(ID) %>%
  summarize(total= n()) %>%
  ungroup()

total_emotion_words_id <- oped_corp_subset %>% 
  unnest_tokens(word, Full_Text, to_lower = TRUE) %>%
  filter(!str_detect(word, "^[0-9]+")) %>%
  anti_join(stop_words) %>%
  anti_join(custom_stop_words, by = "word") %>%
  inner_join(get_sentiments("nrc"))  %>%
  group_by(ID) %>%
  summarize(emotions= n()) %>%
  ungroup()

emotions_to_total_words_id <- total_words_id %>%
  left_join(total_emotion_words_id, by="ID") %>%
  mutate(percent_emotions=round((emotions/total)*100, 1))

oped_corp_subset <- left_join(oped_corp_subset, emotions_to_total_words_id, by = "ID")

processed <- textProcessor(oped_corp_subset$Full_Text, metadata = oped_corp_subset, customstopwords = 
                             c("canada", "canadian", "canadians"), verbose = FALSE)

out <- prepDocuments(processed$documents, processed$vocab, processed$meta, verbose = FALSE)

docs <- out$documents
vocab <- out$vocab
meta <- out$meta

storage1 <- searchK(docs, vocab, K = c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50),
                    prevalence=~ Jurisdiction + Year + Province, data = meta, set.seed(9999), verbose = FALSE)

options(repr.plot.width = 6, repr.plot.height = 6)

plot(storage1)

storage2 <- tibble(storage1$results)

p1 <- ggplot(storage2, aes(x = as.numeric(K), y = as.numeric(heldout))) +
  geom_point() +
  geom_line() +
  labs(title = "Held-out likelihood",
       caption = "",
       x = "",
       y = "") +
  theme(legend.position = "")

p2 <- ggplot(storage2, aes(x = as.numeric(K), y = as.numeric(lbound))) +
  geom_point() +
  geom_line() +
  labs(title = "Lower bound",
       caption = "",
       x = "",
       y = "") +
  theme(legend.position = "")

p3 <- ggplot(storage2, aes(x = as.numeric(K), y = as.numeric(residual))) +
  geom_point() +
  geom_line() +
  labs(title = "Residuals",
       caption = "",
       x = "",
       y = "") +
  theme(legend.position = "")

p4 <- ggplot(storage2, aes(x = as.numeric(K), y = as.numeric(semcoh))) +
  geom_point() +
  geom_line() +
  labs(title = "Semantic coherence",
       caption = "",
       x = "",
       y = "") +
  theme(legend.position = "")

p5 <- grid.arrange(p1, p2, p3, p4, ncol = 2, bottom = textGrob("Number of topics (K)"))

ggsave(here::here("figures", "topic_selection_searchk_results.pdf"), p5, width = 9, height = 8)

# compute models
model5 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 5, data = out$meta, init.type = "Spectral", verbose = FALSE)
model10 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 10, data = out$meta, init.type = "Spectral", verbose = FALSE)
model15 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 15, data = out$meta, init.type = "Spectral", verbose = FALSE)
model16 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 16, data = out$meta, init.type = "Spectral", verbose = FALSE)
model17 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 17, data = out$meta, init.type = "Spectral", verbose = FALSE)
model18 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 18, data = out$meta, init.type = "Spectral", verbose = FALSE)
model19 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 19, data = out$meta, init.type = "Spectral", verbose = FALSE)
model20 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 20, data = out$meta, init.type = "Spectral", verbose = FALSE)
model21 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 21, data = out$meta, init.type = "Spectral", verbose = FALSE)
model22 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 22, data = out$meta, init.type = "Spectral", verbose = FALSE)
model23 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 23, data = out$meta, init.type = "Spectral", verbose = FALSE)
model24 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 24, data = out$meta, init.type = "Spectral", verbose = FALSE)
model25 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 25, data = out$meta, init.type = "Spectral", verbose = FALSE)
model30 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 30, data = out$meta, init.type = "Spectral", verbose = FALSE)
model35 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 35, data = out$meta, init.type = "Spectral", verbose = FALSE)
model40 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 40, data = out$meta, init.type = "Spectral", verbose = FALSE)
model45 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 45, data = out$meta, init.type = "Spectral", verbose = FALSE)
model50 <- stm(documents = out$documents, vocab = out$vocab, prevalence=~ Jurisdiction + Year + percent_emotions,
               K = 50, data = out$meta, init.type = "Spectral", verbose = FALSE)

# plot exclusivity by semantic coherence for select models
M15ExSem<-as.data.frame(cbind(c(1:15),exclusivity(model15), 
                              semanticCoherence(model=model15, docs), "Mod15"))
M20ExSem<-as.data.frame(cbind(c(1:20),exclusivity(model20), 
                              semanticCoherence(model=model20, docs), "Mod20"))
M25ExSem<-as.data.frame(cbind(c(1:25),exclusivity(model25), 
                              semanticCoherence(model=model25, docs), "Mod25"))
M30ExSem<-as.data.frame(cbind(c(1:30),exclusivity(model30), 
                              semanticCoherence(model=model30, docs), "Mod30"))

ModsExSem<-rbind(M15ExSem, M20ExSem, M25ExSem, M30ExSem)

colnames(ModsExSem)<-c("K", "Exclusivity", "SemanticCoherence", "Model")
colnames(M15ExSem)<-c("K", "Exclusivity", "SemanticCoherence", "Model")
colnames(M20ExSem)<-c("K", "Exclusivity", "SemanticCoherence", "Model")
colnames(M25ExSem)<-c("K", "Exclusivity", "SemanticCoherence", "Model")
colnames(M30ExSem)<-c("K", "Exclusivity", "SemanticCoherence", "Model")

ModsExSem$Exclusivity<-as.numeric(as.character(ModsExSem$Exclusivity))
ModsExSem$SemanticCoherence<-as.numeric(as.character(ModsExSem$SemanticCoherence))

M15ExSem$Exclusivity<-as.numeric(as.character(M15ExSem$Exclusivity))
M15ExSem$SemanticCoherence<-as.numeric(as.character(M15ExSem$SemanticCoherence))

M20ExSem$Exclusivity<-as.numeric(as.character(M20ExSem$Exclusivity))
M20ExSem$SemanticCoherence<-as.numeric(as.character(M20ExSem$SemanticCoherence))

M25ExSem$Exclusivity<-as.numeric(as.character(M25ExSem$Exclusivity))
M25ExSem$SemanticCoherence<-as.numeric(as.character(M25ExSem$SemanticCoherence))

M30ExSem$Exclusivity<-as.numeric(as.character(M30ExSem$Exclusivity))
M30ExSem$SemanticCoherence<-as.numeric(as.character(M30ExSem$SemanticCoherence))

ggplot(ModsExSem, aes(SemanticCoherence, Exclusivity, color = Model))+
  geom_point(size = 2, alpha = 0.7) + 
  labs(x = "Semantic coherence",
       y = "Exclusivity",
       title = "Comparing exclusivity and semantic coherence") +
  theme_minimal()

p1 <- ggplot(M15ExSem, aes(SemanticCoherence, Exclusivity))+
  geom_point(size = 2, alpha = 0.7) + 
  geom_text_repel(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "",
       y = "",
       title = "15 topics") +
  xlim(c(-125, -25)) +
  ylim(c(8.7, 9.9))

p2 <- ggplot(M20ExSem, aes(SemanticCoherence, Exclusivity))+
  geom_point(size = 2, alpha = 0.7) + 
  geom_text_repel(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "",
       y = "",
       title = "20 topics") +
  xlim(c(-125, -25)) +
  ylim(c(8.7, 9.9))

p3 <- ggplot(M25ExSem, aes(SemanticCoherence, Exclusivity))+
  geom_point(size = 2, alpha = 0.7) + 
  geom_text_repel(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "",
       y = "",
       title = "25 topics") +
  xlim(c(-125, -25)) +
  ylim(c(8.7, 9.9))

p4 <- ggplot(M30ExSem, aes(SemanticCoherence, Exclusivity))+
  geom_point(size = 2, alpha = 0.7) + 
  geom_text_repel(aes(label=K), nudge_x=.05, nudge_y=.05)+
  labs(x = "",
       y = "",
       title = "30 topics") +
  xlim(c(-125, -25)) +
  ylim(c(8.7, 9.9))

p5 <- grid.arrange(p1, p2, p3, p4, ncol = 2, bottom = textGrob("Semantic coherence"), left = textGrob("Exclusivity", rot = 90))

ggsave(here::here("figures", "topic_selection_semcoh_exclus.pdf"), p5, width = 9, height = 8)

# more model outputs (for K-score = 20)
plot.STM(model20, "hist")

plot.STM(model20, "summary", n=10)

topicNames <- c("Anti-Terrorism Act", "Topic 2", "Energy", "Electorial Politics", "Education", "Policing", "Healthcare",
                "Topic 8", "Topic 9", "Topic 10", "Topic 11", "Gun Policy", "Access Reform", "Judicial System",
                "Journalism", "Topic 16", "War", "Security Intelligence", "Topic 19", "Indigenous Governance")

par(bty="n",col="grey40",lwd=5)

plot.STM(model15, "summary", custom.labels = "", topic.names = topicNames)

labelTopics(model20, topics = c(1:25), n=15)

plot.STM(model20, "labels", topics = c(1, 2, 3, 4, 5), label = "frex", n = 10, width = 55)

plot.STM(model20, "perspectives", topics = c(13, 11))

mod.out.corr <- topicCorr(model20, method = "huge")

plot(mod.out.corr)

thoughts1 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=1, n=3)$docs[[1]]
thoughts2 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=2, n=3)$docs[[1]]
thoughts3 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=3, n=3)$docs[[1]]
thoughts4 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=4, n=3)$docs[[1]]
thoughts5 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=5, n=3)$docs[[1]]
thoughts6 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=6, n=3)$docs[[1]]
thoughts7 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=7, n=3)$docs[[1]]
thoughts8 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=8, n=3)$docs[[1]]
thoughts9 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=9, n=3)$docs[[1]]
thoughts10 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=10, n=3)$docs[[1]]
thoughts11 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=11, n=3)$docs[[1]]
thoughts12 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=12, n=3)$docs[[1]]
thoughts13 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=13, n=3)$docs[[1]]
thoughts14 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=14, n=10)$docs[[1]]
thoughts15 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=15, n=3)$docs[[1]]
thoughts16 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=16, n=3)$docs[[1]]
thoughts17 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=17, n=3)$docs[[1]]
thoughts18 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=18, n=10)$docs[[1]]
thoughts19 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=19, n=3)$docs[[1]]
thoughts20 <- findThoughts(model20,texts=oped_corp_subset$Full_Text, topics=20, n=3)$docs[[1]]

plotQuote(thoughts1, width=50, maxwidth=500, text.cex=0.75, main="Topic 1")
plotQuote(thoughts2, width=50, maxwidth=500, text.cex=1.25, main="Topic 2")
plotQuote(thoughts3, width=50, maxwidth=500, text.cex=1.25, main="Topic 3")
plotQuote(thoughts4, width=50, maxwidth=500, text.cex=1.25, main="Topic 4")
plotQuote(thoughts5, width=50, maxwidth=500, text.cex=1.25, main="Topic 5")
plotQuote(thoughts6, width=50, maxwidth=500, text.cex=1.25, main="Topic 6")
plotQuote(thoughts7, width=50, maxwidth=500, text.cex=1.25, main="Topic 7")
plotQuote(thoughts8, width=50, maxwidth=500, text.cex=1.25, main="Topic 8")
plotQuote(thoughts9, width=50, maxwidth=500, text.cex=1.25, main="Topic 9")
plotQuote(thoughts10, width=50, maxwidth=500, text.cex=1.25, main="Topic 10")
plotQuote(thoughts11, width=50, maxwidth=500, text.cex=1.25, main="Topic 11")
plotQuote(thoughts12, width=50, maxwidth=500, text.cex=1.25, main="Topic 12")
plotQuote(thoughts13, width=50, maxwidth=500, text.cex=1.25, main="Topic 13")
plotQuote(thoughts14, width=150, maxwidth=500, text.cex=1, main="Topic 14")
plotQuote(thoughts15, width=50, maxwidth=500, text.cex=1.25, main="Topic 15")
plotQuote(thoughts16, width=50, maxwidth=500, text.cex=1.25, main="Topic 16")
plotQuote(thoughts17, width=50, maxwidth=500, text.cex=1.25, main="Topic 17")
plotQuote(thoughts18, width=150, maxwidth=500, text.cex=1, main="Topic 18")
plotQuote(thoughts19, width=50, maxwidth=500, text.cex=1.25, main="Topic 19")
plotQuote(thoughts20, width=50, maxwidth=500, text.cex=1.25, main="Topic 20")

# regress topic prevalence in K=20 model against co-variates
out$meta$Province<-as.factor(out$meta$Province)

prep <- estimateEffect(1:20 ~ Jurisdiction + s(Year), model20, metadata=out$meta, uncertainty="Global") #nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep, topics=c(1:20), nsim=1000)# summary of regression on topic 1-3

prep2 <- estimateEffect(1:20 ~ Jurisdiction + Year + percent_emotions, model20, metadata=out$meta, uncertainty="Global") #nsim is defaulted to 25, but on a small model a higher number lead to more consistent results
summary(prep2, topics=c(1:20), nsim=1000)# summary of regression on topic 1-3

mod.out <- stm(docs, vocab, 20, prevalence=~Jurisdiction + s(Year) + percent_emotions, data=meta)
checkResiduals(mod.out, docs)

options(repr.plot.width=50, repr.plot.height=50, repr.plot.res=100)
plot.estimateEffect(prep2, model=model20, covariate="percent_emotions", topics=c(1), 
                    method="pointestimate", 
                    xlim=c(-.5,1))

# convert model outputs to 'tidy' format for plotting
td_beta <- tidy(model20, matrix = "beta")
td_theta <- tidy(model20, matrix = "theta")
td_gamma <- tidy(model20, matrix = "gamma")
tidyprep <- extract.estimateEffect(prep, "Year", model = model20, method = "pointestimate")
tidyprep2 <- extract.estimateEffect(prep2, "Jurisdiction", model = model20, method = "pointestimate")

# top 10 terms
top_terms <- td_beta %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

# plot topics by top 10 terms
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free", ncol = 3) +
  coord_flip() +
  scale_x_reordered() 

# plot one topic by most probable terms
td_beta %>%
  mutate(topic = paste0("Topic ", topic),
         term = reorder_within(term, beta, topic)) %>%
  filter(topic=="Topic 1") %>% #beta values for topic 1
  filter(beta > 0.003) %>% #only plot word probabilities higher than 0.003 for topic 1
  ggplot(aes(term, beta, fill = as.factor(topic))) +
  geom_bar(alpha = 0.8, show.legend = FALSE, stat = "identity") +
  coord_flip() +
  labs(x = "Terms", y = expression(beta),
       title = "Word probabilities for Topic 1")

# plot 20 most probable topics in corpus by mean gamma
gamma_terms <- td_gamma %>%
  group_by(topic) %>%
  summarise(gamma = mean(gamma)) %>%
  arrange(desc(gamma)) %>%
  mutate(topic = paste0("Topic ", topic),
         topic = reorder(topic, gamma)) %>%
  mutate(topic_title = as.character(topic))

# label topics
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 1"] <- "Information, Law & Governance"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 2"] <- "Online Data & Internet Technology"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 3"] <- "Environment, Agriculture & Development"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 4"] <- "Federal Politics & the Harper Conservatives"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 5"] <- "Diversity, Education, & Citizenship"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 6"] <- "Public Police & Accountability"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 7"] <- "Health Care & Information Governance"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 8"] <- "Municipal Politics"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 9"] <- "Foreign Affairs & Trade"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 10"] <- "Federal Politics & Liberal Scandals"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 11"] <- "Provincial Information Commisioners & Compliance"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 12"] <- "Gun Control & Database Politics"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 13"] <- "Federal Information Commissioners & Compliance"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 14"] <- "Law & Justice Procedures"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 15"] <- "Journalism & News Production"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 16"] <- "Intergovernmental Relations"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 17"] <- "Military & Defence"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 18"] <- "Security & Corrections"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 19"] <- "Fiscal Policy"
gamma_terms$topic_title[gamma_terms$topic_title == "Topic 20"] <- "Indigenous Politics"

tidyprep$label <- gsub("\\(.*", "", tidyprep$label)
tidyprep$label[tidyprep$label == "Topic 1"] <- "Information, Law & Governance"
tidyprep$label[tidyprep$label == "Topic 2"] <- "Online Data & Internet Technology"
tidyprep$label[tidyprep$label == "Topic 3"] <- "Environment, Agriculture &\nDevelopment"
tidyprep$label[tidyprep$label == "Topic 4"] <- "Federal Politics & the Harper \nConservatives"
tidyprep$label[tidyprep$label == "Topic 5"] <- "Diversity, Education, & \nCitizenship"
tidyprep$label[tidyprep$label == "Topic 6"] <- "Public Police & Accountability"
tidyprep$label[tidyprep$label == "Topic 7"] <- "Health Care & Information\nGovernance"
tidyprep$label[tidyprep$label == "Topic 8"] <- "Municipal Politics"
tidyprep$label[tidyprep$label == "Topic 9"] <- "Foreign Affairs & Trade"
tidyprep$label[tidyprep$label == "Topic 10"] <- "Federal Politics & Liberal Scandals"
tidyprep$label[tidyprep$label == "Topic 11"] <- "Provincial Information Commisioners\n& Compliance"
tidyprep$label[tidyprep$label == "Topic 12"] <- "Gun Control & Database Politics"
tidyprep$label[tidyprep$label == "Topic 13"] <- "Federal Information Commissioners\n& Compliance"
tidyprep$label[tidyprep$label == "Topic 14"] <- "Law & Justice Procedures"
tidyprep$label[tidyprep$label == "Topic 15"] <- "Journalism & News Production"
tidyprep$label[tidyprep$label == "Topic 16"] <- "Intergovernmental Relations"
tidyprep$label[tidyprep$label == "Topic 17"] <- "Military & Defence"
tidyprep$label[tidyprep$label == "Topic 18"] <- "Security & Corrections"
tidyprep$label[tidyprep$label == "Topic 19"] <- "Fiscal Policy"
tidyprep$label[tidyprep$label == "Topic 20"] <- "Indigenous Politics"

corr_network_labels <- tidyprep %>%
  select(label) %>%
  distinct(label) %>%
  mutate(label = case_when(
    label == "Information, Law & Governance" ~ "Info, Law &\nGovernance",
    label == "Online Data & Internet Technology" ~ "Online Data &\nInternet\nTechnology",
    label == "Environment, Agriculture &\nDevelopment" ~ "Environment,\nAgriculture &\nDevelopment",
    label == "Federal Politics & the Harper \nConservatives" ~ "Fed Politics &\nthe Harper\nConservatives",
    label == "Diversity, Education, & \nCitizenship" ~ "Diversity,\nEducation, &\nCitizenship",
    label == "Public Police & Accountability" ~ "Public Police &\nAccountability",
    label == "Health Care & Information\nGovernance" ~ "Health Care &\nInfo Governance",
    label == "Municipal Politics" ~ "Municipal\nPolitics",
    label == "Foreign Affairs & Trade" ~ "Foreign\nAffairs &\nTrade",
    label == "Federal Politics & Liberal Scandals" ~ "Fed Politics &\nLiberal Scandals",
    label == "Provincial Information Commisioners\n& Compliance" ~ "Prov Info\nCommisioners\n& Compliance",
    label == "Gun Control & Database Politics" ~ "Gun Control &\nDatabase Politics",
    label == "Federal Information Commissioners\n& Compliance" ~ "Fed. Info\nCommissioners &\nCompliance",
    label == "Law & Justice Procedures" ~ "Law & Justice\nProcedures",
    label == "Journalism & News Production" ~ "Journalism &\nNews\nProduction",
    label == "Intergovernmental Relations" ~ "Intergov.\nRelations",
    label == "Military & Defence" ~ "Military &\nDefence",
    label == "Security & Corrections" ~ "Security &\nCorrections",
    label == "Fiscal Policy" ~ "Fiscal\nPolicy",
    label == "Indigenous Politics" ~ "Indigenous\nPolitics"
  ))

tidyprep2$label <- gsub("\\(.*", "", tidyprep2$label)
tidyprep2$label[tidyprep2$label == "Topic 1"] <- "Information, Law & Governance"
tidyprep2$label[tidyprep2$label == "Topic 2"] <- "Online Data & Internet Technology"
tidyprep2$label[tidyprep2$label == "Topic 3"] <- "Environment, Agriculture &\nDevelopment"
tidyprep2$label[tidyprep2$label == "Topic 4"] <- "Federal Politics & the Harper \nConservatives"
tidyprep2$label[tidyprep2$label == "Topic 5"] <- "Diversity, Education, & \nCitizenship"
tidyprep2$label[tidyprep2$label == "Topic 6"] <- "Public Police & Accountability"
tidyprep2$label[tidyprep2$label == "Topic 7"] <- "Health Care & Information\nGovernance"
tidyprep2$label[tidyprep2$label == "Topic 8"] <- "Municipal Politics"
tidyprep2$label[tidyprep2$label == "Topic 9"] <- "Foreign Affairs & Trade"
tidyprep2$label[tidyprep2$label == "Topic 10"] <- "Federal Politics & Liberal Scandals"
tidyprep2$label[tidyprep2$label == "Topic 11"] <- "Provincial Information Commisioners\n& Compliance"
tidyprep2$label[tidyprep2$label == "Topic 12"] <- "Gun Control & Database Politics"
tidyprep2$label[tidyprep2$label == "Topic 13"] <- "Federal Information Commissioners\n& Compliance"
tidyprep2$label[tidyprep2$label == "Topic 14"] <- "Law & Justice Procedures"
tidyprep2$label[tidyprep2$label == "Topic 15"] <- "Journalism & News Production"
tidyprep2$label[tidyprep2$label == "Topic 16"] <- "Intergovernmental Relations"
tidyprep2$label[tidyprep2$label == "Topic 17"] <- "Military & Defence"
tidyprep2$label[tidyprep2$label == "Topic 18"] <- "Security & Corrections"
tidyprep2$label[tidyprep2$label == "Topic 19"] <- "Fiscal Policy"
tidyprep2$label[tidyprep2$label == "Topic 20"] <- "Indigenous Politics"

gamma_terms %>%
  top_n(20, gamma) %>%
  ggplot(aes(topic, gamma, label = topic_title)) +
  geom_col(show.legend = FALSE, width = 0.1) +
  geom_text(hjust = 0, nudge_y = 0.0005, size = 3.5, alpha = 1, color = "black") +
  coord_flip() +
  scale_y_continuous(expand = c(0,0),
                     limits = c(0, .22),
                     labels = percent_format()) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  labs(x = NULL, y = expression(gamma),
       title = "Topics by prevalence") +
  theme(plot.title = element_text(size = 15, face = "bold"))

ggsave(here::here("figures", "top_topics_across_corpus.pdf"), width = 9, height = 6)

tidyprep %>%
  arrange(label) %>%
  ggplot(aes(x = covariate.value, y = estimate)) +
  geom_line(color = "black") +
  geom_ribbon(aes(ymin = ci.lower, ymax = ci.upper), linetype = 2, alpha = 0.1) +
  facet_wrap(~label, ncol = 4, scales = "free_y") +
  expand_limits(y = 0) +
  labs(y = "Expected Topic Proportion",
       x= "",
       title = "Topic prevalence over time, 1999-2019") +
  theme_minimal() +
  theme(plot.margin = unit(c(0.3, 0.6, 0.3, 0.3), "cm")) +
  theme(plot.title = element_text(size = 18, face = "bold"))

ggsave(here::here("figures", "topics_over_time.pdf"), width = 12, height = 15)

stm_corrs <- stminsights::get_network(model = model20,
                                      method = 'simple',
                                      labels = paste(corr_network_labels$label),
                                      cutoff = 0.001,
                                      cutiso = FALSE)

stm_corrs %>%
  activate(nodes) %>%
  mutate(community = as.factor(group_fast_greedy())) %>% 
  ggraph(layout = "stress") +
  geom_edge_link(aes(edge_width = weight, label = weight), label_colour = "black", label_alpha = 1, edge_colour = 'grey', alpha = 0.75, show.legend = FALSE) +
  geom_node_point(aes(size = props, colour = community), alpha =.3, show.legend = FALSE)  +
  geom_node_text(aes(label = name), repel = FALSE, size = 8, alpha = 1, fontface = "italic", check_overlap = TRUE, show.legend = FALSE) +
  scale_size(range = c(5, 70), labels = scales::percent) +
  scale_edge_width(range = c(1, 10)) +
  theme_graph(base_family="sans") +
  theme(plot.title = element_text(size = 50), plot.subtitle = element_text(size = 35)) +
  labs(title = "Topic correlation network")

ggsave(here::here("figures", "topics_correlation_network.pdf"), width = 35, height = 20)

tidyprep2 %>%
  ggplot(aes(x = covariate.value, y = estimate, ymin = ci.lower, ymax = ci.upper)) +
  geom_hline(yintercept = 0, colour = grey(1/2), lty = 2) +
  geom_point() +
  geom_linerange() +
  facet_wrap(~label, ncol = 4) +
  coord_flip() +
  theme_minimal() +
  labs(title = "Estimated effect of newspaper jurisdiction on topic prevalence", x = "", y = "Regression coefficient") +
  theme(plot.title = element_text(size = 15, face = "bold"))

ggsave(here::here("figures", "jurisdiction_coefficients.pdf"), width =12, height = 15)




