library(tidyverse) 
library(mclm)  
library(knitr)
library(kableExtra)
library(ggwordcloud)
library(ggpubr)

#-----------------------
# Read files
#-----------------------
# Read stop words and functional word list to be used in the analysis
stop_list <- read_types("stopwords-es.txt")  %>%
  as_types() 

functional <- read_types("functionalwords-es.txt") %>%
  as_types() 

#colors for plots  
cbp1 <- c("#F8766D", "#009E73", "#56B4E9", "#E69F00",
          "#0072B2", "#A8E442", "#D55E00", "#000000")

#files are stored in the same directory, under the folder named "txt"
# store the names of corpus files in object fnames_BASE
fnames_BASE <- get_fnames("txt") %>%
  keep_re("[.]txt$") 

sub_corp <- fnames_BASE %>%
  re_retrieve_first("txt/([^/]+)", requested_group = 1) 

#-----------------------
# Descriptive analysis
#-----------------------

# stop words and numbers removed from the corpus for cloud plot
flist <- fnames_BASE %>%
  freqlist(file_encoding = "UTF-8")  %>% # as text is in Spanish UTF should be used for special characters
  drop_types(stop_list) %>% 
  drop_re("[[:digit:]]+") %>%  
  data.frame() %>%
  mutate(angle = 90 * sample(c(0, 1), n(), replace = TRUE, prob = c(60, 40))) # random angle assignation for cloud plot

# cloud plot with most used words (>100) by all presidents  
flist %>% filter(abs_freq > 100) %>% 
  ggplot(aes(label = type, size = abs_freq, 
             color = factor(sample.int(10, nrow(flist[flist$abs_freq > 100,]), replace = TRUE)), 
             angle = angle)) +
  geom_text_wordcloud_area(area_corr_power = 1) +
  theme_minimal() %>% 
  #scale_color_manual(values = cbp1) %>% 
  invisible()

# summary table of presidents and their periods  
as_tibble(fnames_BASE) %>% 
  mutate(Year = gsub("*_.*","",gsub(".*/|_*.txt", "", filename)),
         Name = sub("([a-z])([A-Z])", "\\1 \\2", gsub(".*_","",gsub(".*/|_*.txt", "", filename))),
         N = gsub("^txt\\\\*.","", gsub(".*\\\\|\\..*$", "", filename))) %>%
  select(Year, Name, N) %>% 
  arrange(N, Year, Name) %>% 
  group_by(N) %>% 
  summarise(Periodo = paste0(min(Year),"-",max(Year)), Name = max(Name)) %>% 
  knitr::kable(caption = "Name and period of each president") %>% 
  kable_styling(bootstrap_options = c("bordered", "hover"), latex_options = c("repeat_header", "HOLD_position"), position = "left")

# for loop for obtain Freqlist by year
mandat_corp <- vector(mode = 'list', length = length(fnames_BASE)) # create empty list
n_tokens <- NULL # create empty vector
mostused <- NULL # create empty vector
nas <- c(paste0(seq(1990,1990+length(fnames_BASE)-1),"_")) # vector with years names to extract corpus
for (i in 1:length(fnames_BASE)) { # for loop to fill the table with number of tokens/types used each year
  mandat_corp[[i]] <- get_fnames("txt") %>%
    keep_re(nas[i])
  
  n_tokens <- cbind(n_tokens, mandat_corp[[i]] %>% freqlist(file_encoding = "UTF-8") %>% summary() %>% unlist())
  
  mostused <- rbind(mostused, mandat_corp[[i]] %>% freqlist(file_encoding = "UTF-8") %>% 
                      drop_types(stop_list) %>%  drop_types(functional) %>%  drop_re("[[:digit:]]+") %>%  
                      keep_bool(ranks(.) == 1) %>% as.data.frame()) # identify the most used type droping stop words and functional words
}

# Plot of frequency of tokens and types
s <- 8 # font size
reshape2::melt(cbind(names=rownames(n_tokens), as.data.frame(n_tokens)), id = "names")  %>% #reshape frequencies
  left_join(data.frame(cbind(variable=paste0("V",1:31),sub_corp)), by = "variable") %>%  # add subcorp name
  left_join(data.frame(cbind(names="n_types",variable=paste0("V",1:31),mostused)), by = c("names","variable")) %>% #Add most freq type
  filter(names != "tot_n_tokens") %>% 
  ggplot(aes(x = reorder(variable,order(as.numeric(as.character(variable)))), y = value, 
             fill = sub_corp)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = type), angle = 90, hjust = 1.5, size = 3) +
  facet_grid(names ~ ., scales = "free", switch = "y") +
  ggtitle("Number of tokens and types by year") +
  xlab("Year") +
  theme_pubr() +
  scale_fill_manual(values = cbp1) +
  scale_x_discrete(labels = seq(1990,2020)) +
  theme(legend.position = "bottom", 
        axis.text.x = element_text(angle = 90, size = s), 
        axis.text.y = element_text(size = s), 
        legend.key.size = unit(0.4,"cm"), 
        legend.text = element_text(size = s), 
        legend.title = element_blank(), 
        axis.title.x = element_text(size = s), 
        axis.title.y = element_text(size = s))


#-----------------------
# creation of d dataframe, with all attributes form corpus 
#-----------------------
# list of adverbs, time, place, mode, quantity, doubt
tiempo <- c("ahora","antes","después","tarde","luego","ayer","temprano","ya","todavía","anteayer","aún","pronto","hoy")
lugar <- c("aquí","ahí","allí","cerca","lejos","fuera","dentro","alrededor","aparte","encima","debajo","delante","detrás")
modo<- c("así","bien","mal","despacio","deprisa","regular","rapido")
cantidad <- c("mucho","poco","muy","casi","todo","nada","algo","medio","demasiado","bastante","más","menos","además","incluso","también")
duda <- c("acaso","quizá","tal vez","a lo mejor")

##Data frame with extracted information from corpus
d <- data.frame(row.names = c("p_types", "word_len", "p_par",
                              "p_c", "p_dig", "p_bigr",
                              "p_trigr", "p_mode", "p_stopw",
                              "p_funct",  "p_verb", "p_advtim", "p_advlug", "p_advqua", "p_advdou",
                              "p_common", "p_neg", "p_pos"))

for (i in 1:length(fnames_BASE)) {
  
  long_name <- fnames_BASE[[i]]
  period_name <- gsub("*_.*","",gsub(".*/|_*.txt", "", long_name))
  short_name <- sub_corp[[i]]
  
  mandat_corp <- read_fnames(long_name,file_encoding = "UTF-8", trim_fnames = FALSE) #separate paragraphs
  
  
  paragraph <- read_fnames(long_name, sep = "\\n", file_encoding = "UTF-8", trim_fnames = TRUE) #separate paragraphs
  wrdpnc <- read_fnames(long_name, sep = " ", file_encoding = "UTF-8", trim_fnames = TRUE) #separate words with punctuation
  
  toks <- mclm::tokenize(wrdpnc, re_token_splitter = "[[:punct:]]") %>%  as_tokens() #separate words without punctuation
  toksstop <- toks %>% drop_types(stop_list)
  typs <- toks %>% freqlist(file_encoding = "UTF-8") %>%  as_tokens()
  punct <- wrdpnc[re("[[:punct:]]")] %>%  as_tokens() #separate punctuation
  digits <- wrdpnc[re("[[:digit:]]")] %>%  as_tokens() #separate numbers
  bigram <- mclm::tokenize(wrdpnc, re_token_splitter = "[[:punct:]]", ngram_size = 2) %>%  as_tokens() #separate words without punctuation
  trigram <- mclm::tokenize(wrdpnc, re_token_splitter = "[[:punct:]]", ngram_size = 3) %>%  as_tokens() #separate words without punctuation
  
  n_par <- length(paragraph)
  n_c <- length(punct)
  n_tok <- n_tokens(toks) 
  n_typ <- n_tokens(typs)
  n_dig <- n_tokens(digits)
  n_bigr <- bigram %>% freqlist() %>% n_types() 
  n_trigr <- trigram %>% freqlist() %>% n_types() 
  n_stopw <- n_tokens(toks[toks %in% stop_list]) #stopwords used
  n_funct <- n_tokens(toks[toks %in% functional]) # functional words
  n_verb <- n_tokens(toksstop[re("..+(r)$")]) #verbs used
  n_mode <- n_tokens(toks[toks %in% modo])+n_tokens(toks[re("..+(mente)$")]) #modal adverbs
  n_advtim <- n_tokens(toks[toks %in% tiempo]) #time adverbs used
  n_advlug <- n_tokens(toks[toks %in% lugar]) #place adverbs used
  n_advqua <- n_tokens(toks[toks %in% cantidad]) #quantity adverbs used
  n_advdou <- n_tokens(toks[toks %in% duda]) + n_tokens(toks[re("^(proba|posib|puede)")]) #doubt adverbs used
  n_common <- n_tokens(toks[re("..+(emos)$")]) #communal words used
  n_neg <- n_tokens(toks %>% keep_re(paste0("^(",paste(c("ning","nadie","nada","nunca","jamas","tampoco"), 
                                                       collapse = "|"),")..","|^no$|^ni$"))) #negative words used
  n_pos <- n_tokens(toks %>% keep_re(paste0("^(",paste(c("algu", "algú", "siempre","tambien", "efectiva", "verdad", "ciert", "clar"), 
                                                       collapse = "|"),")..","|^si$|^o$"))) #positive words used
  
  # --- fill in column in d ---
  d["p_types", period_name] <- n_typ / n_tok * 100 # ok, because files (about) same size
  d["word_len", period_name] <- sum(nchar(toks)) / n_tokens(toks) 
  d["p_par", period_name] <- (n_tok / n_par) # proportion of tokens by num paragraphs
  d["p_c", period_name] <- n_c / n_tok * 100 #proportion of punctuation marks
  d["p_dig", period_name] <- (n_dig / n_tok) * 1000
  d["p_bigr", period_name] <- n_bigr / n_tok 
  d["p_trigr", period_name] <- n_trigr / n_tok 
  d["p_mode", period_name] <- (n_mode / n_tok)  
  d["p_stopw", period_name] <- n_stopw / n_tok
  d["p_funct", period_name] <- (n_funct / n_tok) 
  d["p_verb", period_name] <- (n_verb / n_tok) * 1000
  d["p_advtim", period_name] <- (n_advtim / n_tok) * 10000
  d["p_advlug", period_name] <- (n_advlug / n_tok) * 10000
  d["p_advqua", period_name] <- (n_advqua / n_tok) * 1000
  d["p_advdou", period_name] <- (n_advdou / n_tok) * 10000
  d["p_common", period_name] <- (n_common / n_tok) * 10000
  d["p_neg", period_name] <- (n_neg / n_tok) * 10000
  d["p_pos", period_name] <- (n_pos / n_tok) * 10000
}

#-----------------------
# description of attributes
#-----------------------
# tendency of some statistics 
d1<-d
d1$attr <- rownames(d)
t<- reshape::melt(d1, id = "attr")
t %>% filter(attr %in% c("p_types", "p_dig", "p_verb", "p_advdou")) %>%
  mutate(attr = factor(attr, labels = c("Doubt", "Numbers", "Types", "Verbs"))) %>% 
  ggplot(aes(x = variable, y = value, group = attr)) +
  geom_line(aes(color = attr)) +
  geom_hline(yintercept = 50, linetype = "dotted") +
  theme_pubr() +
  ggtitle("Tendency of some proportions of attributes") +
  scale_fill_manual(values = cbp1) +
  facet_grid(attr ~ ., switch = "y", scales = "free_y") +
  theme(legend.position = "none", axis.text.x = element_text(angle = 90, size = s),
        axis.text.y = element_blank(), axis.title = element_blank(), axis.ticks.y = element_blank())

#-----------------------
# correspondence analysis
#-----------------------

dt <- d %>% filter(!rownames(.) %in% c("p_par", "p_c", "p_advlug", "p_common", "p_pos")) %>% 
  as.matrix() %>%
  t() %>% 
  drop_empty_rc()

# Correspondence analysis of attributes
d_ca <- ca(dt)
eig.val <- factoextra::get_eigenvalue(d_ca)
kbl(eig.val[1:4,], digits = c(3,1,1), caption = "Variance explained by each dimension", 
    col.names = c("Eigenvalue", "Variance explained", "Cumulative \nVariance explained")) %>% 
  kable_styling(bootstrap_options = c("bordered", "hover"), 
                latex_options = c("repeat_header", "HOLD_position"), position = "left")


short_fnames <- short_names(fnames_BASE) 

text_coord <- row_pcoord(d_ca)                # coordinates of texts
word_coord <- col_pcoord(d_ca)                # coordinates of function words
words_df <- tibble(
  word = colnames(dt),
  x = word_coord[, 1],
  y = word_coord[, 2])
texts_df <- tibble(
  text = short_fnames,
  year = gsub("*_.*","",short_fnames),
  sub_corp = sub_corp,
  x = text_coord[, 1],
  y = text_coord[, 2])

ggplot(data = words_df, aes(x = x, y = y)) +
  geom_text(aes(label = word), col = "gray", size = 3) +
  geom_text(data=texts_df,aes(label = year, col = sub_corp), size = 3) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_hline(yintercept = 0, linetype = "dotted") +
  theme_minimal()+
  scale_color_manual(values = cbp1) +
  theme(legend.position = "bottom", axis.text = element_text(size = 8),
        panel.grid = element_blank(),
        axis.line = element_line(colour = "black"),
        legend.title = element_blank(), legend.key.size = unit(0.2, "cm")) +
  labs(x = paste0("Dimension 1 (", round(eig.val[1,2],1),"%)"),
       y = paste0("Dimension 2 (", round(eig.val[2,2],1),"%)"))


#-----------------------
# factor analysis
#-----------------------

# factor analysis, to reduce dimensions  
d_fa <- factanal(dt, 4, scores = "regression") 
loadings <- broom::tidy(d_fa) %>% select(-uniqueness)
loadings <- loadings %>% mutate(fl1 = ifelse(abs(fl1)<0.53,NA, fl1),
                                fl2 = ifelse(abs(fl2)<0.53,NA, fl2),
                                fl3 = ifelse(abs(fl3)<0.53,NA, fl3),
                                fl4 = ifelse(abs(fl4)<0.53,NA, fl4)) %>% 
  arrange(desc(fl1), desc(fl2), desc(fl3))

kbl(loadings, digits = 3, caption = "Factors identified", 
    col.names = c("Attribute","F1-Use of single words, short lenght", "F2-Use of composed sentences", 
                  "F4-Use of verbs but less numbers", "F5-Use of hesitant adverbs"), ) %>% 
  kable_styling(bootstrap_options = c("bordered", "hover"), 
                latex_options = c("repeat_header", "HOLD_position"), position = "left", full_width = TRUE)

#-----------------------
# Biplots
#-----------------------
# identify components that the corpus files belong to
comps <- rownames(dt) %>%
  factor()
d_plot <- tibble(
  dim1 = d_fa$scores[, 1],
  dim2 = d_fa$scores[, 2],
  dim3 = d_fa$scores[, 3],
  dim4 = d_fa$scores[, 4],
  year = comps,
  period = sub_corp)

#plot of factor1 and factor2
for (i in levels(factor(d_plot$period))) {
  dploti <- d_plot %>% filter(period == i) 
  g1<- ggplot() +  
    geom_point(data = d_plot, aes(x = dim1, y = dim2), shape = 3, color = "lightgrey") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_label(data = dploti, aes(x = dim1, y = dim2, label = year), size = 2, color = "white", fill = "#69b3a2") +
    theme_bw() +
    scale_x_continuous(limits = c(-2,2), position = "top")+
    scale_y_continuous(limits = c(-2,2))+
    theme(axis.title = element_text(size = 8)) +
    labs(x = "F1 - Use of single words \nshort length",
         y = "F2 - Use of composed sentences")
  
  #plot of factor3 and factor4
  g2<-ggplot() +
    geom_point(data = d_plot, aes(x = dim3, y = dim4), shape = 3, color = "lightgrey") +
    geom_vline(xintercept = 0, color = "black", linetype = "dashed") +
    geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
    geom_label(data = dploti, aes(x = dim3, y = dim4, label = year), size = 2, color = "white", fill = "#69b3a2") +
    theme_bw() +
    scale_x_continuous(limits = c(-2,2), position = "top")+
    scale_y_continuous(limits = c(-2,2))+
    theme(axis.title = element_text(size = 8)) +
    labs(x = "F3 - Use of verbs \nless numbers",
         y = "F4 - Hesitant speech")
  
  figure1 <- ggarrange(g1, g2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")
  print(annotate_figure(figure1, top = text_grob(paste0("Period ",i))))
}