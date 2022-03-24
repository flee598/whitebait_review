library(tidyverse)
library(bibliometrix)

#############################
# IMPORT BIB FILES AND TIDY #
#############################

# dois of keeper papers 
df <- read.csv("data/raw/litDataTidied.csv")

df <- df %>%
  filter(article_type == "J") %>%
  filter(pub_year > 1960) %>%
  select(-notes)

# load bib files 
f <- list.files("bibs", pattern = "\\.bib$")
f <- paste("bibs/", f, sep = "")
db <- c("scopus", "wos", "wos", "wos")
dat <- mapply(convert2df, file = f, dbsource = db, MoreArgs = list(format = "bibtex"))

# SCOPUS
scp <- dat[[1]]
scp <- filter(scp, DI %in% df$doi)
row.names(scp) <- NULL
scp_kp <- c("AU", "DE", "C1", "CR", "AB", "RP", "DT", "DI", "SO", "TC", "DB", "TI", "PY", "AU_UN")
scp <- select(scp, all_of(scp_kp))

# WOS
wos <- bind_rows(dat[2:4])
wos <- filter(wos, DI %in% df$doi)
row.names(wos) <- NULL
wos_kp <- c("AU", "DE", "C1", "CR", "AB", "RP", "DT", "DI", "SO", "TC", "DB", "TI", "PY", "AU_UN")
wos <- select(wos, all_of(wos_kp))

# get scp articles not in wos
scp2 <- filter(scp, DI %in% setdiff(DI, wos$DI))

# combine and remove dups
db <- bind_rows(wos, scp2)
db <- db[!duplicated(db$DI),]

colnames(db)

head(db, 1)
#rename columns
nms <- c("Author", "AuthorKeywords", "AuthorAddress", "CitedPapers", "Abstract", "PiAdress", "ArticleType", "Doi", "PublicationName", "TimesCited", "Database",
         "Title", "YearPublished", "Orgs")

colnames(db) <- nms

db$AuthorKeywords <- gsub(";;", ";", db$AuthorKeywords)



# df has 296 records
# db has 286 records 
# find out which are missing and add them to db
df_kp <- filter(df, doi %in% setdiff(doi, db$Doi)) %>%
  select(authors, lead_author_affiliation, article_type, doi, 
         source_title, article_title, pub_year) %>%
  rename(Author = authors, PiAdress = lead_author_affiliation,
         ArticleType = article_type, Doi = doi, PublicationName = source_title,
         Title = article_title, YearPublished = pub_year)

db <- bind_rows(db, df_kp)

# add ID var
db$ID <- 1:nrow(db)

# reorder cols
db <- db %>%
  select(ID,Database, ArticleType, YearPublished, Author, Title,
         PublicationName, AuthorKeywords, Abstract, CitedPapers, Doi, AuthorAddress,
         PiAdress, Orgs, TimesCited)

# save updated csv of both files 
write.csv(db, file = "data/created/biblio_db.csv", row.names = F)
write.csv(df, file = "data/created/handmade_db.csv", row.names = F)


# will now have to go and manually update biblio_db.csv to fill in any gaps
# updated file is called biblio_db_manual_updates.csv


library(tidyverse)

# import file after missing data has been manually added
db <- read.csv(file = "data/created/biblio_db_manual_updates2.csv")

colnames(db)[1] <- "ID"

# tidy up df
db <- db %>% 
  mutate(across(where(is_character),toupper))

# replace NA for now 
db$Author[is.na(db$Author)] <- "missing"

# standardise names
tot <- max(str_count(db$Author, ";"), na.rm = T) + 1

db_names <- db %>%
  select(ID, Author) %>%
  separate(Author, paste("a_", 1:tot, sep = ""), sep = ";") %>%
  pivot_longer(cols = starts_with("a")) %>%
  mutate(value = str_trim(value, side = "both")) %>%
  arrange(value) %>%
  na.omit()

x <- c("AUGSPURGER J","BAKER C","BATTINI M","BOUBEE JA","BRQUEZ A",
       "CRICHIGNO S","CUSSAC V","DAVID B","EIKAAS H","FLORES V",
       "GONZLEZ-WEVAR C","GUTIRREZ P","ALLIBONE R", "HABIT E", "HICKFORD MJ", 
       "HICKS A", "IZQUIERDO M", "JAMES G", "JARVIS M", "JONES P", 
       "LAMBERT P", "MCDOWALL RM ", "MCINTOSH AR", "MORRICONI E", "MUOZ-RAMREZ C",
       "PASCUAL M", "REVENGA J", "ROWE D", "SEGOVIA N", "SEMENAS L",
       "SMITH J", "STEVENS JC", "TAYLOR M", "TORRES P", "VIOZZI G",
       "WEST D", "WHITE RW", "GONZALEZ-WEVAR C", "GUTIRREZ P", "HABIT D",
       "HUENE N", "LAWRENCE C", "MARDONES A", "ORCHARD S", "TAGLIAFERRO M")

y <- c("AUGSPURGER JM","BAKER CF","BATTINI MA","BOUBEE JAT","BRQUEZ AS",
       "CRICHIGNO SA","CUSSAC VE","DAVID BO","EIKAAS HS","FLORES VR",
       "GONZLEZ-WEVAR CA","GUTIRREZ", "ALLIBONE RM", "HABIT EM", "HICKFORD MJH",
       "HICKS AS", "IZQUIERDO MS", "JAMES GD", "JARVIS MG", "JONES PE",
       "LAMBERT PW", "MC DOWALL RM", "MC INTOSH AR", "MORRICONI ER", "MUOZ-RAMREZ CP",
       "PASCUAL MA", "REVENGA JE", "ROWE DK", "SEGOVIA NI", "SEMENAS LG",
       "SMITH JP", "STEVENS JCB", "TAYLOR MJ", "TORRES PF", "VIOZZI GP",
       "WEST DW", "WHITE RWG", "GONZALEZ-WEVAR CA", "GUTIRREZ P PA", "HABIT DE",
       "HUENE NI", "LAWRENCE CS", "MARDONES-LAZCANO A", "ORCHARD DSE", 
       "TAGLIAFERRO ME")

for (i in 1:length(x)) {
  db_names$value <- gsub(y[i], x[i], db_names$value)
}

# back to normal format
db_names <- db_names %>%
  pivot_wider(names_from = name, values_from = value) %>%
  unite(col = "Author", starts_with("a"), sep = ";", na.rm = T)

db_names$Author <- gsub("missing", NA, db_names$Author)

db_names <- db_names %>%
  arrange(ID)

db$Author <- db_names$Author

db <- db %>%
  select(-AuthorAddress, -PiAdress, -Orgs)

# final database! - don't resave this, I've since made additional manual edits
# write.csv(db, file = "data/created/biblio_db_manual_updates3.csv", row.names = F)


db <- read.csv(file = "data/created/biblio_db_manual_updates3.csv")

# load hand recorded database and join to bib database
db2 <- read.csv(file = "data/raw/litDataTidied.csv")

library(tidyverse)
db2 <- db2 %>%
  rename(Title = article_title) %>%
  mutate(across(where(is_character),toupper))

# join on title
db3 <- db %>%
  left_join(db2)

# save and manually fix missing records
write.csv(db3, file = "data/created/biblio_db_manual_updates4.csv", row.names = F)

# load, drop cols and save the final db!
db <- read.csv(file = "data/created/biblio_db_manual_updates4.csv")

colnames(db)

db2 <- db %>%
  select(-ref, -article_type, -authors, -source_title, -pub_year, -doi)

db2 <- db2 %>% 
  mutate(across(where(is_character),toupper))

write.csv(db2, file = "data/created/biblio_db_manual_updates5.csv", row.names = F)


