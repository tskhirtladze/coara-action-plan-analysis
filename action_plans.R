# -----------------------------
# LOAD REQUIRED LIBRARIES
# -----------------------------
library(pdftools)
library(stringr)
library(dplyr)
library(purrr)
library(tools)

# -----------------------------
# SET FOLDER AND PDF FILES
# -----------------------------
folder_path <- ""
pdf_files <- list.files(folder_path, pattern = "\\.pdf$", full.names = TRUE)

if(length(pdf_files) == 0){
  stop("No PDF files found in the folder. Check folder path and files.")
}

cat("Found PDF files:\n")
print(pdf_files)

# -----------------------------
# DEFINE BOILERPLATE KEYWORDS
# -----------------------------
boilerplate_keywords <- c(
  "recognise the diversity of contributions to, and careers in",
  "research in accordance with the needs and nature of the research",
  "base research assessment primarily on qualitative evaluation",
  "for which peer review is central, supported by responsible use of quantitative indicators",
  "abandon inappropriate uses in research assessment of journal- and publication-based metrics",
  "in particular inappropriate uses of journal impact factor \\(jif\\) and h-index",
  "avoid the use of rankings of research organisations in research assessment",
  "commit resources to reforming research assessment",
  "review and develop research assessment criteria, tools and processes",
  "raise awareness of research assessment reform",
  "and provide transparent communication, guidance, and training on assessment criteria and processes as well as their use",
  "exchange practices and experiences to enable mutual learning within and beyond the coalition",
  "communicate progress made on adherence to the principles and implementation of the commitments",
  "evaluate practices, criteria and tools based on solid evidence and the state-of-the-art in research on research",
  "and make data openly available for evidence gathering and research"
)

# -----------------------------
# DEFINE TERMS (INCLUDING PARENT TERMS)
# -----------------------------
library(stringr)

# --- Full terms list (your original, unchanged) ---
terms <- list(
  "open science"              = c("open science","open-science"),
  "open research"             = c("open research","open-research"),
  "open scholarship"          = c("open scholarship","open-scholarship"),
  "open knowledge"            = c("open knowledge","open-knowledge"),
  "equity"                    = c("equity","equality"),
  "inclusion"                 = c("inclusion", "inclusivity", "under represented","under-represented", "underrepresented"),
  "diversity"                 = c("diverse contributions","widening contributions", "language diversity", "epistemic diversity", "indigenous knowledge", "traditional knowledge"),
  "transparency"              = c("transparency", "transparent research"),
  "integrity"                 = c("integrity"),
  "accountability"            = c("accountability"),
  "participation"             = c("participation","participatory"),
  "collaboration"             = c("collaboration"),
  "co creation"               = c("co creation","cocreation","co-creation","co-creative", "cocreated", "co-created", "co-create", "co-research", "coresearch", "co research"),
  "team science"              = c("team science"),
  "openness"                  = c("openness"),
  "societal impact"           = c("societal relevance", "societal impact"),
  "diversity indicators"      = c("diversity indicators"),
  "non traditional outputs"   = c("non traditional outputs", "non-traditional outputs"),
  "altmetrics"                = c("altmetrics", "alt metrics", "alt-metrics", "alternative metrics"),
  "open science indicators"   = c ("open science indicators", "open-science indicators"),
  "multilingualism"           = c("multilingualism", "multi lingualism","multi-lingualism","multilingual", "multi-lingual", "multi lingual"),
  "helsinki initiative"       = c("helsinki initiative"),
  "open data"                 = c("open data","open-data"),
  "data sharing"              = c("data sharing"),
  "fair data"                 = c("fair data","fair principles"),
  "care principles"           = c("care principles"),
  "data repositories"         = c("data repository", "data repositories"),
  "data stewards"             = c("data steward","data stewards","data stewardship"),
  "data management plan"      = c("dmp", "data management plan"),
  "research data management"  = c("data management","rdm", "research data management"),
  "open access"               = c("open access","open-access"),
  "pre prints"                = c("preprints","pre-prints","pre prints","pre-print","pre print", "preprint", "pre printing", "preprinting", "pre-printing"),
  "open repository"           = c("open repository","open-repository","open repositories"),
  "public access"             = c("public access"),
  "publishing ethics"         = c("publishing ethics"),
  "open scholarly communication" = c("open scholarly communication"),
  "reproducible"              = c("reproducibility", "reproducible"),
  "replication studies"       = c("replication studies", "replication study"),
  "pre registration"          = c("pre registration", "preregistration","pre-registration", "pre-registering", "preregistering", "pre-registered", "pre registered", "preregistered"),
  "protocol sharing"          = c("protocol sharing"),
  "open methods"              = c("open methods","open-methods"),
  "open notebooks"            = c("open notebooks", "open-notebooks","open notebook"),
  "open science badges"       = c("open-science badges","open science badges","open-science badge", "open science badge"),
  "reporting guidelines"      = c("reporting guidelines"),
  "open evaluation"           = c("open-evaluation", "open evaluation"),
  "open peer review"          = c("open-peer review","open peer-review", "open peer review"),
  "open source"               = c("open-source","opensource", "open source"),
  "open source software"      = c("open source software","open-source software","opensource software","oss"),
  "open software"             = c("open-software", "open software"),
  "open code"                 = c("open-code", "open code"),
  "open hardware"             = c("open-hardware", "open hardware"),
  "open tools"                = c("open-tools", "open tools"),
  "open research information" = c("open-research information", "open research information"),
  "barcelona declaration"     = c("barcelona declaration"),
  "open infrastructure"       = c("open infrastructure", "open-infrastructure"),
  "open dashboard"            = c("open-dashboard", "open dashboard"),
  "open educational resources"= c("open educational resources", "open education resources","oer"),
  "open educational practices"= c("open educational practices","oep"),
  "open education"            = c("open education"),
  "mooc"                      = c("massive open online course", "mooc", "moocs"),
  "science communication"     = c("science communication"),
  "science popularisation"    = c("science popularization", "science popularisation"),
  "citizen science"           = c("citizen science"),
  "crowdsourcing"             = c("crowd sourcing","crowd-sourcing", "crowdsourcing"),
  "public engagement"         = c("public engagement"),
  "community engagement"      = c("community engagement"),
  "stakeholder engagement"    = c("stakeholder engagement"),
  "knowledge exchange"        = c("knowledge exchange"),
  "knowledge brokering"       = c("knowledge brokering"),
  "transdisciplinary"         = c("transdisciplinary","trans disciplinary", "trans-disciplinary", "transdisciplinarity", "trans disciplinarity"),
  "open science policy"       = c("open-science policy","open science policies", "open science policy"),
  "open science officers"     = c("open science officer", "open-sciece officer", "open-science officers", "open science officers"),
  "open science awareness"    = c("open science awareness")
)

# --- Function: detects only the exact words/phrases from the list ---
count_terms_exact <- function(text, terms) {
  sapply(terms, function(synonyms) {
    sum(sapply(synonyms, function(syn) {
      # Escape regex special characters inside term
      pattern <- paste0("\\b", str_replace_all(syn, "([\\W])", "\\\\\\1"), "\\b")
      str_count(text, regex(pattern, ignore_case = TRUE))
    }))
  })
}

# -----------------------------
# PROCESS EACH PDF
# -----------------------------


# Initialize results list
results_list <- list()

# Process each PDF
for(pdf_file in pdf_files){
  
  doc_name <- basename(pdf_file)
  cat("\nProcessing:", doc_name, "\n")
  
  # Try to extract text
  pdf_text_raw <- tryCatch({
    pdftools::pdf_text(pdf_file)
  }, error = function(e){
    cat("ERROR reading PDF:", doc_name, ":", e$message, "\n")
    return(NULL)
  })
  
  if(is.null(pdf_text_raw)) next  # Skip if failed
  
  # Clean text
  clean_text <- paste(pdf_text_raw, collapse = " ") %>%
    str_replace_all("\n", " ") %>%
    str_squish() %>%
    str_to_lower()
  
  # Remove boilerplate
  for(keyword in boilerplate_keywords){
    clean_text <- str_remove_all(clean_text, regex(keyword, ignore_case = TRUE))
  }
  clean_text <- str_squish(clean_text)
  
  # Save cleaned text
  output_txt <- file.path(folder_path, paste0(file_path_sans_ext(doc_name), "_cleaned.txt"))
  writeLines(clean_text, con = output_txt)
  
  # Word count
  word_count <- str_split(clean_text, "\\s+")[[1]] %>% length()
  
  # Term frequencies
  freq_counts <- count_terms_exact(clean_text, terms)
  
  
  # Binary presence
  binary_counts <- ifelse(freq_counts > 0, 1, 0)
  
  # Store results
  results_list[[doc_name]] <- list(
    Document = doc_name,
    WordCount = word_count,
    Frequencies = freq_counts,
    Binary = binary_counts
  )
}

# Combine frequency dataframe
freq_df <- map_dfr(results_list, function(x){
  df <- as.data.frame(as.list(x$Frequencies), check.names = FALSE)
  df <- cbind(Document = x$Document, WordCount = x$WordCount, df)
  df
})

# Combine binary dataframe
binary_df <- map_dfr(results_list, function(x){
  df <- as.data.frame(as.list(x$Binary), check.names = FALSE)
  df <- cbind(Document = x$Document, df)
  # Rename binary columns
  colnames(df)[-1] <- paste0(colnames(df)[-1], "_binary")
  df
})

# Save CSVs
write.csv(freq_df, file.path(folder_path, "Term_Frequencies.csv"), row.names = FALSE)
write.csv(binary_df, file.path(folder_path, "Term_Binary.csv"), row.names = FALSE)

cat("\nAnalysis complete. Cleaned texts, frequency and binary CSV files saved in folder.\n")


# -----------------------------
# COUNT PDFs WITH BOILERPLATE
# -----------------------------
num_pdfs_with_boilerplate <- sum(sapply(pdf_files, function(pdf_file) {
  
  text <- tryCatch(
    paste(pdftools::pdf_text(pdf_file), collapse = " "),
    error = function(e) return("")
  )
  
  any(str_detect(
    str_to_lower(text),
    regex(boilerplate_keywords, ignore_case = TRUE)
  ))
}))

cat("\nNumber of PDFs containing boilerplate text:",
    num_pdfs_with_boilerplate, "\n")




# -----------------------------
# CLUSTER BINARY ANALYSIS 
# -----------------------------
#CLUSTER MAPPING
# -----------------------------
cluster_mapping <- list(
  "Umbrella Terms" = c("open science", "open research", "open scholarship", "open knowledge"),
  "Equity Diversity and Inclusivity" = c("equity", "inclusion", "diversity"),
  "Research Integrity" = c("transparency", "integrity", "accountability"),
  "Collaboration" = c("participation", "collaboration", "co creation", "team science"),
  "Openness" = c("openness"),
  "Societal Impact" = c("societal impact"),
  "Open Science Indicators" = c("diversity indicators", "non traditional outputs", "altmetrics", "open science indicators"),
  "Multi-lingualism" = c("multilingualism", "helsinki initiative"),
  "Open Data" = c("open data", "data sharing", "fair data", "care principles", "data repositories", "data stewards", "data management plan", "research data management"),
  "Open Publishing" = c("open access", "pre prints", "open repository", "public access", "publishing ethics", "open scholarly communication"),
  "Research Integrity Interventions" = c("reproducible", "replication studies", "pre registration", "protocol sharing", "open methods", "open notebooks", "open science badges", "reporting guidelines"),
  "Open Evaluation" = c("open evaluation", "open peer review"),
  "Open Source Software" = c("open source", "open source software", "open software", "open code", "open hardware", "open tools"),
  "Open Research Information" = c("open research information", "barcelona declaration", "open infrastructure", "open dashboard"),
  "Open Education" = c("open educational resources", "open educational practices","open education", "mooc"),
  "Societal Engagement Activities" = c("science communication", "science popularisation", "citizen science", "crowdsourcing", "public engagement", "community engagement", "stakeholder engagement", "knowledge exchange", "knowledge brokering", "transdisciplinary"),
  "Open Science Policy" = c("open science policy", "open science officers", "open science awareness")
)

# -----------------------------
# MERGE CLUSTER BINARY INTO MAIN BINARY DATAFRAME
# -----------------------------
cluster_results <- lapply(names(results_list), function(doc_name){
  doc_binary <- results_list[[doc_name]]$Binary
  
  cluster_binary <- sapply(cluster_mapping, function(terms_in_cluster){
    any(doc_binary[names(doc_binary) %in% terms_in_cluster] > 0) * 1
  })
  
  c(Document = doc_name, cluster_binary)
})

cluster_df <- bind_rows(lapply(cluster_results, as.data.frame.list))

binary_df_with_clusters <- binary_df %>%
  left_join(cluster_df, by = "Document")

write.csv(binary_df_with_clusters, file.path(folder_path, "Term_Binary_with_Clusters.csv"), row.names = FALSE)


cat("\nCluster binary analysis merged with individual terms saved as Term_Binary_with_Clusters.csv\n")
