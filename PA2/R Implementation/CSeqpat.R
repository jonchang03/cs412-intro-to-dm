#' Mining Frequent Contiguous Sequential Patterns in a Text Corpus
#'
#'
#' Takes in the filepath and minimum support and performs pattern mining
#' @param filepath Path to the text file/text corpus
#' @param minsupport Minimum absolute support for mining the patterns
#' @param docdelim Document delimiter in the corpus
#' @return A dataframe containing the frequent phrase patterns with their absolute support
#' @import utils
#' @import tm
#' @import ngram
#' @import NLP
#' @export
#' @examples
#' test1 <- c("hoagie institution food year year road ",
#' "place little dated opened weekend fresh food")
#' tf <- tempfile()
#' writeLines(test1, tf)
#' CSeqpat(tf, 2, "\t")
CSeqpat <- function (filepath, minsupport,docdelim) {
    
    loadNamespace("tm")
    loadNamespace("NLP")
    
    Terms_vector <- vector()
    Terms_final_seq_pattern <- vector()
    Terms_temp_df <- data.frame()
    Terms_final_seq_pattern_df <- data.frame()
    
    n <- 1
    
    docs_inp <- utils::read.csv(filepath,header=FALSE,sep = docdelim,stringsAsFactors = FALSE)
    # https://stackoverflow.com/questions/47406555/error-faced-while-using-tm-packages-vcorpus-in-r
    # vcorp <- tm::VCorpus(tm::DataframeSource(docs_inp))
    docs <- data.frame(doc_id=row.names(docs_inp),
                       text=unlist(docs_inp$V1))
    vcorp <- tm::VCorpus(tm::DataframeSource(docs))
    
    repeat {
        
        MgramTokenizer <- function(x) unlist(lapply(NLP::ngrams(NLP::words(x), n), paste, collapse = " "), use.names = FALSE)
        
        tdmm <- tm::TermDocumentMatrix(vcorp, control = list(tokenize = MgramTokenizer))
        
        Terms_temp_df <- as.data.frame(tm::findFreqTerms(tdmm,minsupport),stringsAsFactors = FALSE)
        
        if (length(Terms_temp_df) == 0 || (length(Terms_temp_df) > 0 && is.na(Terms_temp_df[1,1])))
        {
            break
        }
        else
        {
            n <- n+1
        }
        
        for (k in 1 : nrow(Terms_temp_df)) {
            
            sum_df <- as.data.frame(tm::tm_term_score(tdmm,Terms_temp_df[k,1]))
            
            for (sum_row in 1 : nrow(sum_df))
            {
                if (sum_df[sum_row,1] > 1) { sum_df[sum_row,1] <- 1
                }
            }
            
            Terms_final_seq_pattern <- rbind(Terms_final_seq_pattern, c(Terms_temp_df[k,1],colSums(sum_df)[[1]]))
            
        }
        
    }
    
    Terms_final_seq_pattern_df <- as.data.frame(Terms_final_seq_pattern,stringsAsFactors = FALSE)
    
    colnames(Terms_final_seq_pattern_df) <- c("Freq_Phrases","Support")
    
    Terms_final_seq_pattern_df <- Terms_final_seq_pattern_df[Terms_final_seq_pattern_df$Support >= minsupport,]
    
    return(Terms_final_seq_pattern_df)
    
}