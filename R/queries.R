#' very speculative approach to creating a query
#' @param token1 character(1) defaults to "cell_type"
#' @param query_val character(1) defaults to "GM12878" could be any supported cell type or line tag
#' @return character()
metadata_query_builder = function(token1="cell_type", query_val="GM12878") {
  t1 = "http://bedbase.org/_private_api/query/bedfiles/other-%3E%3E%27"
  t2 = "%27%3D%25s?query_val="
  t3 = "&columns=name&columns=other"
  paste(t1, token1, t2, query_val, t3, sep="")
}

#' metadata query submitter
#' @param query_type character(1) defaults to "cell_type"
#' @param query_val character(1) defaults to "GM12878"
#' @examples
#' @return output of httr::GET
#' q = bedbaseRClient:::metadata_query_builder()
#' q
#' if (interactive()) {
#'   md = get_bb_metadata()
#'   md # use httr::content,...
#'   cont = httr::content(md)
#'   list(len=length(cont), # GM12878
#'        nm=cont[[2]][[1]],
#'        fields=names(cont[[2]][[2]]))
#' }
#' @export
get_bb_metadata = function(query_type="cell_type", query_val="GM12878") {
  q = metadata_query_builder(token1=query_type, query_val=query_val)
  httr::GET(q)
}

redirect_handler <- function(url) {
#
# thanks to Sanchit Saini, https://github.com/lawremi/rtracklayer/issues/42#issuecomment-790801420
#
    tryCatch(
        expr = {
            response <- RCurl::getURL(url, header = TRUE)
            headers <- strsplit(response, "\r\n")[[1]]
            position <- grep("location:", headers)
            redirected_url <- sub("location: ", "", headers[position])
            if (redirected_url != "NA")
                redirected_url
            else url
            },
        error = function(e) {
                return(url)
            }
    )
}

build_bb_query = function(md5sum="78c0e4753d04b238fc07e4ebe5a02984") {
    paste("http://bedbase.org/api/bed/", md5sum, "/file/bigBed", sep="")
}
#' query a bedbase bigbed file
#' @importFrom RCurl getURL
#' @import GenomicRanges
#' @import methods
#' @param md5sum character(1) md5sum used as key into bedbase resources
#' @param \dots passed to rtracklayer::import.bb, which will have format="bigBed" set
#' @return GRanges
#' @examples
#' b1 = query_bb(md5sum="78c0e4753d04b238fc07e4ebe5a02984", 
#'     which=GenomicRanges::GRanges("chr17:37000000-39000000"))
#' b1
#' @export
query_bb = function(md5sum, ...) {
  q = build_bb_query(md5sum)
  rtracklayer::import.bb(q, format="bigBed", ...)
}
    
