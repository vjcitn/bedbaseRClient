#' very speculative approach to creating a query
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
#' q = bedbaseRClient:::metadata_query_builder()
#' q
#' if (interactive()) {
#'   md = get_bb_metadata()
#'   md # use httr::content,...
#' }
#' @export
get_bb_metadata = function(query_type="cell_type", query_val="GM12878") {
  q = metadata_query_builder(token1=query_type, query_val=query_val)
  httr::GET(q)
}
