#' Defunct function
#'
#' @description Throws an error message when depreciated function names are used
#' @param msg Message that is returned when a deprecated function is used.
#' @export
defunct = function(msg = "This function is deprecated") function(...) return(stop(msg))

#' index_config function
#'
#' @description Depreciated - Use 'ss_index_config' instead
#' @param ... no parameters
#' @export
index_config = defunct("index_config changed name to ss_index_config")

#' index_calc function
#'
#' @description Depreciated - Use 'ss_index_calc' instead
#' @param ... no parameters
#' @export
index_calc = defunct("index_calc changed name to ss_index_calc")

#' merge_csv function
#'
#' @description Depreciated - Use 'ss_index_merge' instead
#' @param ... no parameters
#' @export
merge_csv = defunct("merge_csv changed name to ss_index_merge")

#' threshold_df function
#'
#' @description Depreciated - Use 'ss_threshold' instead
#' @param ... no parameters
#' @export
threshold_df = defunct("threshold_df changed name to ss_threshold")

#' binarize_df function
#'
#' @description Depreciated - Use 'ss_binarize' instead
#' @param ... no parameters
#' @export
binarize_df = defunct("binarize_df changed name to ss_binarize")

#' check_thresh function
#'
#' @description Depreciated - Use 'ss_threshold_check' instead
#' @param ... no parameters
#' @export
check_thresh = defunct("check_thresh changed name to ss_threshold_check")

#' get_mode function
#'
#' @description Depreciated - Use 'ss_get_mode' instead
#' @param ... no parameters
#' @export
get_mode = defunct("get_mode changed name to ss_get_mode")

#' aggregate_df function
#'
#' @description Depreciated - Use 'ss_aggregate' instead
#' @param ... no parameters
#' @export
aggregate_df = defunct("aggregate_df changed name to ss_aggregate")

#' heatmapper function
#'
#' @description Depreciated - Use 'ss_heatmap' instead
#' @param ... no parameters
#' @export
heatmapper = defunct("heatmapper changed name to ss_heatmap")

#' sounddiv function
#'
#' @description Depreciated - Use 'ss_diversity' instead
#' @param ... no parameters
#' @export
sounddiv = defunct("sounddiv changed name to ss_diversity")

#' sounddiv_by_time function
#'
#' @description Depreciated - Use 'ss_diversity_plot' instead
#' @param ... no parameters
#' @export
sounddiv_by_time = defunct("sounddiv_by_time changed name to ss_diversity_plot")

#' sounddiv_prof function
#'
#' @description Depreciated - Use 'ss_diversity_prof' instead
#' @param ... no parameters
#' @export
sounddiv_prof = defunct("sounddiv_prof changed name to ss_diversity_prof")

#' sounddiv_part function
#'
#' @description Depreciated - Use 'ss_divpart' instead
#' @param ... no parameters
#' @export
sounddiv_part = defunct("sounddiv_part changed name to ss_divpart")

#' sounddiv_pairdis function
#'
#' @description Depreciated - Use 'ss_pairdis' instead
#' @param ... no parameters
#' @export
sounddiv_pairdis = defunct("sounddiv_pairdis changed name to ss_pairdis")

