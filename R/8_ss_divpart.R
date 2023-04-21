# Alpha, Beta and Gamma partitioning + dissimilarity measures

# Partitioning the diversity of the overal system

#' Partitioning Soundscape Diversity into Alpha, Beta and Gamma components
#'
#' @description The diversity of a system can generally be broken down into three components:
#' Alpha, Beta and Gamma diversity.
#'
#' For Hill numbers these three components take a multiplicative relationship: Gamma = Alpha x Beta
#'
#' - \bold{Alpha diversity}: The average diversity of the subsystems
#'
#' - \bold{Beta diversity}: The compositional heterogeneity between subsystems.
#'
#' The beta diversity component can be computed as: Beta = Gamma / Alpha.
#'
#' Beta represents the effective number of equally large and completely unique subsystems within the system. As Beta quantifies the ratio between Gamma and Alpha, it can also be seen as the number of times more diverse the whole system is in effective number of OSUs compared to its constituent subsystems on average. The beta diversity ranges from 1 to N (the number of subsystems in the system).
#'
#' - \bold{Gamma diversity}: The average diversity of the whole system
#'


#' @param soundscape_list A list of soundscape objects of equal dimensions, each soundscape object being produced by \code{\link{ss_create}} (or \code{\link{ss_index_merge}}, \code{\link{ss_binarize}} and \code{\link{ss_aggregate}} in sequence).
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @param output A character string. Indicates the format in which the soundscape diversity is expressed. Options are "percentage" (the fraction between the observed soundscape diversity and the maximum possible soundscape diversity), or "raw" (the number of acoustically active OSUs in the soundscape). Defaults to "percentage".
#'
#' @return A dataframe of diversity values
#' @export
ss_divpart <- function(soundscape_list,
                       qvalue,
                       hier_table = NULL,
                       minfreq = 0,
                       maxfreq = "default",
                       mintime = "default",
                       maxtime = "default",
                       output="percentage"){

  # 0. Testing

  ## At some point, test that all the soundscapes have the same dimensions

  if(qvalue < 0) stop("q value needs to be positive (equal or higher than zero)")
  if(qvalue==1) {qvalue=0.99999}


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if(maxfreq=="default"){

    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))

  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }
  }

  else{

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                mintime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }

  }


  if (maxtime=="default"){

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }

  }

  else{

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                maxtime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)){

    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                rownames_df[[i]]>=minfreq&
                                                  rownames_df[[i]]<=maxfreq))

    colnames_df[[i]] <- as.POSIXct(strptime(paste(soundscape_list[[i]]@first_day,
                                                  colnames(soundscape_list[[i]]@aggregated_df),sep=" "),
                                            format= "%Y-%m-%d %H:%M:%S",
                                            tz=soundscape_list[[i]]@tz))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                            colnames_df[[i]]
                                                            >=mintime_list[[i]]
                                                            & colnames_df[[i]]
                                                            <=maxtime_list[[i]])))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]

  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(rbind,
                lapply(new_soundscape_list,
                       function(x) unlist(x)))))

  rownames(site_by_OSU_matrix) <- paste0("OSU_",
                                         seq(1, nrow(site_by_OSU_matrix), 1))

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))

  # 3. Partition the soundscape diversity into its gamma, alpha and beta components

  # 3.1. Helper function

  hill_part <- function(OSU_matrix,qvalue,hierarchy_table){


    if(sum(colSums(OSU_matrix)) != ncol(OSU_matrix)) {

      if(is.null(dim(OSU_matrix))){

        OSU_matrix <- OSU_matrix / sum(OSU_matrix)

      }

      else {

        OSU_matrix <- sweep(OSU_matrix, 2, colSums(OSU_matrix), FUN = "/")

      }

    }

    #0. Helper functions

    alpha <- function(OSU_matrix,qvalue){

      weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
      pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x==0)),])
      pi_w <- sweep(pi,2,weight,"*")
      pi_w_q <- pi_w^qvalue
      pi_w_q[!pi] <- 0
      N <- length(weight)
      alpha_div <- sum(rowSums(pi_w_q))^(1/(1-qvalue))/N
      return(alpha_div)

    }

    gamma <- function(OSU_matrix,qvalue){

      weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
      pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z==0)),])
      pi_w <- sweep(pi,2,weight,"*")
      gamma_div <- sum(rowSums(pi_w)^qvalue)^(1/(1-qvalue))
      return(gamma_div)

    }

    is_nested <- function(hierarchy_table){
      if(is.null(dim(hierarchy_table)) == TRUE) stop("The hierarchy_table object is not a two-dimensional table.")
      leveln <- ncol(hierarchy_table)
      logic_vector <- c()
      if(leveln == 2){return(TRUE)}
      if(leveln > 2){
        for (i in c((leveln-1):2)){
          levelonly <- length(unique(hierarchy_table[,i]))
          levelparent <- nrow(unique(hierarchy_table[,c(i,i+1)]))
          logic <- levelonly == levelparent
          logic_vector <- c(logic_vector,logic)
        }
        return(all(logic_vector))
      }
    }

    # 1. If no hierarchy_table table was supplied


    if(missing(hierarchy_table)){



      level_1 <- alpha(OSU_matrix = OSU_matrix,
                       qvalue = qvalue)

      level_2 <- gamma(OSU_matrix = OSU_matrix,
                       qvalue = qvalue)

      div_vector <- c(level_1,level_2)
      names(div_vector) <- c("Level_1","Level_2")

      #Beta

      beta <- level_2/level_1

      #Sample size

      N <- c(N1=ncol(OSU_matrix),N2=1)

      #Return values

      if (qvalue==0.99999) {qvalue=1}

      results <- list("Hierarchical_levels" = 2, "Order_diversity" = qvalue, "Hill_numbers" = div_vector, "Sample_size" = N, "Beta" = beta)

      return(results)

  } else{

  #2. If a hierarchy_table was supplied

  if(!missing(hierarchy_table)){

    #Check nestedness of hierarchy_table
    if(is_nested(hierarchy_table) == FALSE) stop("The groups in the hierarchy table are not nested.")


    #Count number of levels

    leveln <- ncol(hierarchy_table)
    levels <- paste(rep("Level_",leveln+1),seq(1:(leveln+1)),sep="")

    #Convert hierarchy columns to character
    hierarchy_table <- apply(hierarchy_table,MARGIN = c(1,2), as.character)
    colnames(hierarchy_table) <- levels[-length(levels)]

    #Generate aggregated OTU tables

    OSU_tables_2 <- list()
    OSU_tables_2[[1]] <- OSU_matrix
    OSU_matrix_sub <- OSU_matrix
    for(i in c(2:leveln)){
      OSU_matrix_sub <- merge(t(OSU_matrix_sub),unique(hierarchy_table[,c(i-1,i)]), by.x="row.names",by.y=as.character(levels[i-1]))
      OSU_matrix_sub <- OSU_matrix_sub[,-1]
      OSU_matrix_sub <- aggregate(subset(OSU_matrix_sub, select=rownames(OSU_matrix)), by=list(OSU_matrix_sub[,as.character(levels[i])]), FUN=sum)
      rownames(OSU_matrix_sub) <- OSU_matrix_sub[,1]
      OSU_matrix_sub <- t(OSU_matrix_sub[,-1])
      OSU_tables_2[[i]] <- OSU_matrix_sub
    }

    #Generate vector of diversities at different hierarchical levels

    #Generate vector of diversities at different hierarchical levels
    div_vector <- c()
    for (i in c(1:(leveln+1))){
      if(i == 1){
        #If lowest level
        div_vector <- c(div_vector,alpha(OSU_tables_2[[1]],qvalue))

      }else if(i == leveln+1){
        #If highest level

        div_vector <- c(div_vector,gamma(OSU_tables_2[[1]],qvalue))

      }else{
        #Intermediate level
        div_vector <- c(div_vector,alpha(OSU_tables_2[[i]],qvalue))

      }
    }


    #Name levels
    names(div_vector) <- levels


    #Get beta values
    beta_vector <- c()
    N_vector <- c()
    for(b in c(1:(leveln))){
      beta <- div_vector[b+1]/div_vector[b]
      names(beta) <- paste("B",paste(b,b+1,sep="_"),sep="")
      beta_vector <- c(beta_vector,beta)
      N <- ncol(OSU_tables_2[[b]])
      N_vector <- c(N_vector,N)
    }

    N_vector <- c(N_vector,1)
    names(N_vector) <- paste(rep("N",leveln+1),seq(1:(leveln+1)),sep="")

    #Return values
    if (qvalue==0.99999) {qvalue=1}
    results <- list("Hierarchical_levels" = (leveln+1), "Order_diversity" = qvalue, "Hill_numbers"=div_vector, "Sample_size"=N_vector, "Beta"=beta_vector)
    return(results)

  }

  }
}

  # Diversity partitioning


  if (is.null(hier_table)){

    soundscape_part <- hill_part(OSU_matrix = site_by_OSU_matrix,
                                 qvalue = qvalue)
  }

  else{

    soundscape_part <- hill_part(OSU_matrix = site_by_OSU_matrix,
                                 qvalue = qvalue,
                                 hierarchy_table = hier_table)
  }

  soundscape_part_table <- dplyr::bind_rows(unlist(soundscape_part))

  colnames(soundscape_part_table) <- c("levels",
                                       "q",
                                       paste0("alpha_l",
                                              seq(1, (length(soundscape_part$Hill_numbers)-1), 1)),
                                       "gamma",
                                       paste0("N",
                                              seq(1, length(soundscape_part$Sample_size), 1)),
                                       paste0("beta_l",
                                              seq(1, (length(soundscape_part$Hill_numbers)-1), 1)))


  if (output=="percentage"){
    soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)] <- (soundscape_part_table[3:(3+length(soundscape_part$Hill_numbers)-1)]/nrow(site_by_OSU_matrix))*100
  }

  else {}

  return(soundscape_part_table)

}


#########################################################################################################


# Getting the pairwise dissimilarities between groups in the system

#' Pairwise Beta diversity and dissimilarity values between subsystems in the system
#'
#' @description Computation of pairwise dissimularities among soundscapes.
#'
#' Regular Beta diversity varies between 1-N, its value being dependent on the number of subsystems under consideration (N). As such, due to its dependence on the number of subsystems, it cannot directly be used as a measure of dissimilarity among communities. Instead, several simple transformations can be performed on the Beta values to get dissimilarity indices ranging between 0-1.
#'
#'
#' - Sorensen-subset (local) overlap:
#'
#' Quantifies the average proportion of a sub-system's OSUs which are shared across all considered sub-systems. It quantifies the overlap (similarity) from the sub-system perspective. To make this into a dissimilarity metric, we take the one-complement (1-Sorensen overlap), being the average proportion of non-shared OSUs in the system.
#'
#' - Jaccard-subset (regional) overlap:
#'
#' Quantifies the effective proportion of OSUs which are shared across all subsystems. It quantifies overlap (similarity) from the perspective of the overall system. To make this into a dissimilarity metric, we take the one-complement (1 - Jaccard overlap), being the effective proportion of non-shard OSUs in the whole system.
#'
#' - Sorensen-subset turnover:
#'
#' Quantifies the normalized turnover rate of OSUs from the perspective of the subsystem (alpha) - or the proportion of a subsystem which changes across subsystems. Once again, we take the one-complement as a dissimilarity measure.
#'
#' - Jaccard-subset turnover:
#'
#' Quantifies the normalized OSU turnover rate from the perspective of the whole system (gamma). Once more, the one-complement gives us our dissimilarity measure.
#'
#'
#'
#' @param soundscape_list A list of dataframes of equal dimensions, each dataframe being produced by \code{\link{ss_aggregate}}.
#' @param qvalue A positive integer or decimal number (>=0), most commonly between 0-3. This parameter modulates the sensitivity of diversity values to the relative abundance of Operational Sound Units (OSUs). A value of 0 corresponds to the richness, a value of 1 is the equivalent number of effective OSUs for the Shannon index, a value of 2 is the equivalent number of effective OSUs for the Simpson index.
#' @param hier_table A matrix indicating the relationship between the soundscapes in the soundscape_list. The first column lists the names of all the soundscapes in the soundscape_list, other columns can be used to group soundscapes into higher hierarchical levels. If no hierarchy table is supplied, the function defaults to a 2-level diversity partitioning.
#' @param minfreq A numeric value indicating the lower frequency limit for which to compute the soundscape diversity. If set to default, uses the lowest available frequency in the dataframe.
#' @param maxfreq A numeric value indicating the upper frequency limit for which to compute the soundscape diversity. If set to default, uses the highest available frequency in the dataframe.
#' @param mintime The lower time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the earliest time for which data exists in the dataframe.
#' @param maxtime The upper time limit for which to compute the soundscape diversity, formatted as "HH:MM:SS". If set to default, uses the latest time for which data exists in the dataframe.
#' @return A list of pairwise matrices for the beta diversity, and the one-complement for the Sorensen-subset overlap, Jaccard-subset overlap, Sorensen-subset turnover and Jaccard-subset turnover.
#' @export

ss_pairdis <- function(soundscape_list,
                       qvalue = 0,
                       hier_table=NULL,
                       minfreq=0,
                       maxfreq="default",
                       mintime="default",
                       maxtime="default"){


  # 0. Testing

  ## At some point, test that all the soundscapes have the same dimensions


  ## Check that, if subsetting, the soundscapes all have the same day and night length

  # 0. Helper functions

  pairwise_dis <- function(OSU_matrix,qvalue,hier_table,metric){

    if(sum(colSums(OSU_matrix)) != ncol(OSU_matrix)) {

      if(is.null(dim(OSU_matrix))){

        OSU_matrix <- OSU_matrix / sum(OSU_matrix)

      }

      else {

        OSU_matrix <- sweep(OSU_matrix, 2, colSums(OSU_matrix), FUN = "/")

      }

    }

    if(missing(metric)){metric= c("C","U","V","S")}

    # Helper functions

    beta_dissimilarity <- function(beta,qvalue,N,metric,type){

      # Helper functions

      alpha <- function(OSU_matrix,qvalue){

        weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
        pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x==0)),])
        pi_w <- sweep(pi,2,weight,"*")
        pi_w_q <- pi_w^qvalue
        pi_w_q[!pi] <- 0
        N <- length(weight)
        alpha_div <- sum(rowSums(pi_w_q))^(1/(1-qvalue))/N
        return(alpha_div)

      }

      gamma <- function(OSU_matrix,qvalue){

        weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
        pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z==0)),])
        pi_w <- sweep(pi,2,weight,"*")
        gamma_div <- sum(rowSums(pi_w)^qvalue)^(1/(1-qvalue))
        return(gamma_div)

      }

      CqN <- function(beta,qvalue,N){
        if(qvalue==1){qvalue=0.99999}
        value = ((1/beta)^(qvalue-1) - (1/N)^(qvalue-1)) / (1 - (1/N)^(qvalue-1))
        return(value)
      }

      SqN <- function(beta,N){
        value = ((1/beta) - 1/N)/(1-1/N)
        return(value)
      }

      UqN <- function(beta,qvalue,N){
        if(qvalue==1){qvalue=0.99999}
        value = ((1/beta)^(1-qvalue) - (1/N)^(1-qvalue)) / (1 - (1/N)^(1-qvalue))
        return(value)
      }

      VqN <- function(beta,N){
        value = (N - beta)/(N-1)
        return(value)
      }

      #Quality-check and warnings
      if(class(beta) == "numeric"){
        betan <- length(beta)
        betas <- beta
        Ns <- N
      }

      if(class(beta) == "list"){
        betan <- length(beta$Beta)
        qvalue <- beta$Order_diversity
        betas <- beta$Beta
        Ns <- beta$Sample_size[-length(beta$Sample_size)]
      }

      ###### MULTIPLE HIERARCHIES NEED TO BE ADDED, and similarity functions updated
      results <- list()

      #Sørensen-type overlap (CqN, 1-CqN)
      if ('C' %in% metric){
        CqNs <- c()
        for(i in c(1:betan)){
          CqNs <- c(CqNs,CqN(betas[i],qvalue,Ns[i]))
          names(CqNs)[i] <- paste("L",paste(i,i+1,sep="_"),sep="")
        }
        if (type == "dissimilarity"){
          rCqNs <- 1 - CqNs
          results <- append(results, list(CqN=rCqNs))
        }else{
          results <- append(results, list(CqN=CqNs))
        }
      }

      #Jaccard-type overlap (UqN, 1-UqN)
      if ('U' %in% metric){
        UqNs <- c()
        for(i in c(1:betan)){
          UqNs <- c(UqNs,UqN(betas[i],qvalue,Ns[i]))
          names(UqNs)[i] <- paste("L",paste(i,i+1,sep="_"),sep="")
        }
        if (type == "dissimilarity"){
          rUqNs <- 1 - UqNs
          results <- append(results, list(UqN=rUqNs))
        }else{
          results <- append(results, list(UqN=UqNs))
        }
      }

      #Sørensen-type turnover-complement (VqN, 1-VqN)
      if ('V' %in% metric){
        VqNs <- c()
        for(i in c(1:betan)){
          VqNs <- c(VqNs,VqN(betas[i],Ns[i]))
          names(VqNs)[i] <- paste("L",paste(i,i+1,sep="_"),sep="")
        }
        if (type == "dissimilarity"){
          rVqNs <- 1 - VqNs
          results <- append(results, list(VqN=rVqNs))
        }else{
          results <- append(results, list(VqN=VqNs))
        }
      }

      #Jaccard-type turnover-complement (SqN, 1-SqN)
      if ('S' %in% metric){
        SqNs <- c()
        for(i in c(1:betan)){
          SqNs <- c(SqNs,SqN(betas[i],Ns[i]))
          names(SqNs)[i] <- paste("L",paste(i,i+1,sep="_"),sep="")
        }
        if (type == "dissimilarity"){
          rSqNs <- 1 - SqNs
          results <- append(results, list(SqN=rSqNs))
        }else{
          results <- append(results, list(SqN=SqNs))
        }
      }

      return(results)

    }


    #Count number of levels
    if(!missing(hier_table)){
      leveln <- ncol(hier_table)
    }else{
      leveln <- 1
    }
    levels <- paste("Level_",seq(1, leveln, 1),sep="")
    if(!missing(hier_table)){
      colnames(hier_table) <- levels
    }

    #Generate aggregated OTU tables
    OSU_tables_2 <- list()
    OSU_tables_2[[1]] <- OSU_matrix
    if(leveln > 1){
      OSU_matrix_sub <- OSU_matrix
      for(i in c(2:leveln)){
        OSU_matrix_sub <- merge(t(OSU_matrix_sub),unique(hier_table[,c(i-1,i)]), by.x="row.names",by.y=as.character(levels[i-1]))
        OSU_matrix_sub <- OSU_matrix_sub[,-1]
        OSU_matrix_sub <- stats::aggregate(subset(OSU_matrix_sub, select=rownames(OSU_matrix)), by=list(OSU_matrix_sub[,as.character(levels[i])]), FUN=mean)
        rownames(OSU_matrix_sub) <- OSU_matrix_sub[,1]
        OSU_matrix_sub <- t(OSU_matrix_sub[,-1])
        OSU_tables_2[[i]] <- OSU_matrix_sub
      }
    }

    #Generate results
    results <- list()
    names <- c()
    for (i in c(1:leveln)){
      #Generate matrices
      OSU_table_sub <- OSU_tables_2[[i]]
      indices <- sort(colnames(OSU_table_sub))

      beta_matrix <- matrix(rep(NA,length(indices)^2), nrow = length(indices), ncol = length(indices))
      colnames(beta_matrix) <- indices
      rownames(beta_matrix) <- indices
      if('C' %in% metric){ CqN_matrix <- beta_matrix }
      if('U' %in% metric){ UqN_matrix <- beta_matrix }
      if('V' %in% metric){ VqN_matrix <- beta_matrix }
      if('S' %in% metric){ SqN_matrix <- beta_matrix }


      #Populate matrices
      for (x in indices){
        for (y in indices){
          if(is.na(beta_matrix[x,y])){ #to avoid repeating mirror operations
            combination <- OSU_table_sub[,c(y,x)]

            if(identical(x,y) == TRUE){
              beta <- NA
            }else{


              alpha <- function(OSU_matrix,qvalue){

                weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
                pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(x) !all(x==0)),])
                pi_w <- sweep(pi,2,weight,"*")
                pi_w_q <- pi_w^qvalue
                pi_w_q[!pi] <- 0
                N <- length(weight)
                alpha_div <- sum(rowSums(pi_w_q))^(1/(1-qvalue))/N
                return(alpha_div)

              }

              gamma <- function(OSU_matrix,qvalue){

                weight <- rep(1/ncol(OSU_matrix),ncol(OSU_matrix))
                pi <- as.data.frame(OSU_matrix[apply(OSU_matrix, 1, function(z) !all(z==0)),])
                pi_w <- sweep(pi,2,weight,"*")
                gamma_div <- sum(rowSums(pi_w)^qvalue)^(1/(1-qvalue))
                return(gamma_div)

              }


              alpha <- alpha(OSU_matrix = combination,qvalue = qvalue)
              gamma <- gamma(OSU_matrix = combination,qvalue = qvalue)
              beta <- gamma/alpha
              beta_matrix[y,x] <- beta

              if('C' %in% metric){
                CqN_matrix[y,x] <- beta_dissimilarity(beta=beta,qvalue=qvalue,N=2,metric="C",type="dissimilarity")$CqN
              }

              if('U' %in% metric){
                UqN_matrix[y,x] <- beta_dissimilarity(beta=beta,qvalue=qvalue,N=2,metric="U",type="dissimilarity")$UqN
              }

              if('V' %in% metric){
                VqN_matrix[y,x] <- beta_dissimilarity(beta=beta,qvalue=qvalue,N=2,metric="V",type="dissimilarity")$VqN
              }

              if('S' %in% metric){
                SqN_matrix[y,x] <- beta_dissimilarity(beta=beta,qvalue=qvalue,N=2,metric="S",type="dissimilarity")$SqN
              }

            }
          }
        }
      }

      #Append matrices to results
      results <- append(results, list(Beta=beta_matrix))
      if('C' %in% metric){results <- append(results, list(CqN=CqN_matrix))}
      if('U' %in% metric){results <- append(results, list(UqN=UqN_matrix))}
      if('V' %in% metric){results <- append(results, list(VqN=VqN_matrix))}
      if('S' %in% metric){results <- append(results, list(SqN=SqN_matrix))}

      #Append matrix names
      names <- c(names,paste(paste("Level_",i,sep=""),"beta",sep="_"))
      if('C' %in% metric){names <- c(names,paste(paste("L",i,sep=""),"CqN",sep="_"))}
      if('U' %in% metric){names <- c(names,paste(paste("L",i,sep=""),"UqN",sep="_"))}
      if('V' %in% metric){names <- c(names,paste(paste("L",i,sep=""),"VqN",sep="_"))}
      if('S' %in% metric){names <- c(names,paste(paste("L",i,sep=""),"SqN",sep="_"))}
    }

    #Modify names
    names(results) <- names
    return(results)

  }

  # 1. Preparing the subsetting arguments

  # Frequency subsetting

  minfreq <- minfreq

  if(maxfreq=="default"){

    maxfreq <- max(as.numeric(rownames(soundscape_list[[1]]@aggregated_df)))

  }

  else{maxfreq <- maxfreq}

  if (mintime=="default"){

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- min(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }
  }

  else{

    mintime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(mintime_list)){

      mintime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                mintime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }

  }


  if (maxtime=="default"){

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- max(
        as.POSIXct(
          strptime(
            paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                  colnames(soundscape_list[[i]]@aggregated_df),
                  sep=" "),
            format= "%Y-%m-%d %H:%M:%S",
            tz = soundscape_list[[i]]@tz)))

    }

  }

  else{

    maxtime_list <- vector("list", length(soundscape_list))

    for (i in 1:length(maxtime_list)){

      maxtime_list[[i]] <- as.POSIXct(
        strptime(
          paste(substr(soundscape_list[[i]]@first_day, 1, 12),
                maxtime,
                sep=" "),
          format= "%Y-%m-%d %H:%M:%S",
          tz = soundscape_list[[i]]@tz))

    }
  }

  # 2. Subsetting the soundscapes based on the time and freq arguments

  # Subsetting vectors

  rownames_df <- vector("list", length(soundscape_list))
  rownames_subset <- vector("list", length(soundscape_list))
  colnames_df <- vector("list", length(soundscape_list))
  colnames_subset <- vector("list", length(soundscape_list))
  new_soundscape_list <- vector("list", length(soundscape_list))

  for (i in 1:length(soundscape_list)){

    rownames_df[[i]] <- as.numeric(rownames(soundscape_list[[i]]@aggregated_df))

    rownames_subset[[i]] <- as.character(subset(rownames_df[[i]],
                                                rownames_df[[i]]>=minfreq&
                                                  rownames_df[[i]]<=maxfreq))

    colnames_df[[i]] <- as.POSIXct(strptime(paste(soundscape_list[[i]]@first_day,
                                                  colnames(soundscape_list[[i]]@aggregated_df),sep=" "),
                                            format= "%Y-%m-%d %H:%M:%S",
                                            tz=soundscape_list[[i]]@tz))

    colnames_subset[[i]] <- as.character(hms::as_hms(subset(colnames_df[[i]],
                                                            colnames_df[[i]]
                                                            >=mintime_list[[i]]
                                                            & colnames_df[[i]]
                                                            <=maxtime_list[[i]])))

    new_soundscape_list[[i]] <- soundscape_list[[i]]@aggregated_df[rownames_subset[[i]], colnames_subset[[i]]]

  }

  # 2. Prepare the soundscape list to be in the correct format

  # 2.1. Create site-by-osu df

  site_by_OSU_matrix <-
    as.data.frame(
      t(do.call(rbind,
                lapply(new_soundscape_list,
                       function(x) unlist(x)))))

  rownames(site_by_OSU_matrix) <- paste0("OSU_",
                                         seq(1, nrow(site_by_OSU_matrix), 1))

  colnames(site_by_OSU_matrix) <- as.character(names(soundscape_list))



  # Calculate the pairwise dissimilarities

  if(is.null(hier_table)){

    soundscape_pairdis <- pairwise_dis(OSU_matrix =  site_by_OSU_matrix,
                                       qvalue = qvalue)

  }

  else{
    soundscape_pairdis <- pairwise_dis(OSU_matrix = site_by_OSU_matrix,
                                       qvalue = qvalue,
                                       hierarchy = hier_table)
  }

  soundscape_pairdis


}
