# ---------------------------------------------------------------+
# Collection of useful functions for sync committee calculations |
# ===============================================================+

# Creation of sync committee when MaxEB = 32 and all validators have 32 ETH EB
# ----------------------------------------------------------------------------
assign_sync_committee <- function(validator_set) {
  
  set.seed(1856)
  
  sync_committee_total <- 0
  
  sync_committee_df <- tibble(
    validator = rep(0,512),
    staker = c(rep(" ",512))
  )
  
  while (sync_committee_total <= 512) {
    sync_candidate <- sample(x=seq(1,nrow(validator_set)),size = 1)    # draw one validator from shuffled validator set
    sync_committee_df$validator[sync_committee_total] <- sync_candidate 
    sync_committee_df$staker[sync_committee_total] <- validator_set$staker[sync_candidate] 
    sync_committee_total = sync_committee_total + 1
  }
  sync_committee_df
}

# Check if any of the sync committee members were selected more than once
# -----------------------------------------------------------------------
assign_unique_sync_mbrs <- function(sync_committee) {
  sync_unique_mbrs <- sync_committee %>%
    group_by(validator) %>%
    summarise(selected = n()) %>%
    arrange(desc(selected))
  
  sync_unique_mbrs
}

# Total up the number of validators belonging to each staker
# ----------------------------------------------------------
assign_total_sync_stakers <- function(sync_committee) {
  sync_staker_totals <- sync_committee %>%
    group_by(staker) %>%
    summarise(total = n()) %>%
    arrange(staker)
  
  sync_staker_totals
}

# ============================================================================================+
# Creation of sync committee when MaxEB = 2048 and validators have varying EBs                |
# In this function we have to determine whether the validator passed the sync committee check |
# ============================================================================================+
assign_maxeb_sync_committee <- function(validator_set) {
  
  set.seed(1856)
  
  sync_committee_total <- 0
  
  sync_committee_df <- tibble(
    validator = rep(0,512),
    staker = c(rep(" ",512)),
    EB = rep(0,512)
  )
  
  while (sync_committee_total <= 512) {
    sync_candidate <- sample(x=seq(1,nrow(validator_set)),size = 1)    # draw one validator from shuffled validator set
    # Check if candidate passes the test
    # ----------------------------------
    random_byte <- floor(runif(1,0,255))
    if (random_byte <= (255*validator_set$EB[sync_candidate])/2048) {
      sync_committee_df$validator[sync_committee_total] <- sync_candidate 
      sync_committee_df$staker[sync_committee_total] <- validator_set$staker[sync_candidate] 
      sync_committee_df$EB[sync_committee_total] <- validator_set$EB[sync_candidate] 
      sync_committee_total = sync_committee_total + 1
    } 
  }
  sync_committee_df
}

# Total up the number of validator, grouping by staker & EB
# ----------------------------------------------------------
assign_total_sync_stakers_EB <- function(sync_committee) {
  sync_staker_totals <- sync_committee %>%
    group_by(staker, EB) %>%
    summarise(total = n(), .groups = 'keep') %>%     # summarise with both grouping vars (default is 'drop_last')
    arrange(staker, EB)
  
  sync_staker_totals
}


