# Perform propensity and proximity score matching and return a Match object

library(RPostgreSQL)
library(randomForest)
library(Matching)

doMatching <- function (
  # the covariates for building the model
  covariates,
  # the method to use (prox for random forest, prop for propensity)
  method = 'prox',
  # the radius in which a house is assumed to influence its neighbors
  influence = 500,
  # The radius within which matches must be drawn from. Leave NULL for unrestricted
  matches = 10560,
  # 10K or DEM: are we looking at demolished properties or those that have been invested in?
  which = '10K',
  # Pre or post?
  post = T,
  # how many trees?
  trees = 10000
  ) {
  # Connect to the database and fetch data
  dbdrv <- dbDriver('PostgreSQL')
  
  # Put your database connection parameters here
  con <- dbConnect(dbdrv, 'host',
                   user='user', password='password')
  
  # Create temporary tables for properties that received investment
  if (which=='10K') {
    # We are creating temporary tables for properties that have seen $10,000 in permit investment during the investment period
    
    # This query selects all properties that were distressed and have seen more than $10,000 in non-demolition permit activity in 2008-2010
    # We use the geometry this temporary table to identify which sales are treatment (because they are near one of these properties)
    dbSendQuery(con, "CREATE TEMPORARY TABLE inv11 AS
              SELECT q.*, the_geom FROM parcels.parcels2008 RIGHT JOIN
              (SELECT parid parcelid, SUM(amount) totprmt FROM tax2013_permit
              WHERE EXTRACT(year FROM permdt) >= 2008 AND EXTRACT(year FROM permdt) <= 2010
              AND why <> 'DEM' GROUP BY parid) q USING (parcelid)
              LEFT JOIN nbn.final USING (parcelid)
              WHERE totprmt >= 10000 AND strprob IN (1, 2, 3, 4)
              ")
  
   # These are the properties that received confounding investments
   # This query selects properties that saw $10,000 in non-demolition permit activity in 2011-2013
   # we exclude sales around these because we're not sure whether the market would have reacted yet or not
   dbSendQuery(con, "CREATE TEMPORARY TABLE inv13 AS
              SELECT q.*, the_geom FROM parcels.parcels2008 RIGHT JOIN
              (SELECT parid parcelid, SUM(amount) totprmt FROM tax2013_permit
              WHERE EXTRACT(year FROM permdt) >= 2011 AND EXTRACT(year FROM permdt) <= 2013
              AND why <> 'DEM' GROUP BY parid) q USING (parcelid)
              LEFT JOIN nbn.final USING (parcelid)
              WHERE totprmt >= 10000 AND strprob IN (1, 2, 3, 4)
              ")
   
  } else if (which == 'DEM') {
    # These are properties which had demolition permits pulled 2008-2010 and did not subsequently see an additional building permit
    dbSendQuery(con, "CREATE TEMPORARY TABLE inv11 AS
              SELECT q.*, the_geom FROM parcels.parcels2008 RIGHT JOIN
              (SELECT parid parcelid, max(permdt) dt,  SUM(amount) totprmt FROM tax2013_permit
                WHERE EXTRACT(year FROM permdt) >= 2008 AND EXTRACT(year FROM permdt) <= 2010
                AND why = 'DEM' GROUP BY parid) q USING (parcelid)
                LEFT JOIN nbn.final USING (parcelid)
                WHERE strprob IN (1, 2, 3, 4)
                -- if they subsequently received construction ignore them
                AND (SELECT count(*) FROM tax2013_permit p2 WHERE p2.parid=q.parcelid AND p2.permdt > q.dt AND p2.why <> 'DEM') = 0
                ")
    
    # These are the confounding demolitions: properties which had demolition permits pulled 2011-2013 and did not subsequently see an additional building permit
    dbSendQuery(con, "CREATE TEMPORARY TABLE inv13 AS
              SELECT q.*, the_geom FROM parcels.parcels2008 RIGHT JOIN
              (SELECT parid parcelid, max(permdt) dt,  SUM(amount) totprmt FROM tax2013_permit
                WHERE EXTRACT(year FROM permdt) >= 2011 AND EXTRACT(year FROM permdt) <= 2013
                AND why = 'DEM' GROUP BY parid) q USING (parcelid)
                LEFT JOIN nbn.final USING (parcelid)
                WHERE strprob IN (1, 2, 3, 4)
                -- if they subsequently received construction ignore them
                AND (SELECT count(*) FROM tax2013_permit p2 WHERE p2.parid=q.parcelid AND p2.permdt > q.dt AND p2.why <> 'DEM') = 0
                ")
  }
  
  # This is our main data table, which has columns for all of the covariates upon which we match
  # Leave the outer query alone; it just creates row numbers 1 to nrow. This is important because it means we can
  # use row index, the rn column, and row names interchangeably.
  # Columns are as follows:
  # rn: row number, described above
  # price: Sale price
  # parid: Parcel ID (optional, not used for matching)
  # rtotapr: Appraised value (before rehabilitations took place)
  # rmbed: Number of bedrooms
  # fixbath: Number of bathroom fixtures (more or less the number of bathrooms)
  # sqft: Square feet of living area
  # count_minor_250: Number of somewhat distressed properties within 250 feet of this property, before rehabilitation
  # count_severe_250: Number of severely distressed properties within 500 feet of this property, before rehabilitation
  # count_minor_500: Number of somehat distressed properties 250-500 feet from this property, before rehabilitation
  # count_severe_500: Number of severely distressed properties 250-500 feet from this property, before rehabilitation
  # blight: Is this property distressed?
  # age: How old is the building on this property?
  # medianhhinc: What is the median household income in the Census tract containing this property?
  # pctunemployed: What is the rate of unemployment in the Census tract containing this property?
  # pctownocc: What percentage of housing units that are owner-occupied in the Census tract containing this property?
  # centroid: The centroid of this parcel, for use in distance calculations
  # treatment: Is this parcel near a remediated parcel?
  
  # Note that the remediated parcels themselves are not included in this query. Further note that this gets only
  # sales, not all parcels.
  # Finally, note that the years for sales are hard-coded at >= 2011 (post-rehab) or = 2007 (pre-rehab). You may need to change this.
  
  # Ordinarily we'd just run this in dbGetQuery but we need the results to calculate the geographic matching
  dbSendQuery(con, sprintf("
              CREATE TEMPORARY TABLE pairdat AS
              SELECT ROW_NUMBER() OVER (ORDER BY q.parid) rn, q.* FROM (SELECT 
              s.price, s.parid, a.rtotapr, dd.rmbed, dd.fixbath, dd.sfla sqft, bc.count_minor_250, bc.count_severe_250, bc.count_minor_500, bc.count_severe_500, n.strprob IN (1, 2,3,4) AS blight,
              2014 - dd.yrblt age, acs3.hc01_est_vc69 medianhhinc, acs3.hc02_est_vc06 pctunemployed, acs4.hc02_est_vc51 AS pctownocc, centroid,
              ST_DWithin(p.the_geom, (SELECT ST_Union(the_geom) FROM inv11), 500) AS treatment,
              ROW_NUMBER() OVER (PARTITION BY s.parid ORDER BY s.saledt DESC) prn
              FROM tax2013_sales s
              LEFT JOIN parcels.parcels2008 p ON (s.parid=p.parcelid)
              LEFT JOIN tax2008_asmt a ON (a.parid=s.parid)
              LEFT JOIN (SELECT *, ROW_NUMBER() OVER (PARTITION BY parid ORDER BY sfla DESC) rn FROM tax2008_dweldat) dd ON (s.parid=dd.parid AND dd.rn=1)
              LEFT JOIN tax2008_owndat od ON (od.parid=s.parid)
              LEFT JOIN mconway.blightcount2008 bc ON (bc.parcelid=s.parid)
              LEFT JOIN nbn.final n ON (n.parcelid=s.parid)
              -- these are to make sure this house itself hasn't seen significant investment
              LEFT JOIN inv13 ON (inv13.parcelid=s.parid)
              LEFT JOIN inv11 ON (inv11.parcelid=s.parid)
              LEFT JOIN acs_09_5yr_dp5yr3 acs3 ON (acs3.geo_id2=p.tractid2000)
              LEFT JOIN acs_09_5yr_dp5yr4 acs4 ON (acs4.geo_id2=p.tractid2000)
              WHERE a.luc='062'
              AND s.price <> 0 AND s.saleval = 'A'
              AND inv11.parcelid IS NULL AND inv13.parcelid IS NULL
              -- When there were recent investments, we can't know quite what the right point in time is for comparison
              AND NOT ST_DWithin(p.the_geom, (SELECT ST_Union(the_geom) FROM inv13), 500)
              AND EXTRACT(year FROM s.saledt) %s
              AND ST_Within(centroid, (SELECT ST_Union(the_geom) FROM muniboundaries_shelby WHERE name='Memphis')) 
              AND rmbed IS NOT NULL) q
              WHERE prn=1
              ORDER BY 1 DESC;
              ", ifelse(post, '>= 2011', '= 2007')))
  
  # Creating a geographic index will speed up creating neighbors an incredible amount
  dbSendQuery(con, 'CREATE INDEX pairdat_gix ON pairdat USING gist (centroid)')
  
  # Pull down the data created above
  data <- dbGetQuery(con, 'SELECT * FROM pairdat ORDER BY parid')
  
  # Restrict the matches spatially
  if (!is.null(matches)) {
    # For proximity score matching, we need a list of all possible matches
    if (method == 'prox') {
      possibleMatches <- dbGetQuery(con, sprintf('SELECT p1.rn treatment, p2.rn control FROM pairdat p1
                                INNER JOIN pairdat p2 ON (ST_DWithin(p1.centroid, p2.centroid, %s))
                                WHERE p1.treatment AND NOT p2.treatment', matches))
    } else if (method == 'prop') {
      # For propensity score matching, we need a list of all *impossible* matches
      # We select -1 as the third column because this is what is expected by Match for a restrictions matrix to
      # disallow a match
      restrict <- data.matrix(dbGetQuery(con, sprintf('SELECT p1.rn treatment, p2.rn control, -1 FROM pairdat p1
                        INNER JOIN pairdat p2 ON (NOT ST_DWithin(p1.centroid, p2.centroid, %s))
                       WHERE p1.treatment AND NOT p2.treatment', matches)))
    }
  }
  
  # We clean up the data to remove NA's
  data$blight[is.na(data$blight)] <- F
  # TODO: how many of these nulls are due to no blight, and how many due to mismatch between 2008 and 2013?
  data$count_minor_250[is.na(data$count_minor_250)] <- 0
  data$count_severe_250[is.na(data$count_severe_250)] <- 0
  data$count_minor_500[is.na(data$count_minor_500)] <- 0
  data$count_severe_500[is.na(data$count_severe_500)] <- 0
  
  # We match on the log because it's more normal. We add 1 to avoid log(0)
  data['logrtotapr'] <- log(data$rtotapr + 1)
  
  # We factorize treatment so that the random forest will do classification
  data$treatment <- as.factor(data$treatment)
  
  # We make the row names of data 1:n
  rownames(data) <- data$rn
  
  # make sure rn and index is interchangeable
  stopifnot(data$rn==1:nrow(data))
  
  # Close the connection, dropping temporary tables and freeing space on the DB server
  dbDisconnect(con)
  
  # If you've kept column names consistent, you shouldn't need to edit (much) below here
  
  spatial <- !is.null(matches)
  
  if (method == 'prox') {
    # Do proximity score matching
    rf <- randomForest(treatment~., data=data[,c('treatment', covariates)], proximity=T, nodesize=20, ntree=trees)
    rf
  
    varImpPlot(rf)
    
    if (!spatial) {
      p <- 1 - rf$proximity
    } else {
      p <- matrix(-1, nrow(rf$proximity), ncol(rf$proximity))
      
      for (i in 1:nrow(possibleMatches)) {
        pm <- possibleMatches[i,]
        # This leaves the matrix in an asymmetric state but that's ok because we
        # decompose it soon using this indexing
        p[pm$treatment,pm$control] <- 1 - rf$proximity[pm$treatment, pm$control]
      }
    }  
    
    restrict <- matrix(NA, sum(data$treatment==T)*sum(data$treatment==F), 3)
    idx <- 1
    
    for (j in data$rn[data$treatment==F]) {
      for (i in data$rn[data$treatment==T]) {
        restrict[idx,] <- c(i, j, p[i,j])
        idx <- idx + 1
      }
    }
    
    m <- Match(Y=log(data$price), Tr=data$treatment==T, X=data[,covariates], M=1, restrict=restrict)
    
    # Ensure that matching was done correctlyish
    if (spatial) {
      for (i in 1:length(m$index.treated)) {
        stopifnot(m$index.control[i] %in% possibleMatches[possibleMatches$treatment==m$index.treated[i],'control'])
      }
    }
    
    # should be large
    getp <- function (r) { return(rf$proximity[r[1],r[2]])}
    hist(apply(data.frame(c=m$index.control, t=m$index.treated), 1, getp))
  } else if (method == 'prop') {
    # Do propensity score matching
    glm.fit <- glm(treatment~., family='binomial', data=data[,c('treatment', covariates)])
    data$propensity <- log(glm.fit$fitted.values / (1 - glm.fit$fitted.values))
    
    if (spatial) {
      m <- Match(Y=log(data$price), Tr=data$treatment==T, X=data$propensity, M=1, restrict=restrict)
    } else {
      m <- Match(Y=log(data$price), Tr=data$treatment==T, X=data$propensity, M=1)
    }
  }
  return(list(m, data))
}

# Compute balance statistics and make balance plots
bal <- function(match, data, covariates, matchbalance=T) {
  # balance stats
  # Note that ugly formula and some covariates are hardcoded and thus will not work in other contexts
  if (matchbalance) {
    b <- MatchBalance(I(treatment==T)~rmbed^3+log(I(rmbed + 1))+rmbed:rtotapr+fixbath^3+log(I(fixbath + 1))+fixbath:rtotapr+sqft^3+log(I(sqft + 1))+sqft:rtotapr+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:rtotapr+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:rtotapr+blight^3+log(I(blight + 1))+blight:rtotapr+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:rtotapr+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:rtotapr+pctownocc^3+log(I(pctownocc + 1))+pctownocc:rtotapr+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:rtotapr+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:rtotapr+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:rmbed+fixbath^3+log(I(fixbath + 1))+fixbath:rmbed+sqft^3+log(I(sqft + 1))+sqft:rmbed+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:rmbed+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:rmbed+blight^3+log(I(blight + 1))+blight:rmbed+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:rmbed+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:rmbed+pctownocc^3+log(I(pctownocc + 1))+pctownocc:rmbed+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:rmbed+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:rmbed+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:fixbath+rmbed^3+log(I(rmbed + 1))+rmbed:fixbath+sqft^3+log(I(sqft + 1))+sqft:fixbath+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:fixbath+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:fixbath+blight^3+log(I(blight + 1))+blight:fixbath+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:fixbath+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:fixbath+pctownocc^3+log(I(pctownocc + 1))+pctownocc:fixbath+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:fixbath+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:fixbath+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:sqft+rmbed^3+log(I(rmbed + 1))+rmbed:sqft+fixbath^3+log(I(fixbath + 1))+fixbath:sqft+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:sqft+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:sqft+blight^3+log(I(blight + 1))+blight:sqft+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:sqft+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:sqft+pctownocc^3+log(I(pctownocc + 1))+pctownocc:sqft+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:sqft+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:sqft+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:count_minor_250+rmbed^3+log(I(rmbed + 1))+rmbed:count_minor_250+fixbath^3+log(I(fixbath + 1))+fixbath:count_minor_250+sqft^3+log(I(sqft + 1))+sqft:count_minor_250+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:count_minor_250+blight^3+log(I(blight + 1))+blight:count_minor_250+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:count_minor_250+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:count_minor_250+pctownocc^3+log(I(pctownocc + 1))+pctownocc:count_minor_250+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:count_minor_250+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:count_minor_250+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:count_severe_250+rmbed^3+log(I(rmbed + 1))+rmbed:count_severe_250+fixbath^3+log(I(fixbath + 1))+fixbath:count_severe_250+sqft^3+log(I(sqft + 1))+sqft:count_severe_250+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:count_severe_250+blight^3+log(I(blight + 1))+blight:count_severe_250+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:count_severe_250+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:count_severe_250+pctownocc^3+log(I(pctownocc + 1))+pctownocc:count_severe_250+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:count_severe_250+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:count_severe_250+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:blight+rmbed^3+log(I(rmbed + 1))+rmbed:blight+fixbath^3+log(I(fixbath + 1))+fixbath:blight+sqft^3+log(I(sqft + 1))+sqft:blight+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:blight+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:blight+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:blight+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:blight+pctownocc^3+log(I(pctownocc + 1))+pctownocc:blight+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:blight+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:blight+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:medianhhinc+rmbed^3+log(I(rmbed + 1))+rmbed:medianhhinc+fixbath^3+log(I(fixbath + 1))+fixbath:medianhhinc+sqft^3+log(I(sqft + 1))+sqft:medianhhinc+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:medianhhinc+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:medianhhinc+blight^3+log(I(blight + 1))+blight:medianhhinc+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:medianhhinc+pctownocc^3+log(I(pctownocc + 1))+pctownocc:medianhhinc+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:medianhhinc+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:medianhhinc+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:pctunemployed+rmbed^3+log(I(rmbed + 1))+rmbed:pctunemployed+fixbath^3+log(I(fixbath + 1))+fixbath:pctunemployed+sqft^3+log(I(sqft + 1))+sqft:pctunemployed+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:pctunemployed+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:pctunemployed+blight^3+log(I(blight + 1))+blight:pctunemployed+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:pctunemployed+pctownocc^3+log(I(pctownocc + 1))+pctownocc:pctunemployed+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:pctunemployed+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:pctunemployed+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:pctownocc+rmbed^3+log(I(rmbed + 1))+rmbed:pctownocc+fixbath^3+log(I(fixbath + 1))+fixbath:pctownocc+sqft^3+log(I(sqft + 1))+sqft:pctownocc+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:pctownocc+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:pctownocc+blight^3+log(I(blight + 1))+blight:pctownocc+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:pctownocc+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:pctownocc+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:pctownocc+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:pctownocc+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:count_severe_500+rmbed^3+log(I(rmbed + 1))+rmbed:count_severe_500+fixbath^3+log(I(fixbath + 1))+fixbath:count_severe_500+sqft^3+log(I(sqft + 1))+sqft:count_severe_500+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:count_severe_500+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:count_severe_500+blight^3+log(I(blight + 1))+blight:count_severe_500+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:count_severe_500+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:count_severe_500+pctownocc^3+log(I(pctownocc + 1))+pctownocc:count_severe_500+count_minor_500^3+log(I(count_minor_500 + 1))+count_minor_500:count_severe_500+
                            rtotapr^3+log(I(rtotapr + 1))+rtotapr:count_minor_500+rmbed^3+log(I(rmbed + 1))+rmbed:count_minor_500+fixbath^3+log(I(fixbath + 1))+fixbath:count_minor_500+sqft^3+log(I(sqft + 1))+sqft:count_minor_500+count_minor_250^3+log(I(count_minor_250 + 1))+count_minor_250:count_minor_500+count_severe_250^3+log(I(count_severe_250 + 1))+count_severe_250:count_minor_500+blight^3+log(I(blight + 1))+blight:count_minor_500+medianhhinc^3+log(I(medianhhinc + 1))+medianhhinc:count_minor_500+pctunemployed^3+log(I(pctunemployed + 1))+pctunemployed:count_minor_500+pctownocc^3+log(I(pctownocc + 1))+pctownocc:count_minor_500+count_severe_500^3+log(I(count_severe_500 + 1))+count_severe_500:count_minor_500,
                          data=data[,c('treatment', 'rtotapr', covariates)], match.out=match, nboots=1000, paired=F)
  }
  
  # histograms of the the before-and-after
  for (covariate in c('rtotapr', covariates)) {
    #graphics.off()
    layout(matrix(1:4, 2, 2))
    
    if (is.numeric(data[,covariate])) {
      # treatment, before and after
      hist(data[data$treatment==T,covariate], sub=paste(covariate, 'treatment, before matching'))
      hist(data[data$treatment==T,covariate], sub=paste(covariate, 'treatment, after matching'))
    
      hist(data[data$treatment==F,covariate], sub=paste(covariate, 'control, before matching'))
      hist(data[match$index.control,covariate], sub=paste(covariate, 'control, after matching'))
    } else {
      plot(as.factor(data[data$treatment==T,covariate]), sub=paste(covariate, 'treatment, before matching'))
      plot(as.factor(data[data$treatment==T,covariate]), sub=paste(covariate, 'treatment, after matching'))
      
      plot(as.factor(data[data$treatment==F,covariate]), sub=paste(covariate, 'control, before matching'))
      plot(as.factor(data[match$index.control,covariate]), sub=paste(covariate, 'control, after matching'))
    }
    
  }
}