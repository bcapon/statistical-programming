raw <- "39.311024, -76.674227, iconHomicideShooting, ’p2’, ’<dl><dt>Leon Nelson</dt>
<dd class=\"address\">3400 Clifton Ave.<br />Baltimore, MD 21216</dd><dd>black male,
17 years old</dd><dd>Found on January 1, 2007</dd><dd>Victim died at Shock Trauma
</dd><dd>Cause: shooting</dd></dl>’"
gsub("</dd>.*","",gsub(".*<dd>Cause: ", "", raw))
# Can then apply this using sapply(raw_data, function(x) gsub ("</dd>.*","",gsub
#                                 (".*<dd>Cause: ", "", raw)),USE.NAMES = FALSE)

# If data was available, the rest is:
# grep("<dd>Cause:",raw) -> ii ## check for cause field
# raw[-ii] ## OK these really are missing - code as NA
# cause[-ii] <- NA ## set missings to NA
# mean(tolower(cause)=="shooting",na.rm=TRUE) ## prop shooting
