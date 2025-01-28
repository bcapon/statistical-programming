raw <- "39.311024, -76.674227, iconHomicideShooting, ’p2’, ’<dl><dt>Leon Nelson</dt>
<dd class=\"address\">3400 Clifton Ave.<br />Baltimore, MD 21216</dd><dd>black male,
17 years old</dd><dd>Found on January 1, 2007</dd><dd>Victim died at Shock Trauma
</dd><dd>Cause: shooting</dd></dl>’"
#rl <- strsplit(raw,",")[[1]]

# . can represent any character
txt <- "He scribbled a note on his clipboard and clicked his heels as he clinched the deal."
grep("c.i",strsplit(txt," ")[[1]])

# * means it should be matched zero or more time, + means it should be matched 1
# or more times and ? indicates it's optional and will be matched at most once
txt <- "To flunk this exam would be disaster, thought Phil, picking up his fork to eat"
grep("f.*k",strsplit(txt," ")[[1]])

# Doesn't work here:
txt <- "Flunk this exam and I’ll be toast, thought Farouk, forking beans into his mouth."
grep("f.*k",strsplit(txt," ")[[1]])
grep("[Ff].*k",strsplit(txt," ")[[1]]) # This includes the capital but returns with chars after
grep("\\b[Ff].*k\\b",strsplit(txt," ")[[1]]) # \\b indicates start and end of word

