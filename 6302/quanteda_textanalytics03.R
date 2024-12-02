# Sample program for using quanteda for text modeling and analysis
# Documentation: vignette("quickstart", package = "quanteda.textmodels")
# Website: https://quanteda.io/

library("quanteda.textmodels")

# Transform corpus to dfm
data(data_corpus_irishbudget2010, package = "quanteda.textmodels")
ie_dfm <- dfm(tokens(data_corpus_irishbudget2010))

# Set reference scores
refscores <- c(rep(NA, 4), 1, -1, rep(NA, 8))

# Predict Wordscores model
ws <- textmodel_wordscores(ie_dfm, y = refscores, smooth = 1)

# Plot estimated word positions (highlight words and print them in red)
textplot_scale1d(ws,
                 highlighted = c("minister", "have", "our", "budget"), 
                 highlighted_color = "red")


# Get predictions
pred <- predict(ws, se.fit = TRUE)

# Plot estimated document positions and group by "party" variable
textplot_scale1d(pred, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))

# Plot estimated document positions using the LBG transformation and group by "party" variable

pred_lbg <- predict(ws, se.fit = TRUE, rescaling = "lbg")

textplot_scale1d(pred_lbg, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))


# Estimate Wordfish model
library("quanteda.textmodels")
wf <- textmodel_wordfish(dfm(tokens(data_corpus_irishbudget2010)), dir = c(6, 5))

# Plot estimated word positions
textplot_scale1d(wf, margin = "features", 
                 highlighted = c("government", "global", "children", 
                                 "bank", "economy", "the", "citizenship",
                                 "productivity", "deficit"), 
                 highlighted_color = "red")


# Plot estimated document positions
textplot_scale1d(wf, groups = data_corpus_irishbudget2010$party)


# Transform corpus to dfm
ie_dfm <- dfm(tokens(data_corpus_irishbudget2010))

# Run correspondence analysis on dfm
ca <- textmodel_ca(ie_dfm)

# Plot estimated positions and group by party
textplot_scale1d(ca, margin = "documents",
                 groups = docvars(data_corpus_irishbudget2010, "party"))



