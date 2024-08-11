


plot(results.pcm, type = 'infotrace', which.items = c(1,2,3,4),
     main = "", par.settings = simpleTheme(lwd=2)) # item information functions (IIF)
plot(results.pcm, type = 'info', theta_lim = c(-4,4), lwd=2) # test information curve
plot(results.pcm, type = 'itemscore', theta_lim = c(-4,4), lwd=2, facet_items = FALSE) # item scoring traceline
plot(results.pcm, type = 'SE', theta_lim = c(-4,4), lwd=2) # test standard errors
