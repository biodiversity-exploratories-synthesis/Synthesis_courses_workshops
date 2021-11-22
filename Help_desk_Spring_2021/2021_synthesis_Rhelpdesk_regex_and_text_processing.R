######################################################
#                                                    #
# REGULAR EXPRESSIONS AND TEXT PROCESSING IN R       #
#                                                    #
######################################################
# Biodiversity Exploratories Synthesis R helpdesk
# Noëlle Schenk 05.02.2021
#
# this file contains examples and exercises for text processing and 
# regular expressions in R.

###
# RESOURCES
#
# stringr documentation https://www.rdocumentation.org/packages/stringr/versions/1.4.0
# regular expressions in R : https://stat.ethz.ch/R-manual/R-devel/library/base/html/regex.html
# R regex vignette tutorial : https://cran.r-project.org/web/packages/stringr/vignettes/regular-expressions.html
# Cheatsheet : https://rstudio.com/wp-content/uploads/2016/09/RegExCheatsheet.pdf
# install.packages("stringr")
require(stringr)


###
# REGULAR EXPRESSIONS
x <- c("why", "video", "cross", "extra", "deal", "authority")
str_subset(x, "[aeiou]") # gives subset of matches
str_detect(x, "[aeiou]") # tells you if there’s any match to the pattern
str_count(x, "[aeiou]")  # counts number of matches per word

###
# SPECIAL CHARACTERS
x <- c("abc ABC 123 .!?\(){}")
str_locate(x, "a")



###
# TEXT MATCHING
text = c(c("Bumblebees are found mainly in northern temperate regions, thought 
here are a few native South American species and New Zealand has some 
naturalised species that were introduced around 100 years ago to pollinate 
red clover. They range much further north than honey bees, and colonies can 
be found on Ellesmere Island in northern Canada, only 880 km from the north  
pole!)", c("With the recent popularity of using bumblebees in glasshouse 
pollination they will probably be found in most parts of the world before, 
especially Bombus terrestris which seems to be the most popular species sold 
for this purpose. Recently there have been proposals to introduce bumblebees 
into Australia to pollinate crops in glasshouses. Now, though I dearly love 
bumblebees, I do think that this might not be a very good idea. No matter 
what security measures are taken, mated queens WILL escape eventually and 
that will probably lead to their establishment in the wild. And yet another 
non-native invasion of a country that has suffered more than most from such 
things. This invasion may or may not be benign, but isn't it better to err 
on the side of caution? Apparently there are already colonies of Bombus 
terrestris on Tasmania, so I suppose it is now only a matter of time before 
they reach the mainland.")))

# is the pattern appearing?
str_detect(text, "With")
str_detect(text, "s{2}") # two letters after each other appearing?

# where is the pattern appearing?
str_locate(text, "s{2}.{1}o") # detects "glasshouse"
str_locate(text, "ss.{1}o")

# what exactly is matched?
str_match(text, "s{1,2}.{1}o")
str_match(text, "glasshouse.{1,40}Now")

text2 <- c(c("here and there, but it could also be there and here. \n where? here, maybe it could aso be there."), c("test"))
str_match(text2, "here.{1,40}there") # gives you the line of the first occurrence
str_match_all(text2, "here.{1,40}there") # gives you the line of  all occurrences

# match words in both way
str_match_all(text2, "here.{1,40}there|there{1,40}here")


x <- c("species 1 (*), species 2 (**), c, d, e")
y <- str_split(x, ",")
y <- y[[1]]
str_split("species 1 (*)", "\\(")
#TODO escape character
data.table::data.table(first_column = y)
