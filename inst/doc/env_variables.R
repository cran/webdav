## ----echo=FALSE, results = 'asis'---------------------------------------------
# Load fortunes to add some wisdom
library(fortunes)
fortune_text <- capture.output(fortune(106))  # Capture fortune output
cat("> ", paste(fortune_text, collapse = "\n> "))  # Format as a quotation

## ----echo=FALSE, results = 'asis'---------------------------------------------
# Load fortunes to add some wisdom
library(fortunes)
fortune_text <- capture.output(fortune("password"))  # Capture fortune output
cat("> ", paste(fortune_text, collapse = "\n> "))  # Format as a quotation

