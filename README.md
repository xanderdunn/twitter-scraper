[![Build Status](https://travis-ci.org/xanderdunn/twitter-scraper.svg?branch=master)](https://travis-ci.org/xanderdunn/twitter-scraper)

# Twitter Search Scraping
This is an efficient Twitter search results scraper.  It will scrape tweet data for companies across the year 2013.  Data is output to `./output/<TICKER SYMBOL OF COMPANY>/<SEARCH TERM>.csv`.  The scraper will check for any previously collected data to prevent duplicate stored records and repeated effort.

## Build and Run
- `stack build`
- `stack exec scrape-twitter`

## Quality
- Travis CI builds haddock documentation, runs unit tests, creates test coverage percentage, prints cyclomatic complexity, and fails build if ghc or hlint product any warnings
