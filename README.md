[![Build Status](https://travis-ci.org/xanderdunn/twitter-scraper.svg?branch=master)](https://travis-ci.org/xanderdunn/twitter-scraper)

# Twitter Search Scraping
This is an efficient Twitter search results scraper.  It will scrape tweet data for a given search term across a span of time.  Data is output to ./output/ in .csv files.  The scraper will check for any previously collected data to prevent duplicate stored records and repeated effort.

## Build and Run
- `stack build`
- `stack exec scrape-twitter`

## Quality
- Travis CI builds haddock documentation, runs unit tests, creates test coverage percentage, prints cyclomatic complexity, and fails build if ghc or hlint product any warnings
