# Twitter Search Scraping
This is an efficient Twitter search results scraper.  It will scrape tweet data for a given search term across a span of time.  Data is output to ./output/ in .csv files.  The scraper will check for any previously collected data to prevent duplicate stored records and repeated effort.

## Build and Run
- `stack build`
- `stack exec scrape-twitter`
