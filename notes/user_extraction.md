
getting feed from page:

if there's a custom extractor for the url, use that, else
if there's a custom extractor for and app url in a meta tag, use that, else
if there's a rel="alternate" pointing to an rss or atom feed, use that, else
if there's a rel="alternate" on the domain's home page, use that, else 
if there's a name="twitter:site" meta tag, use that, else
if there's a property="fb:profile\_id" meta tag, use that, else
if there's a property="fb:pages" meta tag, use that, else
if the domain's robots.txt points to a sitemap, use that else
if domain/sitemap.xml returns a sitemap, use that else
give up


getting article author:

- custom extractor if there is one
- standard meta tags
- opengraph tags
- twitter card tags
- linked data json 
- DC.whatever dublin core meta tags
- citation\_ google scholar meta tags
- twitter:site

getting polling interval:

1. interval from rss/atom tags
2. Cache-Control 
3. Expires
4. every five minutes

getting author av:

- ld+json author.logo
- ld+json logo 
- ld+json publisher.logo 
- rel="apple-touch-icon"
- rel="icon"
- favicon.ico
- twitter:site

sites we need custom content extractors for:

- youtube
- medium 
- soundcloud
- linkedin
- facebook
- reddit

sites we need custom feed extractors for:

- twitter
- youtube
- soundcloud
- github 
- linkedin
- facebook
- reddit
