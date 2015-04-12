# serials
Prototype of serials app

Keep it simple:

* [ ] web prototype with local data
* [ ] haskell scripts to parse websites


USEFUL CONCURRENCY

- https://wiki.haskell.org/Control-Engine#Web_Crawling
- https://wiki.haskell.org/Haskell_for_multicores#Message_passing_channels
- http://chimera.labs.oreilly.com/books/1230000000929/ch07.html
- [Async Pool](https://hackage.haskell.org/package/async-pool)
- [Async](https://hackage.haskell.org/package/async)
- [Monad Loops](https://hackage.haskell.org/package/monad-loops)


- [WReq](http://www.serpentine.com/wreq/tutorial.html)
- Learn about: STM


Approaches
----------

- hpmor: fanfiction
- mother of learning: fanfiction
- fanfiction: fanfiction
- worm: rss
- twig: rss
- pact: rss
- blindsight

https://hackage.haskell.org/package/feed-0.3.9.2/docs/Text-Feed-Constructor.html
 feedFromXML?


How to Handle twig
------------------

the rss feeds won't work, because it doesn't give you the entire chapter index

- follow links and scrape for titles (Complete, works for other sites too). Would work for LOTS of sites. Gives a lot of garbage though. You'd have to ignore all the comment links, etc. 

- I need the thing that will cover the most ground in the future ...

- explicitly follow "Next" and "Previous" links, or something like that
  - you could tell it what to look for :)
  - WORM: a[title=Next Chapter], last paragraph in entry-content
  - TWIG: Previous, Next, ".entry-content > p a"
  - PACT: ".nav-links a", the other names are unreliable?

  - Fanfiction: Next (button with onClick, still, very parseable)

Give it the URL of the first chapter. Follow the next links until the end :)

- parse the options, follow the link to get the real URL. you still have to know the URL scheme for it. 

- MOST sites have some sort of table of contents. 
- I COULD just not follow the link. I mean, it should continue to work, especially if I repair the links every time. That's definitely easiest for now, right? And VERY similar to my other fanfiction work.


