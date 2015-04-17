serials
=========

Prototype of web serials app

Development
-----------

Haskell Server

    cabal sandbox init
    cabal install --only-dependencies
    cabal run

Front-end application
  
    npm install
    npm start

Approaches
----------

- hpmor: fanfiction menuLinks, hpmor.com tocLinks
- mother of learning: menuLinks
- fanfiction: menuLinks
- worm: tocLinks
- twig: tocLinks
- pact: tocLinks
- blindsight: ????
- patreon (https://www.patreon.com/dreamscapes?ty=c) ???

TODO
----

PARSING

- [x] CLOSED. filters valid links
- [x] clean title / url of newlines

SERVICE

- [x] Store settings per source
- [ ] Manually scan a source
  - button to kick it off
  - store the chapters
  - scan -> chapters -> store
  - display chapters
  - store the scan results itself
  - switch to a separate scan worker
- [ ] CRUD source chapters

PROTOTYPE USER INTERFACE

- [ ] List of sources in the system
- [ ] List of chapters per source
- [ ] Admin: Add a new source
- [ ] Admin: Edit course selector, settings, url, etc
- [ ] Admin: scan history
- [ ] Admin: manually add a chapter (in order)
- [ ] Admin: manually hide a scanned chapter

MILESTONES
----------

- [ ] List of sources. View, Edit, Remove, Add. 
- [ ] Button to do a scan per source. Store the results. Page to view the scan results
- [ ] Store the latest chapter list
- [ ] Manage chapter lists (hide, add chapter)
