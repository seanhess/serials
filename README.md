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
  
    cd web/
    npm install
    npm start

Visit the front-end server
  
    http://localhost:3000/

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
- [x] Manually scan a source

PROTOTYPE USER INTERFACE

- [x] List of sources in the system
- [x] List of chapters per source
- [x] Admin: Add a new source
- [x] Admin: Edit course selector, settings, url, etc
- [x] Admin: override the name, and don't wipe it out when you re-scan
- [x] Admin: manually hide a scanned chapter
- [x] User: see sources
- [x] User: see chapters for source
- [x] Source image
- [x] Fix: friendship is optimal has a bunch of extra links
- [x] Fix: HPMOR relative links
- [x] Fix: Worm missing titles
- [x] CSS: why are covers zoomed in on the book page?
- [ ] Deploy!
- [ ] Sign up to be notified of progress
- [ ] Suggest a book
- [ ] Fix: Wildbow arc titles

LATER

- [ ] Admin: manually add a chapter (in order)
- [ ] Admin: scan history
- [ ] Big: automatic scanning
- [ ] Big: Track where you are
- [ ] Big: notifications
- [ ] Big: better UI



MILESTONES
----------

- [ ] List of sources. View, Edit, Remove, Add. 
- [ ] Button to do a scan per source. Store the results. Page to view the scan results
- [ ] Store the latest chapter list
- [ ] Manage chapter lists (hide, add chapter)
