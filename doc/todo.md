Bugs
----
- [x] Root domain redirects to library, instead of bookshelf
- [ ] chapter updates point to localhost
- [ ] big covers in form and gallery?

Milestone: Better Library
-------------------------
- [ ] Users can edit books
- [ ] Users can add  books
- [ ] Full change history for auditing
- [ ] Search by author or title
- [ ] Search by tag
- [ ] Track total number of subscribers
- [ ] Show more in the library: subscribed
- [ ] Sort by # subscribers

USER GENERATED LIBRARY
- [x] Any user can add books
- [x] Edit link -> any user can edit a book
- [ ] Store changes whenever source is saved
- [ ] Don't store a change if it is the same
- [ ] only admin can set hidden and or status
- [ ] View changes
- [ ] BUG: should do something even without a root selector
- [ ] BUG: validation when they don't put in a URL

Milestone: Feedback and Discovery
---------------------------------
- [x] add a good TOS
- [x] keep tracking down authors. Post on /r/webfiction. Ask for feedback specifically from authors. Ask for feedback specifically from authors.

Milestone: MVP
-------------------
- [x] health check for last import to add to monitor
- [x] alerts
- [x] signup flow
- [x] multiple entry points
- [x] CDN libraries (externals)
- [x] book chapters are way too slow (see pact)
- [x] Dedicated in-app about page. Not a landing page.
- [x] landing page should redirect to app if you are logged in
- [x] Mention early access on the landing page
- [x] BUG: signup process
- [x] BUG: alert clicks redirect you How do I get them clickable?
- [x] login page doesn't auto complete
- [x] login page zooms in when you select the field
- [x] User sign up date?
- [x] validate signup data (name and password length)
- [x] new screenshots with hpmor instead of twig
- [x] /hello and /app - REQUIRED. To be able to redirect, AND choose which one to go to
- [x] INDEX: redirect to either hello or the other app prior to loading everything. Redirect from server!
- [x] analytics: unique landing page hits, unique signups, unique complete, unique subscribe, unique read. % of users that do each one. Use Google Analytics! It's free!
- [x] Signup link from main app
- [x] Art attribution. Is that considered good form?
- [x] Find or commision real art
- [x] Art: the spaceships. I need something I can use.
- [x] Switch to invite-only beta
- [x] Jump to bookmark
- [x] Analytics: build funnel
- [x] Personally invite ryan and guys from thread. Who else?
- [x] Privacy policy and terms. Explain your intent to support. Add "Authors" section. Link from about page.
- [x] Sign ins not saving! Expired? Why?
- [x] List of users.
- [x] Stats about users. see their goods
- [x] Better emails and copy.
- [x] way to silence chapter updates when updating / purging.
- [x] BUG: pact chapters
- [x] Separate "hidden" from status, allow Ryan to subscribe while still making it hidden.
- [x] version hard-coded into executable to make sure it's correct
- [x] Make sure emails made it to karen and aaron
- [x] Ask permission for all art, and give notice
- [x] Automatically send email on signup for now. (until more people)
- [x] don't delete read/unread information on unsubscribe
- [x] new domain name: web fiction
- [x] follow up with the reddit guys
- [x] Wait for Eliezer to have time to give permission
- [x] Post on /r/rational, and /r/webfiction, asking for feedback and criticism

BUGS
----
- [x] pushing haskell errors doesn't fail!

REGRESSIONS
-----------
- [x] codes not being used up
- [x] Books all appear as "subscribed" at first
- [ ] can't see books if not logged in

ART PERMISSION
-----------------
- [x] TALKING 3 worlds collide.
- [x] Friendship is optimal. Gave Permission. DA
- [x] Twisted Cogs
- [x] Harry Potter and the Methods of Rationality
- [?] ASKED DA. Mother of Learning
- [?] ASKED DA. Pact
- [x] The Metropolitan Man

DOMAIN NAMES
------------
webfiction.co
webfiction.com ($3688)

AUTHOR PERMISSION
-----------------
Should I ask for permission from authors? They're as scared as the artists are.

BACKLOG
----
- [ ] Center on larger screens
- [ ] Only show "Updated <date>" if the book is still active
- [ ] Book chapter links should work on hover. Set the href and override?
- [ ] Sort gallery by chapters read per week
- [ ] Sort library by title?
- [ ] Update RethinkDB
- [ ] Update GHC
- [ ] Switch to stack
- [ ] Login not autofilling
- [ ] Beta invites: generate an invite code without an associated email address. Can sign up usinga ny email from there.

TWEAKS
------
- [x] bookmark not showing when it hits a title
- [ ] better top bar. Menu button? Slide out?

OPTIMIZATIONS
---------------------
- [ ] http2 nginx proxy in front (FOR SPEEEED) SPDY support. In for android and ios safari! It's currently spinning
- [ ] ssl cert from StartSSL (see email). Hmm. maybe not. They don't support subdomains?

FUTURE
-------
- [ ] Icon for new chapters. (How do I define new? It should be per-user. Whether they've "seen" them or not? Within the last week? Deviation from the rest of the chapters?)
- [ ] Library: show progress in the gallery. Show badges for ... new chapters? Or unread?

MIGRATIONS
----------
- [x] r.db('serials').table('sources').update({imageArtist: "", imageArtistUrl: "", authorUrl: ""})
- [x] r.db('serials').table('sources').update({hidden: false})
- [x] r.db('serials').table('invites').update({created: "2015-06-03T20:38:21.623Z"})
- [x] r.db('serials').table('subscriptions').update({subscribed: true})
- [ ] r.db('serials').table('sources').update({chapters:[]})

FOLLOW UP
---------
- emailed eliezer
- commented to mahasim - http://mahasim.deviantart.com/art/Twig-Commission-527933451
- note to nkabuto - http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077

- http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077


Communities
------------
- [ ] (370) https://www.reddit.com/r/webfiction/
- [ ] (2145) https://www.reddit.com/r/Parahumans/
- [ ] (4600) http://www.reddit.com/r/writinghub
- [ ] http://www.reddit.com/r/webfiction/comments/167rcd/where_list_read_serials_webfiction_webnovels_draft/
- [ ] https://www.reddit.com/r/writing/
- [ ] Webfictionguide.com
