NEXT
- √ switch to stack and ghc 7.10
- √ switch to rethinkdb 2.0
- get it all working on the real server
- admin can see proposed books and check them off as lookin' good
- password reset
- use fanfiction as the source for that draco book
- add a few discovery features

Bugs
----
- [x] Root domain redirects to library, instead of bookshelf
- [x] chapter updates point to localhost
- [x] big covers in form and gallery?
- [x] Security: put secret key auth into an environment variable
- [ ] Scan health: when they add new books, they don't have a last scan date.

Milestone: Better Library
-------------------------
- [x] Users can edit books
- [x] Users can add  books
- [ ] Admin audit queue / full change history
- [ ] Password reset

- [ ] Search by author or title
- [ ] Search by tag
- [ ] Track total number of subscribers
- [ ] Show more in the library: subscribed
- [ ] Sort by # subscribers

Tweaks
------
- [x] better top bar. Menu button? Slide out?

- [ ] switch to fixed-height cover overlays

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
- [ ] Icon for new chapters. (How do I define new? It should be per-user. Whether they've "seen" them or not? Within the last week? Deviation from the rest of the chapters?)
- [ ] Library: show progress in the gallery. Show badges for ... new chapters? Or unread?

Feedback
------------
- hire a designer
- weekly email newsletter of books?
- automatically distribute content to kindle
- confusion about the "subscription" idea. Why do that? Landing page wording unclear
- change homepage: just be a list of stories? Get people right in, instead of al anding page.
- simpler submissions (advanced, maybe?)
- iframe (like stumbleupon)


Milestone: Feedback and Discovery
---------------------------------
- [x] add a good TOS
- [x] keep tracking down authors. Post on /r/webfiction. Ask for feedback specifically from authors. Ask for feedback specifically from authors.

Milestone: MVP
-------------------
- [x] bookmark not showing when it hits a title
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
- [x] can't see books if not logged in

ART PERMISSION
-----------------
- [x] YES 3 worlds collide.
- [x] YES Friendship is optimal. Gave Permission. DA
- [x] YES Twisted Cogs
- [x] ??? Harry Potter and the Methods of Rationality
- [x] YES Mother of Learning
- [x] NO  DA. Pact
- [x] YES The Metropolitan Man

OPTIMIZATIONS
-------------
- [ ] http2 nginx proxy in front (FOR SPEEEED) SPDY support. In for android and ios safari! It's currently spinning
- [ ] ssl cert from StartSSL (see email). Hmm. maybe not. They don't support subdomains?

MIGRATIONS
----------
- [x] r.db('serials').table('sources').update({imageArtist: "", imageArtistUrl: "", authorUrl: ""})
- [x] r.db('serials').table('sources').update({hidden: false})
- [x] r.db('serials').table('invites').update({created: "2015-06-03T20:38:21.623Z"})
- [x] r.db('serials').table('subscriptions').update({subscribed: true})
- [ ] r.db('serials').table('sources').update({chapters:[]})

Communities
------------
- [ ] (370) https://www.reddit.com/r/webfiction/
- [ ] (2145) https://www.reddit.com/r/Parahumans/
- [ ] (4600) http://www.reddit.com/r/writinghub
- [ ] http://www.reddit.com/r/webfiction/comments/167rcd/where_list_read_serials_webfiction_webnovels_draft/
- [ ] https://www.reddit.com/r/writing/
- [ ] Webfictionguide.com
