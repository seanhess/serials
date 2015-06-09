
Next Milestone: MVP
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
- [x] (small) login page doesn't auto complete
- [x] (small) login page zooms in when you select the field
- [x] User sign up date?
- [x] validate signup data (name and password length)
- [x] new screenshots with hpmor instead of twig
- [x] Remove wildbow properties
- [x] /hello and /app - REQUIRED. To be able to redirect, AND choose which one to go to
- [x] INDEX: redirect to either hello or the other app prior to loading everything. Redirect from server!
- [x] analytics: unique landing page hits, unique signups, unique complete, unique subscribe, unique read. % of users that do each one. Use Google Analytics! It's free!
- [x] Signup link from main app
- [x] Art attribution. Is that considered good form?
- [x] Find or commision real art
- [?] Art: the spaceships. I need something I can use.
- [x] Switch to invite-only beta
- [x] Jump to bookmark
- [-] Analytics: build funnel
- [x] Personally invite ryan and guys from thread. Who else?
- [x] Privacy policy and terms. Explain your intent to support. Add "Authors" section. Link from about page.
- [x] Sign ins not saving! Expired? Why?
- [x] List of users.
- [x] Stats about users. see their goods
- [x] Better emails and copy.

- [x] way to silence chapter updates when updating / purging.
- [x] BUG: pact chapters
- [x] Separate "hidden" from status, allow Ryan to subscribe while still making it hidden.
- [x] Clean up wildbow stuff and email him asking for review

BACKLOG
----
- [ ] version hard-coded into executable to make sure it's correct
- [ ] new domain name
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
- [ ] r.db('serials').table('sources').update({hidden: false})

FOLLOW UP
---------
- emailed eliezer
- commented to mahasim - http://mahasim.deviantart.com/art/Twig-Commission-527933451
- note to nkabuto - http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077

- http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077
