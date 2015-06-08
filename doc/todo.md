
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
- [ ] Sign ins not saving! Expired? Why?
- [ ] List of users.
- [ ] Stats about users. see their goods
- [ ] Better emails and copy.
      - find another site with a good beta invite process. simple?
      - email: you're invited! You can sign up immediately
      - email: you're on the list! We will let you know
      - email: you've signup up, hooray!
- [ ] Beta invites:
      - generate an invite code without needing an email
      - signups let you set email and verifies it?
generate an invite code without attaching an email. Allow them to change email during signup. Verify email address on signup? Why does it matter? Ability to paste an invite link to someone.

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
- [ ] Icon for new chapters

MIGRATIONS
----------
- [ ] `r.db('serials').table('sources').update({imageArtist: "", imageArtistUrl: "", authorUrl: ""})`

FOLLOW UP
---------
- emailed eliezer
- commented to mahasim - http://mahasim.deviantart.com/art/Twig-Commission-527933451
- note to nkabuto - http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077

- http://nkabuto.deviantart.com/art/Sci-fi-City-02-288260077
