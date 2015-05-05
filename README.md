serials
=========

Serials aims to be a podcast-like experience for reading serial publications on the web. 

Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format makes it hard to keep track of your progress. 

The system will scan sources to create a table of contents for each book. It will notify subscribers, and keep track of their reading progress.

![Build Status](https://circleci.com/gh/seanhess/serials.svg?&style=shield&circle-token=5b00f3b0fd452b4027e442419d1a1ef381215f26)

Pre-alpha Site
--------------

http://serials.orbit.al

MILESTONES
----------

- [x] List of sources. View, Edit, Remove, Add. 
- [x] Button to do a scan per source. Store the results. Page to view the scan results
- [x] Store the latest chapter list
- [x] Manage chapter lists (hide, add chapter)
- [x] Automatic scanning
- [x] User accounts
- [x] Responsive front-end reading experience
- [ ] Notifications
- [ ] Keep track of where you are / queue
- [ ] Discovery
- [ ] Launch / optimization / etc

Contributing
------------

Let's talk! Contact me, submit an issue or PR with your proposed changes, or pick up an issue already logged.

* [Issues and Features that need work](https://github.com/seanhess/serials/issues)


Development
-----------

* Install [RethinkDB](rethinkdb.com)
* Install GHC 7.8 and Cabal 1.22
* Install Node and `npm install webpack`

Run Database

    rethinkdb

Haskell Server

    cabal sandbox init
    cabal install --only-dependencies
    cabal run

Front-end application

    npm start

Visit the hot reload server (front-end changes will update in-place)

    http://localhost:3000/

