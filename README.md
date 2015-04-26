serials
=========

We aim to create a podcast-like experience for reading serial publications on the web. 

There are many great books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format makes it hard to keep track of your progress. 

The system will scan sources to create a table of contents for each book. It will notify subscribers, and keep track of their reading progress.

Pre-alpha Site
--------------

http://serials.orbit.al

Contributing
------------

Let's talk! Contact me, submit an issue or PR with your proposed changes, or pick up an issue already logged. 

* [Issues and Features that need work](https://github.com/seanhess/serials/issues)

MILESTONES
----------

- [x] List of sources. View, Edit, Remove, Add. 
- [x] Button to do a scan per source. Store the results. Page to view the scan results
- [x] Store the latest chapter list
- [x] Manage chapter lists (hide, add chapter)
- [ ] Automatic scanning
- [ ] User accounts
- [ ] Responsive front-end reading experience
- [ ] Notifications
- [ ] Keep track of where you are / queue

Development
-----------

Haskell Server

    cabal sandbox init
    cabal install --only-dependencies
    cabal run

Front-end application
  
    cd web/
    webpack -w

Visit the server
  
    http://localhost:3001/

