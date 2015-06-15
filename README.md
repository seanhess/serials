serials
=========

Serials aims to be a podcast-like experience for reading serial publications on the web. 

Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format makes it hard to keep track of your progress.

The system will scan sources to create a table of contents for each book. It will notify subscribers, and keep track of their reading progress.

![Build Status](https://circleci.com/gh/seanhess/serials.svg?&style=shield&circle-token=5b00f3b0fd452b4027e442419d1a1ef381215f26)

Disclaimer
----------

Neither Orbital Labs or Serials are affiliated with any of the authors, books, art or content. We currently use art under the Fair Use doctrine, but prior to launch we will only use art we have a clear license to.

Please contact [webfiction@orbit.al](mailto:webfiction@orbit.al) if we are using anything we don't have rights to and we'll clear it up

Early Access Site
-----------------

http://webfiction.co

MILESTONES
----------

- [x] Early Access Launch

Contributing
------------

Let's talk! Contact me, submit an issue or PR with your proposed changes, or pick up an issue already logged.

* [TODO](./doc/todo.md)

Development
-----------

* Install [RethinkDB 1.16](rethinkdb.com)
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

