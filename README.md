Web Fiction
=========

This app aims to be a podcast-like experience for reading serial publications on the web. 

Many excellent books and webcomics are being published openly on the web today. Most of those are published one chapter at a time. It's awesome that authors are self-publishing, but this format makes it hard to keep track of your progress.

The system will scan sources to create a table of contents for each book. It will notify subscribers, and keep track of their reading progress.

![Build Status](https://circleci.com/gh/seanhess/serials.svg?&style=shield&circle-token=5b00f3b0fd452b4027e442419d1a1ef381215f26)

Disclaimer
----------

Neither Orbital Labs or Web Fiction are affiliated with any of the authors, books, art or content. We currently use art under the Fair Use doctrine, but prior to launch we will only use art we have a clear license to.

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

* Install [RethinkDB 2](rethinkdb.com)
* Install [Haskell with Stack](https://github.com/commercialhaskell/stack)
* Install Node and `npm install -g webpack`

Run Database

    rethinkdb

Haskell Server

    # enter the REPL
    $ stack ghci

    # Run the application (Ctrl-C to exit)
    Main> mainApi

    # Reload your code
    Main> :r

    # Alternatively, build and run the executable
    $ stack build
    $ stack exec serials api


Front-end application

    $ npm install
    $ cs web && webpack

You'll probably want to make yourself an admin by hand. Once you've signed up, go to [http://localhost:8080](http://localhost:8080) and enter the following query into the Data Explorer

    r.db('serials').table('users').update({admin: true})

<!--
Hot Reloading
Visit the hot reload server (front-end changes will update in-place)
    http://localhost:3000/
-->

