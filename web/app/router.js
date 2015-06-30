// @flow

import React from 'react'
import Router from 'react-router'
import {Route, DefaultRoute, RouteHandler, NotFoundRoute, Redirect, Link} from 'react-router'
import {run} from './data/load'

var router;
var currentQuery:Object = {};

export type UrlChange = (Handler:any, state:any) => void

export function start(routes:ReactElement, render:Function, onUrlChange:UrlChange) {
  router = Router.run(routes, run(render, function(Handler, state) {
    // save the current query
    currentQuery = state.query

    onUrlChange(Handler, state)
  }))
}

export function transitionTo(route:string, params?:Object, query?:Object) {
  if (router) {
    router.transitionTo(route, params, query)
  }
}

export function query():Object {
  return currentQuery
}

export var Routes = {
  root: "root",
  bookshelf: "bookshelf",
  library: "library",
  sources: "sources",
  source: "source",
  users: "admin-users",
  invites: "admin-invites",
  signup: "signup",
  admin: "admin",
  about: "about",
  login: "login",
  book: "book",
  change: "change",
  changeFeed: "changeFeed",
  resetPassword: "resetPassword",
  forgotPassword: "forgotPassword",
}
