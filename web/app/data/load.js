// @flow

import {map, flatten} from 'lodash'
import {Promise} from 'es6-promise'
import React from 'react'

type Route = {
  handler: {
    load:Function;
  }
}

export function loadAll(routes:Array<Route>, params:any, onData:(data:any)=>void) {
  var data = {loaded: false};

  routes
    .filter(route => route.handler.load)
    .forEach(function(route) {

      // ok, they're allowed to do more than one, right?
      var promises = route.handler.load(params)

      return map(promises, function(promise, name) {

        if (!promise.then) {
          // it isn't a promise, it's a value
          // resolve it
          promise = Promise.resolve(promise)
        }

        return promise.then(function(d) {
          data[name] = d
          data.loaded = true
          onData(data)
        }, throwError)
      })
    })
}

function throwError(err) {
  throw err
}

// store the last one :)
var lastHandler:any
var lastState:any
var lastData:any
var innerRender:any

function nothing() {}

export function run(ren:Function, onUrlChange:Function = nothing):Function {

  innerRender = ren

  return function(Handler, state) {
    lastHandler = Handler
    lastState = state
    lastData = {loaded: false}

    onUrlChange(Handler, state)

    // render once without any data
    render()

    // render again every time any of the promises resolve
    loadAll(state.routes, state.params, render)
  }
}

export function render(data:any = lastData) {
  lastData = data
  var Handler = lastHandler
  var state = lastState
  innerRender(Handler, state, data)
}

// global reload
export function reloadHandler() {
  loadAll(lastState.routes, lastState.params, render)
}
