// @flow

import {map, flatten} from 'lodash'
import Promise from 'bluebird'

type Route = {
  handler: {
    load:Function;
  }
}

export function loadAll(routes:Array<Route>, params:any, onData:(data:any)=>void) {
  var data = {};

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
          onData(data)
        }, throwError)
      })
    })
}

function throwError(err) {
  throw err
}
