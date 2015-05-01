// @flow

import moment from 'moment'

export var updateLocalStorage = function(key, data) {
    return localStorage.setItem(key, JSON.stringify(data))
}


export var getLocalStorage = function(key) {
    return JSON.parse(localStorage.getItem(key))
}

export function toDateString(str:string):string {
  var date = moment(str)
  return date.calendar()
}

