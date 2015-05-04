// @flow

import moment from 'moment'

declare var localStorage;

export var updateLocalStorage = function(key:string, data:?Object):void {
    return localStorage.setItem(key, JSON.stringify(data))
}


export var getLocalStorage = function(key:string):Object {
    return JSON.parse(localStorage.getItem(key))
}

export function toDateString(str:string):string {
  var date = moment(str)
  return date.calendar()
}

