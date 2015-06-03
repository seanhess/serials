// @flow
var axios = require('axios')
var path:any = require('path')
import {getLocalStorage} from './helpers'
import {Alerts} from './model/alert'
import {Promise} from 'es6-promise'

export type Body = Object | string

export var api = function(method:string, url:string, data?:Body) {
  var config = {
    method: method,
    url: url,
    data: data,
    headers: {}
  }

  config.headers = {
    "Content-Type": "application/json"
  }

  return axios(config)
  .then(toData, error)
}


export function Get(url:string) {
  return api("get", url)
}

export function Del(url:string) {
  return api("delete", url)
}

export var Delete = Del

export function Post(url:string, body?:Body) {
  return api("post", url, body)
}

export function Put(url:string, body?:Body) {
  return api("put", url, body)
}

function toData(res) {
  return res.data
}

function error(err) {
  console.error("API", err.status, err.statusText+":", err.data)

  
  if (err.status >= 500) {
    Alerts.update("error", "Something is broken. Please email support at serials@orbit.al and we'll take a look")
  }
  else {
    Alerts.update("error", "Error: " + err.data)
  }
  return Promise.resolve()
  .then(() => {throw err})
}


// ------------------------------------------------

// this could theoretically be configured by settings, but it might be easier to do
// with a proxy anyway
var API_ENDPOINT = ""

export function url(...paths:Array<string>):string {
  // I need to join the API with the path
  return API_ENDPOINT+'/'+path.join(...paths)
}

// webpack can set this for us, can't it?
// but it depends on which version we're building...
// it defaults to ""
// but sometimes you can override it
