
var axios = require('axios')
var path:any = require('path')
import {getLocalStorage} from './helpers'

export var api = function(method:string, url:string, data?:Object) {
  var config = {
    method: method,
    url: url,
    data: data,
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

export function Post(url:string, body:Object) {
  return api("post", url, body)
}

export function Put(url:string, body:Object) {
  return api("put", url, body)
}

function toData(res) {
  return res.data
}

function error(err) {
  console.error(err)
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
