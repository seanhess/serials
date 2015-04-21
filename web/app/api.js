// @flow

var axios = require('axios')
var path:any = require('path')

export var api = function(method:string, url:string, data?:Object) {
  return axios({
    method: method,
    url: url,
    data: data
  })
  .then(toData, error)
}


export function Get(url:string) {
  return api("get", url)
}

export function Del(url:string) {
  return api("delete", url)
}

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
  console.err(err)
}

// ------------------------------------------------

var API = "http://localhost:3001"

export function url(...paths:Array<string>):string {
  // I need to join the API with the path
  return API+'/'+path.join(...paths)
}
