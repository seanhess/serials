
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
  console.error(err)
}

// ------------------------------------------------

var API_ENDPOINT = ""
if (window.location.host == "localhost:3000") {
  API_ENDPOINT = "http://localhost:3001"
}

export function url(...paths:Array<string>):string {
  // I need to join the API with the path
  return API_ENDPOINT+'/'+path.join(...paths)
}

// webpack can set this for us, can't it?
// but it depends on which version we're building...
// it defaults to ""
// but sometimes you can override it
