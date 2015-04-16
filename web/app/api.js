import axios from 'axios'
import path from 'path'

export var api = function(method, url, data) {
  return axios({
    method: method,
    url: url,
    data: data
  })
  .then(toData, error)
}

export var get  = (url) => api("get", url)
export var del  = (url) => api("delete", url)
export var post = (url, body) => api("post", url, body)
export var put  = (url, body) => api("put", url, body)

function toData(res) {
  return res.data
}

function error(err) {
  console.err(err)
}

// ------------------------------------------------

var API = "http://localhost:3001"

export function url(...paths) {
  return API+path.join(...paths)
}
