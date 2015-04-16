// @flow

import axios from 'axios'
import path from 'path'

var API = "http://localhost:3001"

function data(res) {
  return res.data
}

function url(base, ...paths) {
  console.log("TEST", base, paths)
  return base+path.join(...paths)
}

export var SourceModel = {
  findAll:  () => axios.get(url(API,'/sources')).then(data),
  find:   (id) => axios.get(url(API, '/sources', id)).then(data),
  create: (source) => axios.post(url(API, '/sources'), source).then(data),
  delete: (id) => axios.delete(url(API, '/sources', id)).then(data),
  save:  (id, source) => axios.put(url(API, '/sources', id), source).then(data),
}

