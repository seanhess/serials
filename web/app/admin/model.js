// @flow

import {get, post, put, del, url} from '../api'

type Source = {}

export var SourceModel = {
  findAll() {
    return get(url('sources'))
  },

  find(id:string) {
    return get(url('sources', id))
  },

  create(source:Source) {
    return post(url('sources'), source)
  },

  del(id:string) {
    return del(url('sources', id))
  },

  save(id:string, source:Source) {
    return put(url('sources', id), source)
  }
}

export var ScanModel = {
  findBySource(id:string) {
    return get(url('sources', id, 'scans'))
  }
}
