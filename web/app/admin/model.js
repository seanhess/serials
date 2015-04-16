// @flow

import {get, post, put, del, url} from '../api'

export var SourceModel = {
  findAll:  () => get(url('/sources')),
  find:   (id) => get(url('/sources', id)),
  create: (source) => post(url('/sources'), source),
  del: (id) => del(url('/sources', id)),
  save:  (id, source) => put(url('/sources', id), source)
}

