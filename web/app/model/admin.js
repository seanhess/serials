// @flow

import {Get, Post, Put, Del, url} from '../api'

export function importLog(n:number) {
  return Get(url('admin', 'import-log', n.toString()))
  .then(log => log.text)
}

export function version() {
  return Get(url('version.txt'))
}
