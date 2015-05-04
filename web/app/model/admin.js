// @flow

import {Get, Post, Put, Del, url} from '../api'


export var AdminModel = {
  importLog(n:number) {
    return Get(url('admin', 'import-log', n))
    .then(log => log.text)
  }
}

