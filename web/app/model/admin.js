// @flow

import {Get, Post, Put, Del, url} from '../api'


export function importLog(n:number) {
  return Get(url('admin', 'import-log', n))
  .then(log => log.text)
}

export function version() {
  return Get(url('version.txt'))
}

export function invitesAll() {
  return Get(url('invites'))
}

export function invitesAdd(email:string) {
  return Post(url('invites'), JSON.stringify(email))
}

