// @flow

import {Promise} from 'es6-promise'

import {Get, Post, Put, Delete, url} from '../api'
import {EventEmitter} from 'events'
import {Subscription, findSubscription} from './subscription'
import {settings} from './settings'

// UserModel //////////////////////////////////////


export type User = {
  id: string;
  firstName: string;
  lastName: string;
  email: string;
  token: string;
  created: Date;
}

export type Login = {
  email:string;
  password:string;
}

// methods for logging in and out
// also currently logged in state
declare var SETTINGS;

export class UserModel {

  currentUser: ?User;

  events: EventEmitter;

  constructor() {
    this.currentUser = settings().user
    this.events = new EventEmitter()
  }

  //// Auth ////////////////////////////////

  login(login:Login) {
    return Put(url('auth'), login)
    .then(user => this._updateAuth(user))
  }

  logout() {
    return Delete(url('auth'))
    .then(() => this._clearAuth())
    .then(u => this._updateAuth(u))
  }

  refresh() {
    return Get(url('auth'))
    .then(user => this._updateAuth(user))
  }

  isLoggedIn():boolean {
    return !!this.currentUser
  }

  currentUserId():string {
    if (!this.currentUser) return ""
    return this.currentUser.id
  }

  //// Admin ////////////////////////////////

  loadAll():Promise<Array<User>> {
    return Get(url('users'))
  }

  delete(id:string):Promise<void> {
    return Delete(url('users', id))
  }


  //// Changes //////////////////////////////
  bind(f:Function) {
    this.events.on('change', f)
  }

  _updateAuth(user:User):Promise<User> {
    this.currentUser = user
    this.events.emit('change', this)
    return user
  }

  _clearAuth():void {
    this.currentUser = null
  }

}


export var Users = new UserModel()

export function loadSubscription(sourceId:string):Promise<Subscription> {
  var userId = Users.currentUserId()
  if (!userId) return Promise.resolve()
  return findSubscription(Users.currentUserId(), sourceId)
}

export function userApiURL(id:?string):string {
  if (!id) return ""
  return url('users', id)
}


