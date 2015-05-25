// @flow

import Promise from 'bluebird'

import {Get, Post, Put, Delete, url} from '../api'
import {EventEmitter} from 'events'
import {Subscription, findSubscription} from './subscription'

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
    this.currentUser = SETTINGS.user
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

  update(user:User) {
    return Put(url(`users/${user.id}`), user)
    .then(user => user)
  }

  isLoggedIn():boolean {
    return !!this.currentUser
  }

  currentUserId():string {
    if (!this.currentUser) return ""
    return this.currentUser.id
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
  return findSubscription(Users.currentUserId(), sourceId)
}

export function userApiURL(id:?string) {
  if (!id) return ""
  return url('users', id)
}

