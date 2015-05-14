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

export type Signup = {
  firstName:string;
  lastName:string;
  email:string;
  password:string;
  passwordConfirmation:string;
}

// methods for logging in and out
// also currently logged in state
export class UserModel {

  currentUser: ?User;

  hasAuth:boolean;

  events: EventEmitter;

  constructor() {
    this.currentUser = null
    this.hasAuth = false
    this.events = new EventEmitter()
  }

  //// Auth ////////////////////////////////
  auth():Promise<User> {
    if (this._auth) {
      return this._auth
    }
    this._auth = this._checkAuth()
    return this._auth
  }

  login(login:Login) {
    return Put(url('auth'), login)
    .then(obj  => obj.user)
    .then(user => this._updateAuth(user))
  }

  logout() {
    return Delete(url('auth'))
    .then(() => this._clearAuth())
    .then(u => this._updateAuth(u))
  }

  signup(signup:Signup) {
    return Post(url('signup'), signup)
    .then((obj) => {
      return obj.user
    })
    .then(u => this._updateAuth(u))
  }

  isLoggedIn():boolean {
    return !!this.currentUser
  }


  //// Changes //////////////////////////////
  bind(f:Function) {
    this.events.on('change', f)
  }

  // private
  _auth: ?Promise<User>;

  _checkAuth() {
    console.log("CHECK AUTH: should only be called once")
    return Get(url('auth'))
    .then(u => this._updateAuth(u))
  }

  _updateAuth(user:User):Promise<User> {
    this.currentUser = user
    this.hasAuth = true
    this.events.emit('change', this)
    this._auth = Promise.resolve(user)
    return user
  }

  _clearAuth():void {
    this._auth = null
    this.currentUser = null
    this.hasAuth = true
  }

}


export var Users = new UserModel()

export function loadSubscription(sourceId:string):Promise<Subscription> {
  return Users.auth().then((user) => findSubscription(user.id, sourceId))
}


