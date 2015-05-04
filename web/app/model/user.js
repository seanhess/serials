// @flow

import Promise from 'bluebird'

import {Get, Post, Put, Del, url} from '../api'
import {getLocalStorage, updateLocalStorage} from '../helpers'

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

}

export type Signup = {

}


export var UserModel = {
  checkAuth() {
    var token = getLocalStorage('userToken')
    return Get(url('auth/current?token=' + token))
  },

  login(login:Login) {
    return Post(url('login'), login)
    .then((obj) => {
      updateLocalStorage('userToken', obj.token)
      return obj.user
    })
  },

  logout() {
    return new Promise((resolve, reject) => {
      updateLocalStorage('userToken', null)
      resolve()
    })
  },

  signup(signup:Signup) {
    return Post(url('signup'), signup)
  },
}

