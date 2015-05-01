// @flow

import {Get, Post, Put, Del, url} from '../api'
import {getLocalStorage} from '../helpers'


// UserModel //////////////////////////////////////


export type User = {
  id: string;
  firstName: string;
  lastName: string;
  email: string;
  token: string;
  created: date;
}


export var UserModel = {
  checkAuth() {
    var token = getLocalStorage('userToken')
    return Get(url('auth/current?token=' + token))
  },

  login(login) {
    return Post(url('login'), login)
  },

  signup(signup) {
    return Post(url('signup'), signup)
  },
}

