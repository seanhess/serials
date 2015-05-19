// @flow
import {Get, Post, Put, Del, url} from '../api'

export type Signup = {
  firstName:string;
  lastName:string;
  email:string;
  code:string;
  password:string;
  passwordConfirmation:string;
}

export type Invite = {
  email: string;
  id: string;
  code: string;
  userId?: string;
  sent?: string; // date
}

export function emptyInvite():Invite {
  return {
    email: "",
    id: "",
    code: "",
  }
}

export function invitesAll() {
  return Get(url('invites'))
}

export function invitesAdd(email:string) {
  return Post(url('invites'), JSON.stringify(email))
}

export function invitesFind(code:string) {
  console.log("FIND", code)
  return Get(url('invites', code))
}

export function invitesSend(code:string) {
  return Post(url('invites', code, 'sent'))
}

export function signup(signup:Signup) {
  return Post(url('users'), signup)
}

