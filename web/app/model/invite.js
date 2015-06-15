// @flow
import {Get, Post, Put, Del, url} from '../api'
import {User} from './user'

export type Signup = {
  firstName:string;
  lastName:string;
  email:string;
  code:string;
  password:string;
  passwordConfirmation:string;
}

export type InviteSignup = {
  userId: string;
  date: Date;
}

export type Invite = {
  email: string;
  id: string;
  code: string;
  signup?: InviteSignup;
  sent?: string; // date
  created: string;
}

export function emptyInvite():Invite {
  return {
    email: "",
    id: "",
    code: "",
    created: "",
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

export function invitesDelete(code:string) {
  return Del(url('invites', code))
}

export function signup(signup:Signup):Promise<User> {
  return Post(url('users'), signup)
}

