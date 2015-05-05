// @flow

import {Get, Post, Put, Del, url} from '../api'

import {Source} from './source'

export type Subscription = {
  userId: string;
  sourceId: string;
}

export function userBooks(userId:string):Promise<Array<Source>> {
  return Get(url('users', userId, 'books'))
}

export function findSubscription(userId:string, sourceId:string):Promise<Subscription> {
  return Get(url('users', userId, 'subs', sourceId))
}

export function setSubscribed(userId:string, sourceId:string, subscribed:boolean):Promise<void> {
  var subUrl = url('users', userId, 'subs', sourceId)
  if (subscribed) {
    return Put(subUrl, "")
  }
  else {
    return Del(subUrl)
  }
}

// TODO start keeping track of local data here
// when you login, load all subscriptions and store them
// then when you add a subscription, add to both the local store and the sever?

// It's much easier to just load it from there server, right?
