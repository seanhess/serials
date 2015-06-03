// @flow

import {Get, Post, Put, Del, url} from '../api'
import {Source} from './source'
import {cloneDeep} from 'lodash'

export type Subscription = {
  userId: string;
  sourceId: string;
  chapters: { [id:string]:SubChapter };
}

export type SubChapter = {
  chapterId: string;
  read: boolean;
}

export function newSubscription(userId: string, sourceId: string):Subscription {
  return {userId, sourceId, chapters: {}}
}

export function userBooks(userId:string):Promise<Array<Source>> {
  return Get(url('users', userId, 'books'))
}

export function findSubscription(userId:string, sourceId:string):Promise<Subscription> {
  return Get(url('users', userId, 'subs', sourceId))
}

export function saveSubscription(sub:Subscription):Promise<void> {
  return Put(url('users', sub.userId, 'subs', sub.sourceId), sub)
}

// maybe I should have it say "subscribed: false" and always return one
export function setSubscribed(userId:string, sourceId:string, subscribed:boolean):Promise<void> { 
  var subUrl = url('users', userId, 'subs', sourceId)
  if (subscribed) {
    return Post(subUrl, "")
  }
  else {
    return Del(subUrl)
  }
}

export function markAsRead(subscription:Subscription, chapterId:string, read:boolean):Subscription {
  var sub = cloneDeep(subscription)

  sub.chapters[chapterId] = {
    chapterId: chapterId,
    read: read
  }

  return sub
}

// TODO start keeping track of local data here
// when you login, load all subscriptions and store them
// then when you add a subscription, add to both the local store and the sever?

// It's much easier to just load it from there server, right?
