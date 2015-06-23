// @flow

import {DefaultImageUrl} from './source'
import {Get, Post, Put, Del, url} from '../api'

type URL = string;
type Name = string;

export type Submission = {
  id: string;
  url: URL;
  title: string;
  author: Name;
  imageUrl: URL;
  imageArtist: Name;
  imageArtistUrl: URL;
}

export function emptySubmission():Submission {
  return {
    id: "",
    url: "",
    title: "",
    author: "",
    imageUrl: DefaultImageUrl,
    imageArtist: "",
    imageArtistUrl: ""
  }
}

export function findById(id:string):Promise<Submission> {
  return Get(url('submissions', id))
}

export function findAll():Promise<Array<Submission>> {
  return Get(url('submissions'))
}

export function create(sub:Submission) {
  return Post(url('submissions'), sub)
}

export function save(id:string, sub:Submission) {
  return Put(url('submissions', id), sub)
}

export function validate(sub:Submission):?string {
  if (!sub.url)           return "URL is required"
  if (!sub.title)         return "Title is required"
  if (!sub.author)        return "Author is required"
  if (!sub.imageUrl)      return "Image URL is required"
  if (!sub.imageArtist)   return "Artist is required"
  if (!sub.imageArtistUrl) return "Artist URL is required"
}
