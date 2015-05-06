// @flow

import {Get, Post, Put, Del, url} from '../api'


// ChapterModel /////////////////////////////////////


export type Chapter = {
  id: string;
  edited: bool;
  name: string;
  content: ContentLink | ContentTitle;
  hidden: bool;
}

export type ContentLink = {
  tag: string;
  linkURL: string;
  linkText: string;
}

export type ContentTitle = {
  tag: string;
  titleText: string;
}

export var ChapterModel = {
  findBySource(id:string) {
    return Get(url('sources', id, 'chapters'))
  },

  importSource(id:string) {
    return Post(url('sources', id, 'chapters'), {})
  },

  save(chapter:Chapter) {
    chapter.edited = true
    return Put(url('chapters', chapter.id), chapter)
  },

  clear(chapter:Chapter) {
    chapter.edited = false
    return Put(url('chapters', chapter.id), chapter)
  },

  hidden(chapter:Chapter, hidden:bool) {
    chapter.hidden = hidden
    return Put(url('chapters', chapter.id), chapter)
  },

  deleteBySource(id:string) {
    return Del(url('sources', id, 'chapters'))
  },
}

export function emptyChapter():Chapter {
  return {
    id: "",
    edited: false,
    name: "",
    number: 0,
    content: {tag: "", titleText: ""},
    hidden: false
  }
}


export function showChapter(chapter:Chapter):bool {
  return !chapter.hidden
}

export function isLink(chapter:Chapter):bool {
  return chapter.content.tag == "Link"
}

export function proxyURL(remoteUrl:string):string {
  return url('proxy', encodeURIComponent(remoteUrl))
}

export function chapterProxyURL(chapter:Chapter):string {
  var content:ContentLink = (chapter.content : any)
  return proxyURL(content.linkURL)
}

export function findChapter(id:string):Promise<Chapter> {
  return Get(url('chapters', id))
}

