// @flow

import {Get, Post, Put, Del, url} from '../api'
import shortid from 'shortid'


// ChapterModel /////////////////////////////////////


export type Chapter = {
  id: string;
  sourceId: string;
  added: Date;
  number: number;
  edited: bool;
  content: Content;
  hidden: bool;
}

export type Content = ContentLink | ContentTitle;

export type ContentLink = {
  tag: "Link";
  linkURL: string;
  linkText: string;
}

export type ContentTitle = {
  tag: "Title";
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

  delete(id:string) {
    return Del(url('chapters', id))
  },

  deleteBySource(id:string) {
    return Del(url('sources', id, 'chapters'))
  }
}

export function chapterContentURL(chapter:Chapter):string {
  var content:ContentLink = (chapter.content : any)
  return content.linkURL
}


export function emptyChapter(sourceId:string, link:Content = emptyLink()):Chapter {
  return {
    id: shortid.generate(),
    sourceId: sourceId,
    edited: false,
    name: "",
    number: 0,
    added: new Date(),
    content: link,
    hidden: false
  }
}

export function emptyLink():ContentLink {
  return {
    tag: "Link",
    linkURL: "",
    linkText: ""
  }
}

export function emptyTitle():ContentTitle {
  return {
    tag: "Title",
    titleText: ""
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

export function proxyContent(remoteUrl:string):Promise<string> {
  return Get(url('proxy', encodeURIComponent(remoteUrl)))
}

export function findChapter(id:string):Promise<Chapter> {
  return Get(url('chapters', id))
}
