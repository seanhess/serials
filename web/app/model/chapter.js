// @flow

import {Get, Post, Put, Del, url} from '../api'


// ChapterModel /////////////////////////////////////


export type Chapter = {
  id: string;
  url: string;
  edited: bool;
  name: string;
  link: Link;
  hidden: bool;
}

export type Link = {
  linkURL: string;
  linkTitle: string;
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
    chapter.url = chapter.link.linkURL
    chapter.name = chapter.link.linkTitle
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


export function showChapter(chapter:Chapter):bool {
  return !chapter.hidden && (chapter.name.length > 0)
}

