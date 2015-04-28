// @flow

import {Get, Post, Put, Del, url} from './api'
import moment from 'moment'

/////////////////////////////////////////////////////

export var AdminModel = {
  importLog(n:number) {
    return Get(url('admin', 'import-log', n))
    .then(log => log.text)
  }
}


// SourceModel //////////////////////////////////////

export type Source = {
  id: string;
  importSettings: any;
  disabled: bool;
  name: string;
  author: string;
  url: string;
  imageUrl: string;
  imageMissingTitle: boolean;
  lastScan?: Scan;
}

export type Scan = {
  date: string;
  total: number;
  new: Array<string>;
  updated: Array<string>;
}


export var SourceModel = {
  findAll() {
    return Get(url('sources'))
  },

  find(id:string) {
    return Get(url('sources', id))
  },

  create(source:Source) {
    // clear the id
    source.id = ""
    return Post(url('sources'), source)
  },

  del(id:string) {
    return Del(url('sources', id))
  },

  save(id:string, source:Source) {
    return Put(url('sources', id), source)
  }
}





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

/////////////////////////////////////////////////////////

export var Menu = "MenuSettings"
export var TOC = "TOCSettings"

export function emptySource():Source {
  return {
    id: "",
    disabled: false,
    name: "",
    author: "",
    url: "",
    imageUrl: "",
    imageMissingTitle: false,
    importSettings: {
      tag: TOC,
    }
  }
}

export function emptyScan():Scan {
  return {
    date: "",
    new: [],
    updated: [],
    total: 0
  }
}

export function showChapter(chapter:Chapter):bool {
  return !chapter.hidden && (chapter.name.length > 0)
}


export function toDateString(str:string):string {
  var date = moment(str)
  return date.calendar()
}
