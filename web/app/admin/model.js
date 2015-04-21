// @flow

//import {get, Post, put, del, url} from '../api'
var {Post, Put, Del, url, Get} = require('../api')

type Source = {
  id: string;
  importSettings: any;
  disabled: bool;
  name: string;
  url: string;
}

type Chapter = {
  chapterId: string;
  url: string;
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

export var ScanModel = {
  findBySource(id:string) {
    return Get(url('sources', id, 'scans'))
  }
}

export var ChapterModel = {
  findBySource(id:string) {
    return Get(url('sources', id, 'chapters'))
  },

  importSource(id:string) {
    return Post(url('sources', id, 'chapters'), {})
  },

  save(chapter:Chapter) {
    console.log("Put THERE", chapter)
    return Put(url('chapters', chapter.chapterId), chapter)
  }
}

export var Menu = "MenuSettings"
export var TOC = "TOCSettings"

export function emptySource():Source {
  return {
    id: "",
    disabled: false,
    name: "",
    url: "",
    importSettings: {
      tag: TOC,
    }
  }
}

