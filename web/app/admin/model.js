// @flow

import {get, post, put, del, url} from '../api'

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
    return get(url('sources'))
  },

  find(id:string) {
    return get(url('sources', id))
  },

  create(source:Source) {
    // clear the id
    source.id = ""
    return post(url('sources'), source)
  },

  del(id:string) {
    return del(url('sources', id))
  },

  save(id:string, source:Source) {
    return put(url('sources', id), source)
  }
}

export var ScanModel = {
  findBySource(id:string) {
    return get(url('sources', id, 'scans'))
  }
}

export var ChapterModel = {
  findBySource(id:string) {
    return get(url('sources', id, 'chapters'))
  },

  importSource(id:string) {
    return post(url('sources', id, 'chapters'), {})
  },

  save(chapter:Chapter) {
    console.log("PUTTTTER THERE", chapter)
    return put(url('chapters', chapter.chapterId), chapter)
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

