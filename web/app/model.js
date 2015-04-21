// @flow

var {Get, Post, Put, Del, url} = require('./api')


// SourceModel //////////////////////////////////////

type Source = {
  id: string;
  importSettings: any;
  disabled: bool;
  name: string;
  url: string;
  imageUrl: string;
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


type Chapter = {
  id: string;
  url: string;
  edited: bool;
  name: string;
  link: Link;
  hidden: bool;
}

type Link = {
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

export var Menu = "MenuSettings"
export var TOC = "TOCSettings"

export function emptySource():Source {
  return {
    id: "",
    disabled: false,
    name: "",
    url: "",
    imageUrl: "",
    importSettings: {
      tag: TOC,
    }
  }
}

export function showChapter(chapter:Chapter):bool {
  return !chapter.hidden
}
