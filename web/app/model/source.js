// @flow

import {Get, Post, Put, Del, url} from '../api'
import {Chapter} from './chapter'


// SourceModel //////////////////////////////////////

export type SourceStatus = "Active" | "Disabled" | "Complete" | "Abandoned" | "Proposed";

export var Status = {
  Active: "Active",
  Disabled: "Disabled",
  Complete: "Complete",
  Abandoned: "Abandoned",
  Proposed: "Proposed",
  All: ([]: Array<SourceStatus>)
}

Status.All = [Status.Active, Status.Disabled, Status.Complete, Status.Abandoned, Status.Proposed]

export type Source = {
  id: string;
  importSettings: ImportSettings;
  name: string;
  tags: Array<string>;
  author: string;
  authorUrl: string;
  hidden: boolean;
  url: string;
  imageUrl: string;
  imageMissingTitle: boolean;
  imageArtist: ?string;
  imageArtistUrl: ?string;
  imageArtistAboutUrl: ?string;
  lastScan?: Scan;
  status: SourceStatus;
  chapters: Array<Chapter>;
}

export type Scan = {
  date: string;
  total: number;
  new: Array<string>;
  updated: Array<string>;
}

export type ScanResult = {
  scan: Scan;
  allChapters: Array<Chapter>;
}

////////////////////////////////////////////////////////////////

export type ImportSettings = MenuSettings | TOCSettings;
type URL = string;

export var Menu = "MenuSettings"
export var TOC = "TOCSettings"

export type MenuSettings = {
  tag: "MenuSettings";
  menuBase: URL;
  menuOpen: string;
}

export type TOCSettings = {
  tag: "TOCSettings";
  tocSelector: string;
  titleSelector: string;
}

/////////////////////////////////////////////////////////

export function scan(source:Source):Promise<ScanResult> {
  return Post(url('sources', 'scan'), source)
}

//////////////////////////////////////////////////////////

export type TagCount = {
  tagName: string;
  tagCount: number;
}

export function allTags():Promise<Array<TagCount>> {
  return Get(url('tags'))
}

//////////////////////////////////////////////////////////

export function isNotHidden(source:Source):boolean {
  return source.hidden !== true
}

export var SourceModel = {
  findAll(tag?:string) {
    var u = url('sources')
    if (tag) u += "?tag="+tag
    return Get(u)
  },

  findRecommended():Promise<Array<Source>> {
    return this.findAll().then(ss => ss.filter(isRecommended))
  },

  find(id:string) {
    return Get(url('sources', id))
  },

  create(source:Source) {
    // clear the id
    source.id = ""
    return Post(url('sources'), source)
  },

  save(id:string, source:Source) {
    return Put(url('sources', id), source)
  },

  delete(id:string) {
    return Del(url('sources', id))
  }
}



export function emptySource(imageUrl:string = ""):Source {
  return {
    id: "",
    name: "",
    author: "",
    authorUrl: "",
    hidden: false,
    tags: [],
    url: "",
    imageUrl: imageUrl,
    imageArtist: null,
    imageArtistUrl: null,
    imageArtistAboutUrl: null,
    status: Status.Proposed,
    imageMissingTitle: false,
    importSettings: emptyImportSettings(TOC),
    chapters: []
  }
}

export function emptyImportSettings(type:string):ImportSettings {
  if (type == Menu) {
    return emptyMenuSettings()
  }
  else {
    return emptyTOCSettings()
  }
}

function emptyTOCSettings():TOCSettings {
  return {
    tag: TOC,
    tocSelector: "body",
    titleSelector: ""
  }
}

function emptyMenuSettings():MenuSettings {
  return {
    tag: Menu,
    menuBase: "",
    menuOpen: "",
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

export function isRecommended(source:Source):boolean {
  return source.name === "Friendship is Optimal" ||
         source.name === "Harry Potter and the Methods of Rationality"
}

export function isSearch(term:string):(source:Source) => boolean {
  var search = new RegExp(term.toLowerCase())
  return function(source:Source):boolean {
    return search.test(source.name.toLowerCase()) || search.test(source.author.toLowerCase())
  }
}

export var DefaultImageUrl = "http://i.imgur.com/bMwt85W.jpg"

export function defaultImageUrl(value:string):string {
  return value || DefaultImageUrl
}


export function validate(source:Source):?string {
  if (!source.name)         return "Title is required"
  if (!source.author)        return "Author is required"
  if (!source.authorUrl)        return "Author URL is required"
  if (!source.imageUrl)      return "Image URL is required"
  //if (!source.imageArtist)   return "Artist is required"
  //if (!source.imageArtistUrl) return "Artist URL is required"
  if (!source.url)           return "Table of Contents URL is required"

  //if (!source.chapters.length) return "Scan Chapters before saving"

  //if (source.importSettings.tag === "MenuSettings") {
    //var menuSettings:MenuSettings = (source.importSettings : any)
    //if (!menuSettings.menuBase) return "Base URL is required"
    //if (!menuSettings.menuOpen) return "Open Selector is required"
  //}

  //else {
    //var toc:TOCSettings = (source.importSettings : any)
    //if (!toc.tocSelector) return "Root Selector is required"
  //}
}
