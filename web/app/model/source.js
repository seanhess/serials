// @flow

import {Get, Post, Put, Del, url} from '../api'
import {User} from './user'


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
}

export type Scan = {
  date: string;
  total: number;
  new: Array<string>;
  updated: Array<string>;
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

////////////////////////////////////////////////////////////

export type Change = {
  id: string;
  source: Source;
  kind: "Edit" | "Create";
  createdAt: string;
  createdBy: User;
}

export function findChanges(sourceId:string):Promise<Array<Change>> {
  return Get(url('sources', sourceId, 'changes'))
}

export function findChange(sourceId: string, changeId:string):Promise<Array<Change>> {
  return Get(url('sources', sourceId, 'changes', changeId))
}


//////////////////////////////////////////////////////////

export function isNotHidden(source:Source):boolean {
  return source.hidden !== true
}

export var SourceModel = {
  findAll() {
    return Get(url('sources'))
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
  }
}



export function emptySource():Source {
  return {
    id: "",
    name: "",
    author: "",
    authorUrl: "",
    hidden: false,
    url: "",
    imageUrl: "http://i.imgur.com/bMwt85W.jpg",
    imageArtist: null,
    imageArtistUrl: null,
    imageArtistAboutUrl: null,
    status: Status.Proposed,
    imageMissingTitle: false,
    importSettings: emptyImportSettings(TOC)
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
    tocSelector: "",
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

