// @flow

import {Get, Post, Put, Del, url} from '../api'


// SourceModel //////////////////////////////////////

export type SourceStatus = "Active" | "Disabled" | "Complete" | "Abandoned" | "Hidden";

export var Status = {
  Active: "Active",
  Disabled: "Disabled",
  Complete: "Complete",
  Abandoned: "Abandoned",
  Hidden: "Hidden",
  All: ([]: Array<SourceStatus>)
}

Status.All = [Status.Active, Status.Disabled, Status.Complete, Status.Abandoned, Status.Hidden]


export type Source = {
  id: string;
  importSettings: ImportSettings;
  name: string;
  author: string;
  url: string;
  imageUrl: string;
  imageMissingTitle: boolean;
  lastScan?: Scan;
  status: SourceStatus;
}

export type Scan = {
  date: string;
  total: number;
  new: Array<string>;
  updated: Array<string>;
}

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


export function notHidden(source:Source):boolean {
  return source.status !== Status.Hidden
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

  save(id:string, source:Source) {
    return Put(url('sources', id), source)
  }
}



export function emptySource():Source {
  return {
    id: "",
    name: "",
    author: "",
    url: "",
    imageUrl: "",
    status: Status.Active,
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

