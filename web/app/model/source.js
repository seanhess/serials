// @flow

import {Get, Post, Put, Del, url} from '../api'


// SourceModel //////////////////////////////////////

export type SourceStatus = "Active" | "Disabled" | "Complete" | "Abandoned";

export var Status = {
  Active: "Active",
  Disabled: "Disabled",
  Complete: "Complete",
  Abandoned: "Abandoned",
  All: ([]: Array<SourceStatus>)
}

Status.All = [Status.Active, Status.Disabled, Status.Complete, Status.Abandoned]


export type Source = {
  id: string;
  importSettings: any;
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


export var Menu = "MenuSettings"
export var TOC = "TOCSettings"


export function emptySource():Source {
  return {
    id: "",
    name: "",
    author: "",
    url: "",
    imageUrl: "",
    status: Status.Active,
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

