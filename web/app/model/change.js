// @flow

import {Source, emptySource, ImportSettings} from './source'
import {User, emptyUser} from './user'
import {Chapter, contentText, chapterContentURL, emptyChapter} from './chapter'
import {Get, Post, Put, Del, url} from '../api'
import {remove, isElement, uniq, isEqual} from 'lodash'

export type Change = {
  id: string;
  baseId: string;
  source: Source;
  createdAt: string;
  createdBy: User;
}

export function emptyChange():Change {
  return {
    id: "",
    baseId: "",
    source: emptySource(),
    createdAt: (new Date()).toString(),
    createdBy: emptyUser()
  }
}

export function findChanges(sourceId:string):Promise<Array<Change>> {
  return Get(url('sources', sourceId, 'changes'))
}

export function findChange(changeId:string):Promise<Change> {
  return Get(changeUrl(changeId))
}

export function changeUrl(changeId:string):string {
  return url('changes', changeId)
}

//////////////////////////////////////////////////////////////


// the two nested fields are chapters, and import settings

export function diffBasic(from:Source, to:Source):Array<FieldChange> {
  // or I could just make one that is nested, right?
  // no because chapters need to be handled completely differently
  var basicFields = Object.keys(to).filter(function(key) {
    return key !== "chapters" && key !== "importSettings" && key !== "changeId" && key !== "lastScan"
  })

  return diffFields(from, to, basicFields)
}

export function diffImport(from:ImportSettings, to:ImportSettings):Array<FieldChange> {
  return diffFields(from, to, Object.keys(to))
}

export function diffChapter(from:?Chapter, to:?Chapter):Array<FieldChange> {
  var fromContent = from && from.content
  var toContent = to && to.content

  return diffFields(fromContent, toContent, Object.keys(toContent || fromContent || {}))
}

export type ChapterDiff = {
  id: string;
  hidden: boolean;
  index: number;
  text: string;
  url: ?string;
}

function toChapterDiff(chapter:Chapter, index:number):ChapterDiff {
  return {
    id: chapter.id,
    hidden: chapter.hidden,
    index: index,
    text: contentText(chapter),
    url: chapterContentURL(chapter),
  }
}

export function emptyChapterDiff():ChapterDiff {
  return toChapterDiff(emptyChapter(), 0)
}

export function diffChapters(froms:Array<Chapter>, tos:Array<Chapter>):Array<FieldChange> {

  // here the field means the chapter id
  var fromMap = froms.map(toChapterDiff).reduce(idMap, {})
  var toMap = tos.map(toChapterDiff).reduce(idMap, {})

  var fromIds = froms.map(id)
  var toIds = tos.map(id)
  var allIds = uniq(fromIds.concat(toIds))

  var updates = diffFields(fromMap, toMap, allIds)

  // TODO also detect changing the order of stuff
  // see if the order is different

  return updates
}

export type ChangeType = "deleted" | "updated" | "new" | "unchanged";

export type FieldChange<T> = {
  field: string;
  change: ChangeType;
  oldValue: T;
  newValue: T;
}

function diffFields<T>(from:?Object, to:?Object, keys:Array<string>):Array<FieldChange> {
  return keys
  .map(k => diffField(from, to, k))
  // .reduce(values, [])
}

function diffField(from:?Object, to:?Object, key:string):FieldChange {

  var fromValue = from && from[key]
  var toValue = to && to[key]
  var change:ChangeType = "unchanged"

  if (fromValue && !toValue) {
    change = "deleted"
  }
  else if (!fromValue && toValue) {
    change = "new"
  }
  else if (!isEqual(fromValue, toValue)) {
    change = "updated"
  }

  return {
    field: key,
    change: change,
    oldValue: fromValue,
    newValue: toValue
  }
}

function values<T>(vs:Array<T>, v:?T):Array<T> {
  if (v) return vs.concat([v])
  return vs
}

function idMap(map:{[key:string]:any}, value:{id:string}):{[key:string]:any} {
  map[value.id] = value
  return map
}

function id(item:{id:string}):string {
  return item.id
}

 //ok, diff them intelligently. Do I really want to make them generic?
 //no, do it by hand!
//export function diff(from:Object, to:Object):Array<FieldChange> {
   //compare: name vs name, etc
  //return Object.keys(from).filter(function(key) {
    //return from[key] && from[key] != to[key]
  //})
  //.map(function(key) {
  //})
//}


