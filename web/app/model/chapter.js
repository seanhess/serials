// @flow

import {Get, Post, Put, Del, url} from '../api'
import shortid from 'shortid'
import {deepClone} from 'lodash'


// ChapterModel /////////////////////////////////////


export type Chapter = {
  id: string;
  added: Date;
  edited: bool;
  content: Content;
  hidden: bool;
}

export type Content = ContentLink | ContentTitle;

export type ContentLink = {
  tag: "Link";
  linkURL: string;
  linkText: string;
}

export type ContentTitle = {
  tag: "Title";
  titleText: string;
}

export function importSource(id:string) {
  return Post(url('sources', id, 'chapters'), {})
}

export function chapterContentURL(chapter:Chapter):string {
  var content:ContentLink = (chapter.content : any)
  return content.linkURL
}


export function emptyChapter(sourceId:string, link:Content = emptyLink()):Chapter {
  return {
    id: shortid.generate(),
    sourceId: sourceId,
    edited: false,
    name: "",
    number: 0,
    added: new Date(),
    content: link,
    hidden: false
  }
}

export function emptyLink():ContentLink {
  return {
    tag: "Link",
    linkURL: "",
    linkText: ""
  }
}

export function emptyTitle():ContentTitle {
  return {
    tag: "Title",
    titleText: ""
  }
}

export function showChapter(chapter:Chapter):bool {
  return !chapter.hidden
}

export function isLink(chapter:Chapter):bool {
  return (chapter.content.tag : any) == "Link"
}

export function proxyURL(remoteUrl:string):string {
  return url('proxy', encodeURIComponent(remoteUrl))
}

export function proxyContent(remoteUrl:string):Promise<string> {
  return Get(url('proxy', encodeURIComponent(remoteUrl)))
}

export function findChapter(id:string):Promise<Chapter> {
  return Get(url('chapters', id))
}

export function contentText(chapter:Chapter):string {
  if (chapter.content.tag === "Link") {
    var link:ContentLink = (chapter.content : any)
    return link.linkText
  }
  else {
    var title:ContentTitle = (chapter.content : any)
    return title.titleText
  }
}

export function setContentText(chapter:Chapter, text:string):void {
  if (chapter.content.tag === "Link") {
    var link:ContentLink = (chapter.content : any)
    link.linkText = text
  }
  else {
    var title:ContentTitle = (chapter.content : any)
    title.titleText = text
  }
}




