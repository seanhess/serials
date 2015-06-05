// @flow

import {curry, dropWhile, tail, find} from 'lodash'

import {Chapter, isLink} from '../model/chapter'
import {SubChapter} from '../model/subscription'

export type ChapterAndRead = {
  chapter: Chapter;
  read: boolean;
}

export var toChapterAndRead = curry(function(subs:?{[id:string]:SubChapter}, chapter:Chapter):ChapterAndRead {

  var isRead:boolean = false

  if (subs && subs[chapter.id] && subs[chapter.id].read) {
    isRead = true
  }

  return {
    chapter: chapter,
    read: isRead
  }
})

export function isUnread(c:ChapterAndRead) {
  return isLink(c.chapter) && !c.read
}

export function isRead(c:ChapterAndRead) {
  return isLink(c.chapter) && c.read
}

export function lastRead(chapters:Array<ChapterAndRead>):?ChapterAndRead {
  return chapters.reduce(function(last, current) {
    if (current.read) {
      return current
    }
    return last
  }, null)
}

export function afterLastRead(cs:Array<ChapterAndRead>, last:ChapterAndRead):Array<ChapterAndRead> {
   return tail(dropWhile(cs, function(c) {
    return c != last
  }))
}

export function findBookmark(cs:Array<ChapterAndRead>):?ChapterAndRead {
  var last = lastRead(cs)
  if (!last) return nextLink(cs)
  var unread = afterLastRead(cs, last)
  return nextLink(unread)
}

export function nextLink(cs:Array<ChapterAndRead>):?ChapterAndRead {
  return find(cs, function(c:ChapterAndRead) {
    return isLink(c.chapter)
  })
}
