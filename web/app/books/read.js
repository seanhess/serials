// @flow

import React from 'react'
import {chapterContentURL, proxyContent, proxyURL, findChapter} from '../model/chapter'
import {loadSubscription} from '../model/user'
import {saveSubscription, markAsRead} from '../model/subscription'

var MARK_READ_INTERVAL = 30*1000

export class Read extends React.Component {

  timer:any;

  static load(params) {
    // interesting... I need to do two in order
    var p = findChapter(params.id)

    return {
      chapter: p,
      subscription: p.then(c => loadSubscription(c.sourceId))
      //content: p.then(c => proxyContent(chapterContentURL(c)))
    }
  }

  componentWillReceiveProps(props:any) {
    if (props.loaded) {
      console.log("LOADED")
    }
  }

  markAsRead() {
    console.log("MARK AS READ")
    // need to wait for subscription.
    if (!this.props.subscription) return
    var sub = markAsRead(this.props.subscription, this.props.chapter.id, true)
    saveSubscription(sub)
  }

  render():React.Element {
    return <div />
  }
}

