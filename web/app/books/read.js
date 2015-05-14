// @flow

import React from 'react'
import {chapterContentURL, proxyContent, proxyURL, findChapter} from '../model/chapter'
import {loadSubscription} from '../model/user'
import {saveSubscription} from '../model/subscription'

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

  componentDidMount() {
    // if they read for at least 1 minute, mark it as read
    this.timer = setTimeout(() => {
      this.markAsRead()
    }, MARK_READ_INTERVAL)
  }


  componentWillUnmount() {
    clearTimeout(this.timer)
  }

  markAsRead() {
    var sub = this.props.subscription
    if (!sub) return

    sub.chapters[this.props.chapter.id] = {
      chapterId: this.props.chapter.id,
      read: true
    }

    saveSubscription(sub)
  }

  //contentInnerHTML() {
    //return {__html: this.props.content}
  //}

  render():React.Element {

    if (!this.props.chapter) {
      return <div/>
    }

    // maybe it would be better to make a second application
    // and inject our source code
    console.log("TEST", this.props.content)

    var frameStyle = {
      border: 0,
      position: 'absolute',
      top: 0,
      bottom: 0,
      right: 0,
      left: 0,
      width: '100%',
      height: '100%'
    }

    //return <div dangerouslySetInnerHTML={this.contentInnerHTML()} />

    var url = proxyURL(chapterContentURL(this.props.chapter))
    return <iframe src={url} style={frameStyle}>
      Your browser doesn't support iFrames.
    </iframe>
  }
}
