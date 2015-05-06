// @flow

import React from 'react'
import {chapterProxyURL, findChapter} from '../model/chapter'
import {findSubscription} from '../model/subscription'

var MARK_READ_INTERVAL = 60*1000

export class Read extends React.Component {

  timer:any;

  static load(params) {
    // interesting... I need to do two in order
    var pchap = findChapter(params.id)

    return {
      chapter: pchap,
      //subscription: 
      //content: pchap.then(proxyHTML)
    }
  }

  componentDidMount() {
    // if they read for at least 1 minute, mark it as read
    this.timer = setTimeout(() => {
    }, MARK_READ_INTERVAL)
  }

  componentWillUnmount() {
    clearTimeout(this.timer)
  }

  render() {

    if (!this.props.chapter) {
      return <div/>
    }

    var url = chapterProxyURL(this.props.chapter)

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

    return <div>
      <iframe src={url} style={frameStyle}>
        Your browser doesn't support iFrames.
      </iframe>
    </div>
  }
}
