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
    if (!this.props.subscription) return
    var sub = markAsRead(this.props.subscription, this.props.chapter.id, true)
    saveSubscription(sub)
  }

  //contentInnerHTML() {
    //return {__html: this.props.content}
  //}

  loaded() {
    console.log("LOADED")
    var iframe = this.refs.frame.getDOMNode()
    console.log("IFRAME", iframe)

    var win = iframe.contentWindow || iframe.contentDocument.parentWindow
    //console.log(Object.keys(win))
    console.log("WIN", win)

    // iframes suck. Think of something different
    // what else could I do?
    // I could just mark it as read immediately, and direct you to the other page? That's the most transparent of me
    // just provide a mechanism for marking as read / not...

    // LIMITATION: I can't detect whether they click the wrong one or not
    // what if they hit one and immediately back out?
    // intermediate link: check-read.js... 
    // it starts a timer? sets a start time?
    // then if they hit back, and it hasn't been very much time since they left, I can unmark it as read...
    // I need a better way to mark them as read

    //if (win.document.body) {
      //console.log("CHECK", win.document.body)
      //console.log("HEIGHT", win.document.documentElement.scrollHeight || win.document.body.scrollHeight)
        ////iframe.height = 
    //}
  }

  render():React.Element {

    if (!this.props.chapter) {
      return <div/>
    }

    // maybe it would be better to make a second application
    // and inject our source code

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

    // IGNORE PROXY FOR NOW
    // it's too slow. we would need to cache it or speed it up somehow
    //return <div dangerouslySetInnerHTML={this.contentInnerHTML()} />
    //console.log("PROXY", chapterContentURL(this.props.chapter))
    //var url = proxyURL(chapterContentURL(this.props.chapter))
    var url = chapterContentURL(this.props.chapter)
    return <div>
      <div>Hello</div>
      <iframe ref="frame" src={url} style={frameStyle} onLoad={this.loaded.bind(this)}>
        Your browser doesn't support iFrames.
      </iframe>
    </div>
  }
}

//function setIframeHeight(iframe) {
    //if (iframe) {
        //var iframeWin = iframe.contentWindow || iframe.contentDocument.parentWindow;
        //if (iframeWin.document.body) {
            //iframe.height = iframeWin.document.documentElement.scrollHeight || iframeWin.document.body.scrollHeight;
        //}
    //}
//};
