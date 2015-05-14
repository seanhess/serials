// @flow

import React from 'react'
import {Link} from 'react-router'
import Promise from 'bluebird'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, emptySource, SourceStatus, Status} from '../model/source'
import {ChapterModel, showChapter, isLink, proxyURL} from '../model/chapter'
import {Users} from '../model/user'
import {findSubscription, setSubscribed, SubChapter, Subscription} from '../model/subscription'
import {Cover} from'../cover'

import {toDateString} from '../helpers'
import {SomethingWrong} from './support'
import {last, groupBy, values, curry, dropWhile, takeWhile, tail} from 'lodash'
import {Colors} from '../style'

function loadSubscription(params) {
  return Users.auth().then((user) => findSubscription(user.id, params.id))
}

type ChapterAndRead = {
  chapter: Chapter;
  read: boolean;
}

var toChapterAndRead = curry(function(subs:?{[id:string]:SubChapter}, chapter:Chapter):ChapterAndRead {
  return {
    chapter: chapter,
    read: !!subs && subs[chapter.id] && subs[chapter.id].read
  }
})

function lastReadChapter(chapters:Array<ChapterAndRead>):?ChapterAndRead {
  return chapters.reduce(function(last, current) {
    if (current.read) {
      return current
    }
    return last
  }, null)
}

// where's span when I need it!
function readUnread(chapters:Array<ChapterAndRead>) {
  var last = lastReadChapter(chapters)

  if (!last) {
    return {read: [], unread: chapters}
  }

  var read = takeWhile(chapters, function(c) {
    return c != last
  }).concat([last])

  var unread = tail(dropWhile(chapters, function(c) {
    return c != last
  }))

  return {read, unread}
}

export class Book extends React.Component {

  static load(params) {
    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id),
      subscription: loadSubscription(params)
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {subscription: null, showRead: false}
  }

  toggleSubscribe() {
    var hasSubscription = !!this.state.subscription
    var sourceId = this.props.params.id
    return Users.auth()
    .then((user) => setSubscribed(user.id, sourceId, !hasSubscription))
    .then(this.reloadSubscription.bind(this))
  }

  reloadSubscription() {
    return loadSubscription(this.props.params)
    .then((sub) => this.setState({subscription: sub}))
  }

  componentWillReceiveProps(props:any) {
    this.setState({subscription: props.subscription})
  }

  showRead() {
    this.setState({showRead: true})
  }

  render():?React.Element {
    var sub = this.state.subscription

    var source:Source = this.props.source || emptySource()
    var chapters = this.props.chapters || []
    var lastChapter = last(chapters) || {}
    var shown = chapters.filter(showChapter)
    var chaptersAndSubs = shown.map(toChapterAndRead(sub && sub.chapters))

    var row = (cs) => <Chapter chapter={cs.chapter} read={cs.read} key={cs.chapter.id} />

    // split into two groups, those less than the last chapter read, and those greater than it
    var {read, unread} = readUnread(chaptersAndSubs)

    var readContent = ""
    
    if (this.state.showRead) {
      readContent = read.map(row)
    }
    else if (read.length) {
      readContent = <a onClick={this.showRead.bind(this)} style={ReadStyle}>Show {read.length} read chapters</a>
    }

    return <div>
      <h3> </h3>
      <div style={{marginTop: 10}}>
        <div style={{float: 'left', width: 160, marginBottom: 15}}>
          <Cover source={source} />
        </div>
        <div style={{marginLeft: 160}}>
          <h3>{source.name}</h3>
          <div>by {source.author}</div>
          <div style={{color: '#888'}}>Updated {toDateString(lastChapter.added)}</div>
          <div style={{color: statusColor(source.status)}}>{source.status}</div>
        </div>
      </div>

      <div style={{clear: 'both'}}>
        {this.renderSubscribe(sub)}
      </div>

      <hr />

      <div style={{marginTop: 10}}>
        {readContent}
      </div>

      <div style={{marginTop: 10}}>
        {unread.map(row)}
      </div>

      <hr />

      <SomethingWrong />
    </div>
  }

  renderSubscribe(subscription:Subscription):?React.Element {
    var hasSubscription = !!subscription
    var className = "expand"
    var text = "Subscribe"
    if (hasSubscription) {
      className += " secondary"
      text = "Subscribed!"
    }

    return <button className={className} onClick={this.toggleSubscribe.bind(this)}>{text}</button>
  }
}

export function statusColor(status:SourceStatus):string {
  if (status === Status.Active) {
    return "#009800"
  }
  else if (status === Status.Complete) {
    //return "#207DE5"
    return Colors.dark
  }
  else {
    return "#888"
  }
}

var ReadStyle = {
  color:"#AAA"
}

export class Chapter extends React.Component {
  render():React.Element {
    var chapter:Chapter = this.props.chapter

    var content = ""
    if (isLink(chapter)) {
      content = this.renderLink(this.props.chapter)
    }
    else {
      content=this.renderTitle(this.props.chapter)
    }

    return <div>{content}</div>
  }

  renderLink(chapter:Chapter) {
    var style = {}

    if (this.props.read) {
      style = ReadStyle
    }

    return <Link to="chapter" params={{id: chapter.id}} style={style}>
      {chapter.content.linkText}
    </Link>
  }

  renderTitle(chapter:Chapter):React.Element {
    return <h5 style={{marginTop: 15, marginBottom: 5}}>{chapter.content.titleText}</h5>
  }
}

