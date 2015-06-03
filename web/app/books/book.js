// @flow

import React from 'react'
import {Link} from 'react-router'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, emptySource, SourceStatus, Status} from '../model/source'
import {ChapterModel, showChapter, isLink, proxyURL} from '../model/chapter'
import {Users, loadSubscription} from '../model/user'
import {findSubscription, setSubscribed, SubChapter, Subscription, markAsRead, saveSubscription} from '../model/subscription'
import {Alerts} from '../model/alert'
import {Cover} from'../cover'

import {toDateString} from '../helpers'
import {SomethingWrong} from './support'
import {last, groupBy, values, curry, dropWhile, takeWhile, tail, assign} from 'lodash'
import {Colors} from '../style'

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

function unread(c:ChapterAndRead) {
  return !c.read
}

//function lastReadChapter(chapters:Array<ChapterAndRead>):?ChapterAndRead {
  //return chapters.reduce(function(last, current) {
    //if (current.read) {
      //return current
    //}
    //return last
  //}, null)
//}

//function untilLastRead(chapters:Array<ChapterAndRead>, last:?ChapterAndRead):Array<ChapterAndRead> {
  //if (!last) return []
  //return takeWhile(chapters, function(c) {
    //return c != last
  //}).concat([last])
//}

//function afterLastRead(chapters:Array<ChapterAndRead>, last:?ChapterAndRead):Array<ChapterAndRead> {
  //if (!last) return chapters
  //return tail(dropWhile(chapters, function(c) {
    //return c != last
  //}))
//}

export class Book extends React.Component {

  static load(params) {
    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id)
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {subscription: null, showRead: false}
  }

  toggleSubscribe() {
    var hasSubscription = !!this.state.subscription
    var sourceId = this.props.params.id
    setSubscribed(Users.currentUserId(), sourceId, !hasSubscription)
    .then(function() {
      if (!hasSubscription) {
        Alerts.update("success", "You are subscribed!")
      }
      else {
        Alerts.update("secondary", "You are unsubscribed")
      }
    })
    .then(this.reloadSubscription.bind(this))
  }

  reloadSubscription() {
    return loadSubscription(this.props.params.id)
    .then((sub) => this.setState({subscription: sub}))
  }

  componentWillReceiveProps(props:any) {
    this.reloadSubscription()
  }

  markAsRead(chapter:Chapter) {
    // then replace them in place
    var sub = markAsRead(this.state.subscription, chapter.id, true)
    saveSubscription(sub)
    .then(this.reloadSubscription.bind(this))
  }

  markAsUnread(chapter:Chapter) {
    // then replace them in place
    var sub = markAsRead(this.state.subscription, chapter.id, false)
    saveSubscription(sub)
    .then(this.reloadSubscription.bind(this))
  }

  render():?React.Element {
    var sub = this.state.subscription

    var source:Source = this.props.source || emptySource()
    var chapters = this.props.chapters || []
    var lastChapter = last(chapters) || {}
    var shown = chapters.filter(showChapter)
    var chaptersAndSubs = shown.map(toChapterAndRead(sub && sub.chapters))

    var current = chaptersAndSubs.filter(unread)[0]

    var row = (cs) => <Chapter 
                        chapter={cs.chapter} 
                        read={cs.read} 
                        key={cs.chapter.id} 
                        onMarkRead={this.markAsRead.bind(this)}
                        onMarkUnread={this.markAsUnread.bind(this)}
                        isCurrent={cs == current}
                      />
    
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
        {this.renderSubscribe(sub, source)}
      </div>

      <div style={{marginTop: 10}}>
        {chaptersAndSubs.map(row)}
      </div>

      <br />

      <SomethingWrong />
    </div>
  }

  renderSubscribe(subscription:Subscription, source:Source):?React.Element {
    var hasSubscription = !!subscription
    var className = "expand"
    var text = "Subscribe to " + source.name
    if (hasSubscription) {
      className += " secondary"
      text = "Subscribed"
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

var CollapseBorder = {
  marginBottom: -1
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

    return <div>
      {content}
    </div>
  }

  toggleIsRead() {
    if (this.props.read) {
      this.props.onMarkUnread(this.props.chapter)
    }

    else {
      this.props.onMarkRead(this.props.chapter)
    }
  }

  renderLink(chapter:Chapter):React.Element {
    var readStyle = {color: Colors.highlight}

    if (this.props.read) {
      readStyle = ReadStyle
    }

    var border = 'solid 1px #DDD'

    return <div className="row" style={assign({}, CollapseBorder, {borderBottom: border, borderTop: border, padding: 10})}>

      <div style={{display: 'table-cell', verticalAlign: 'middle', width: 26}}>
        <ReadIcon read={this.props.read} onClick={this.toggleIsRead.bind(this)} isCurrent={this.props.isCurrent}/>
      </div>

      <div style={{display: 'table-cell'}}>
        <Link to="chapter" params={{id: chapter.id}} style={readStyle}>
          {chapter.content.linkText}
        </Link>
      </div>
    </div>
  }

  renderTitle(chapter:Chapter):React.Element {
    return <div className="row" style={assign({}, CollapseBorder, {borderBottom: 'solid 1px #666', padding: 10, marginTop: 20})}>
      <h5 style={{margin:0}}>{chapter.content.titleText}</h5>
    </div>
  }
}


// this should only show up 
// if subscribed, and not on the first chapter
// eh, do this later
class ReadNow extends React.Component {
  render():React.Element {
    return <div>
      <button>Bookmark: </button>
    </div>
  }
}

class ReadIcon extends React.Component {
  render():React.Element {
    var readStyle = {color: Colors.highlight}
    var icon = "fa fa-circle"
    var padding = 0

    if (this.props.read) {
      readStyle = ReadStyle
      icon = "fa fa-circle-thin"
    }

    if (this.props.isCurrent) {
      padding = 1
      icon = "fa fa-bookmark"
    }

    var style = assign({}, readStyle, {fontSize: 16, marginLeft: 2, paddingLeft: padding})

    return <a onClick={this.props.onClick}>
      <span className={icon} style={style}/>
    </a>
  }
}
