// @flow

import React from 'react'
import {Link, RouteHandler} from 'react-router'
import {last, groupBy, values, curry, dropWhile, takeWhile, tail, assign} from 'lodash'

import {SourceModel, Source, emptySource, SourceStatus, Status} from '../model/source'
import {showChapter, isLink, proxyURL, chapterContentURL, contentText, Chapter} from '../model/chapter'
import {Users, loadSubscription} from '../model/user'
import {setSubscribed, SubChapter, Subscription, markAsRead, saveSubscription, newSubscription} from '../model/subscription'
import {Alerts} from '../model/alert'
import {findBookmark, toChapterAndRead, ChapterAndRead} from './bookmark'

import {toDateString} from '../helpers'
import {Colors, clickable, CollapseBorder, Cell} from '../style'
import {transitionTo, Routes} from '../router'
import {BookInfo, CoverColumns, BookArt, BookTitle, BookDetails} from './book-info'


export class Book extends React.Component {

  props: {
    params: {id: string};
    source: Source;
  };

  static load(params) {
    return {
      source: SourceModel.find(params.id)
    }
  }

  constructor(props:any) {
    super(props)
    this.state = {
      subscription: newSubscription(Users.currentUserId(), props.params.id),
      showRead: false
    }
  }

  // really, I should call a lifecycle function when the url loads. That makes way more sense

  forceLogin() {
    transitionTo(Routes.login, {}, {to: 'book', id: this.props.params.id})
  }

  toggleSubscribe() {

    if (!Users.isLoggedIn()) {
      this.forceLogin()
      return
    }

    var sourceId = this.props.params.id

    // set subscribed to false
    var sub = this.state.subscription
    sub.subscribed = !sub.subscribed
    this.setState({subscription: sub})

    // update on the server
    return setSubscribed(Users.currentUserId(), sourceId, sub.subscribed)
    .then(function() {
      if (sub.subscribed) {
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

  componentDidMount() {
    this.reloadSubscription()
  }

  markAsReadUnread(chapter:Chapter, read:boolean) {
    if (!this.state.subscription.subscribed) {
      Alerts.update('info', 'Please subscribe to enable bookmarking')
      return Promise.resolve()
    }
    var sub = markAsRead(this.state.subscription, chapter.id, read)
    this.setState({subscription:sub})
    return saveSubscription(sub)
  }

  markAsRead(chapter:Chapter) {
    return this.markAsReadUnread(chapter, true)
  }

  markAsUnread(chapter:Chapter) {
    return this.markAsReadUnread(chapter, false)
  }

  readChapter(chapter:Chapter) {
    this.markAsReadUnread(chapter, true)
    .then(function() {
      var url = chapterContentURL(chapter)
      window.location = url
    })
  }

  readBookmark(cr:?ChapterAndRead):void {
    if (!cr) return
    var chapter = cr.chapter
    //var node = React.findDOMNode(this)
    //console.log("READ", chapter, node)
    var chapterNode = document.getElementById(chapter.id)
    chapterNode.scrollIntoView()
  }

  render():?React.Element {
    var sub = this.state.subscription

    var source:Source = this.props.source || emptySource()
    var chapters = source.chapters || []
    var shown = chapters.filter(showChapter)

    var readChapters = {}
    if (sub.subscribed) {
      readChapters = sub.chapters
    }

    var chaptersAndSubs = shown.map(toChapterAndRead(readChapters))
    var lastChapter = last(chapters) || {}

    //var current = chaptersAndSubs.filter(unread)[0]
    var current:?ChapterAndRead = findBookmark(chaptersAndSubs)

    return <div>
      <h3> </h3>

      <BookTitle source={source} />

      <div style={{marginTop: 10}}>
        {this.renderSubscribe(sub, source)}
      </div>

      <div style={{marginTop: -5}}>
        <CoverColumns>
          <BookArt source={source} />
          <BookInfo source={source} lastChapter={lastChapter}/>
        </CoverColumns>
      </div>

      <div style={{clear: 'both', marginBottom: -5}}>
        <ReadBookmark current={current} onClick={() => this.readBookmark(current)}/>
      </div>

      <div>
        {chaptersAndSubs.map(this.renderChapterRow.bind(this, current))}
      </div>

      <div style={{marginTop: 15}}>

        <BookDetails source={source}/>

        <EditBook source={source} />
      </div>

      <div style={{height: 50}}> </div>
    </div>
  }

  renderChapterRow(current:?ChapterAndRead, cs:ChapterAndRead):React.Element {
      return <ChapterRow
        chapter={cs.chapter} 
        read={cs.read} 
        key={cs.chapter.id} 
        onClick={this.readChapter.bind(this)}
        onMarkRead={this.markAsRead.bind(this)}
        onMarkUnread={this.markAsUnread.bind(this)}
        isCurrent={cs == current}
      />
  }

  renderSubscribe(sub:Subscription, source:Source):?React.Element {
    var className = "expand"
    var content = <div>
      <div style={{fontSize: 20, fontWeight: 'bold'}}>Subscribe</div>
      <div style={{fontSize: 14, marginTop: 10, fontWeight: 'normal'}}>Enables bookmarking and notifications</div>
    </div>
    if (sub.subscribed) {
      className += " secondary"
      content = "Subscribed"
    }

    return <button className={className} 
      onClick={this.toggleSubscribe.bind(this)}>
      {content}
    </button>
  }
}

var ReadStyle = {
  color:"#AAA"
}

export class ChapterRow extends React.Component {

  props: {
    chapter: Chapter;
    read: boolean;
    onMarkUnread: Function;
    onMarkRead: Function;
    onClick: Function;
    isCurrent: boolean;
  };

  render():React.Element {
    var chapter:Chapter = this.props.chapter

    var content = ""
    if (isLink(chapter)) {
      content = this.renderLink(this.props.chapter)
    }
    else {
      content = this.renderTitle(this.props.chapter)
    }

    return <div id={chapter.id}>
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

  onClickChapter() {
    this.props.onClick(this.props.chapter)
  }

  renderLink(chapter:Chapter):React.Element {
    var readStyle = {color: Colors.highlight}

    if (this.props.read) {
      readStyle = ReadStyle
    }

    return <div className="row" style={Cell}>

      <div style={{display: 'table-cell', verticalAlign: 'middle', width: 26}}>
        <ReadIcon read={this.props.read} onClick={this.toggleIsRead.bind(this)} isCurrent={this.props.isCurrent}/>
      </div>

      <div style={{display: 'table-cell'}}>
        <a onClick={this.onClickChapter.bind(this)} style={assign({}, readStyle, clickable)}>
          {contentText(chapter)}
        </a>
      </div>
    </div>
  }

  renderTitle(chapter:Chapter):React.Element {
    var style = assign({}, CollapseBorder, {
      borderBottom: 'solid 1px #666',
      padding: 10,
      backgroundColor: Colors.light
    })
    return <div className="row" style={style}>
      <h5 style={{margin:0, color: Colors.dark}}>{contentText(chapter)}</h5>
    </div>
  }
}


// this should only show up 
// if subscribed, and not on the first chapter
// eh, do this later
class ReadBookmark extends React.Component {

  props: {
    current: ChapterAndRead;
    onClick: Function;
  };

  render():React.Element {
    var current = this.props.current

    var bookmark = ""

    if (current) {
      bookmark = <button onClick={this.props.onClick} className="info expand">
        <span className="fa fa-bookmark"></span>
        <span> Read - </span>
        <span> {contentText(current.chapter)}</span>
      </button>
    }

    return <div>{bookmark}</div>
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

    return <a onClick={this.props.onClick} style={clickable}>
      <span className={icon} style={style}/>
    </a>
  }
}

export class EditBook extends React.Component {

  props: {source: Source};

  render():React.Element {
    return <div>
      <Link className="secondary button" to={Routes.source} params={this.props.source}>
        <span className="fa fa-pencil-square-o"></span>
        <span> Edit book details</span>
      </Link>
    </div>
  }
}

