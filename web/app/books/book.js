// @flow

import React from 'react'
import {Link} from 'react-router'
import Promise from 'bluebird'

import {RouteHandler} from 'react-router'
import {SourceModel, Source, emptySource, SourceStatus, Status} from '../model/source'
import {ChapterModel, showChapter, isLink, proxyURL} from '../model/chapter'
import {Users} from '../model/user'
import {findSubscription, saveSubscription} from '../model/subscription'
import {Cover} from'../cover'

import {toDateString} from '../helpers'
import {SomethingWrong} from './support'
import {last, groupBy, values} from 'lodash'

function loadSubscription(params) {
  return Users.auth().then((user) => findSubscription(user.id, params.id))
}

export class Book extends React.Component {

  static load(params) {
    return {
      source: SourceModel.find(params.id),
      chapters: ChapterModel.findBySource(params.id),
      subscription: loadSubscription(params)
    }
  }

  constructor(props) {
    super(props)
    this.state = {subscription: null}
  }

  toggleSubscribe() {

    var sub = this.state.subscription
    if (!sub) {
      // TODO: get them to log in
      return
    }

    sub.subscribed = !sub.subscribed

    return saveSubscription(sub)
    .then(() => this.reloadSubscription())
  }

  reloadSubscription() {
    return loadSubscription(this.props.params)
    .then((sub) => this.setState({subscription: sub}))
  }

  componentWillReceiveProps(props) {
    this.setState({subscription: props.subscription})
  }

  render() {
    var subscription = this.state.subscription

    var source:Source = this.props.source || emptySource()
    var chapters = this.props.chapters || []
    var lastChapter = last(chapters) || {}
    var shown = chapters.filter(showChapter)

    // group them by arcs?
    var row = (c) => <Chapter chapter={c} key={c.id} />

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
        {this.renderSubscribe(subscription)}
      </div>

      <hr />

      <div style={{marginTop: 10}}>
        {shown.map(row)}
      </div>

      <hr />

      <SomethingWrong />
    </div>
  }

  renderSubscribe(subscription) {
    var hasSubscription = subscription && subscription.subscribed
    var className = "expand"
    var text = "Subscribe"
    if (hasSubscription) {
      className += " secondary"
      text = "Unsubscribe"
    }

    return <button className={className} onClick={this.toggleSubscribe.bind(this)}>{text}</button>
  }
}

export function statusColor(status:SourceStatus):string {
  if (status === Status.Active) {
    return "#009800"
  }
  else if (status === Status.Complete) {
    return "#207DE5"
  }
  else {
    return "#888"
  }
}

export class Chapter extends React.Component {
  render() {
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
    return <Link to="chapter" params={{id: chapter.id}}>
      {chapter.content.linkText}
    </Link>
  }

  renderTitle(chapter) {
    return <h5 style={{marginTop: 15, marginBottom: 5}}>{chapter.content.titleText}</h5>
  }
}

