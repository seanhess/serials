// @flow

import React from 'react'
import url from 'url'
import {cloneDeep} from 'lodash'
import {toDateString} from '../helpers'
import {makeUpdate, number} from '../data/update'
import {isLink, emptyChapter, Chapter} from '../model/chapter'

export class Chapters extends React.Component {
  render():?React.Element {
    var chapters = this.props.chapters || []
    var onUpdate = this.props.onUpdate
    //var source = this.props.source

    var row = c => (
      <ChapterRow chapter={c}
        onSaveChapter={this.props.onSaveChapter}
        onClearChapter={this.props.onClearChapter}
        onDeleteChapter={this.props.onDeleteChapter}
        onHiddenChange={this.props.onHiddenChange}
      />
    )

    return <div>
      <table>
        <tr>
          <th></th>
          <th></th>
          <th>Number</th>
          <th>Content</th>
          <th>Added</th>
        </tr>
        {chapters.map(row)}
      </table>
    </div>
  }
}

export class LinkContent extends React.Component {
  render():?React.Element {
    var content = this.props.content
    return <div>
      <span>{content.linkText}</span>
      <span> - </span>
      <span><a href={content.linkURL}>{urlPath(content.linkURL)}</a></span>
    </div>
  }
}

export class TitleContent extends React.Component {
  render():?React.Element {
    var content = this.props.content
    return <div>{content.titleText}</div>
  }
}

export class Content extends React.Component {
  render():?React.Element {
    var content = this.props.content
    var inner = ""
    if (content.tag == "Link") {
      inner = <LinkContent content={content} />
    }
    else {
      inner = <TitleContent content={content} />
    }
    return <div>{inner}</div>
  }
}


export class ChapterRow extends React.Component {

  constructor(props:any) {
    super(props)
    this.state = {editing: null}
  }

  render():?React.Element {
    var content;
    if (this.state.editing) {
      content = this.renderEdit()
    }
    else {
      content = this.renderView()
    }
    return content
  }

  clear() {
    var chapter = this.props.chapter
    this.setState({editing: null})
    this.props.onClearChapter(chapter)
  }

  delete() {
    var chapter = this.props.chapter
    this.setState({editing: null})
    this.props.onDeleteChapter(chapter)
  }

  edit() {
    this.setState({editing: cloneDeep(this.props.chapter)})
  }

  toggleHidden() {
    var chapter = this.props.chapter
    this.props.onHiddenChange(chapter, !chapter.hidden)
  }

  save() {
    var chapter = this.state.editing
    this.setState({editing: null})
    this.props.onSaveChapter(chapter)
  }

  renderEdit():?React.Element {

    var chapter:any = this.state.editing || emptyChapter("")

    var update = makeUpdate(chapter, (v) => {
      this.setState({editing: v})
    })

    function updateName(chapter:any, value) {
      if (isLink(chapter)) {
        chapter.content.linkText = value
      }

      else {
        chapter.content.titleText = value
      }
    }

    var nameValue = (isLink(chapter)) ? chapter.content.linkText : chapter.content.titleText

    var urlFieldStyle = {
      display: (isLink(chapter)) ? 'block' : 'none'
    }

    return <tr key={chapter.id}>
      <td colSpan="6">
        <div className="row">
          <div className="columns small-2">
            <label>Number</label>
            <input type="number" value={chapter.number}
              onChange={update((c, v) => c.number = v, number)}
            />
          </div>
          <div className="columns small-10">
            <label>Name</label>
            <input type="text" value={nameValue}
              onChange={update(updateName)}/>
          </div>
        </div>
        <div style={urlFieldStyle}>
          <label>URL</label>
          <input type="text" value={chapter.content.linkURL}
            onChange={update((c, v) => c.content.linkURL = v)}
          />
        </div>
        <div className="right">
          <button className="secondary" onClick={this.delete.bind(this)}>Delete</button>
        </div>
        <div>
          <button onClick={this.save.bind(this)}>Save</button>
          <span> </span>
          <button className="secondary" onClick={this.clear.bind(this)}>Revert to Scan</button>
          <span> </span>
        </div>
      </td>
    </tr>
  }

  renderView():?React.Element {
    var chapter = this.props.chapter


    var color;
    if (chapter.hidden) {
      color = '#AAA'
    }

    var weight;
    if (chapter.edited) {
      weight = 'bold'
    }

    var style = {
      color: color,
      fontWeight: weight
    }


    return <tr key={chapter.id}>
      <td><a onClick={this.edit.bind(this)}>Edit</a></td>
      <td><a onClick={this.toggleHidden.bind(this)} style={style}>
        <span className="fa fa-eye"></span></a>
      </td>
      <td style={style}>{chapter.number}</td>
      <td style={style}><Content content={chapter.content}/></td>
      <td>{toDateString(chapter.added)}</td>
    </tr>
  }
}

function urlPath(u) {
  var uri = url.parse(u, false, false)
  var out = uri.path
  return out
}

