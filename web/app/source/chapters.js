// @flow

import React from 'react'
import url from 'url'
import {cloneDeep, findIndex} from 'lodash'
import {toDateString} from '../helpers'
import {makeUpdate, number} from '../data/update'
import {isLink, emptyChapter, Chapter, setContentText, contentText, chapterContentURL} from '../model/chapter'
import {displayIf, Colors} from '../style'

export class ChaptersList extends React.Component {

  props: {
    update: Function;
    chapters: Array<Chapter>;
  };

  constructor(props:any) {
    super(props)
    this.state = {dragging: null}
  }

  onDragChapter(chapter:Chapter) {
    this.setState({dragging: chapter})
  }

  onDrop() {
    this.setState({dragging: null})
  }

  onDragOver(chapter:Chapter) {
    var chapters = this.props.chapters || []
    var toIndex = findIndex(chapters, c => c.id === chapter.id)
    var fromIndex = findIndex(chapters, c => c.id === this.state.dragging.id)
    // I need to remove it from the array, and splice into the other place
    chapters.splice(toIndex, 0, chapters.splice(fromIndex, 1)[0])
    this.props.update(chapters)
  }

  render():?React.Element {
    var chapters = this.props.chapters || []
    var update = this.props.update

    function updateChapter(chapter:Chapter) {
      var cs = chapters.map(function(c) {
        if (c.id === chapter.id) {
          return chapter
        }
        return c
      })
      update(cs)
    }

    function deleteChapter(chapter:Chapter) {
      update(chapters.filter(c => c.id !== chapter.id))
    }

    var row = c => (
      <ChapterRow key={c.id}
        isDragging={this.state.dragging && this.state.dragging.id == c.id}
        chapter={c}
        update={updateChapter}
        delete={deleteChapter}
        onDragChapter={this.onDragChapter.bind(this)}
        onDragOver={this.onDragOver.bind(this)}
      />
    )

    var small = {
      width: 10
    }

    return <div onDrop={this.onDrop.bind(this)}>
      <table style={{width: '100%'}}>
        <tr style={{width: '100%'}}>
          <th style={small}></th>
          <th style={small}></th>
          <th style={small}></th>
          <th>Content</th>
          <th style={small}></th>
          <th>First Scanned</th>
        </tr>
        {chapters.map(row)}
      </table>
    </div>
  }
}

export class ChapterRow extends React.Component {

  props: {
    isDragging: boolean;
    chapter:Chapter;
    update:Function;
    delete:Function;
    onDragChapter:Function;
    onDragOver:Function;
  };

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
    var chapter = this.state.editing
    chapter.edited = false
    this.setState({editing: null})
    this.props.update(chapter)
  }

  edit() {
    this.setState({editing: cloneDeep(this.props.chapter)})
  }

  delete() {
    var chapter = this.props.chapter
    this.props.delete(chapter)
  }

  toggleHidden() {
    var chapter = this.props.chapter
    chapter.hidden = !chapter.hidden
    this.props.update(chapter)
  }

  save() {
    var chapter = this.state.editing
    this.setState({editing: null})
    chapter.edited = true
    this.props.update(chapter)
  }

  renderEdit():?React.Element {

    var chapter:any = this.state.editing || emptyChapter("")

    var update = makeUpdate(chapter, (v) => {
      this.setState({editing: v})
    })

    var nameValue = (isLink(chapter)) ? chapter.content.linkText : chapter.content.titleText

    return <tr key={chapter.id}>
      <td colSpan="5">
        <label>Name</label>
        <input type="text" value={nameValue}
          onChange={update((c, v) => setContentText(c, v))}/>
        <div style={displayIf(isLink(chapter))}>
          <label>URL</label>
          <input type="text" value={chapter.content.linkURL}
            onChange={update((c, v) => c.content.linkURL = v)}
          />
        </div>
        <div className="right">
          <button className="secondary" onClick={this.delete.bind(this)}>Delete</button>
        </div>
        <div>
          <button onClick={this.save.bind(this)}>Done</button>
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

    var onDragOver = (e) => {
      e.preventDefault();
      this.props.onDragOver(chapter)
    }

    var onDragStart = () => {
      this.props.onDragChapter(chapter)
    }

    var rowStyle = {}
    if (this.props.isDragging) {
      rowStyle = {
        outline: "solid 1px " + Colors.paperLine,
        background: Colors.paper
      }
    }

    return <tr key={chapter.id} onDragOver={onDragOver} style={rowStyle}>
      <td draggable="true" onDragStart={onDragStart}><span className="fa fa-bars"></span></td>
      <td><a onClick={this.edit.bind(this)}>Edit</a></td>
      <td><a onClick={this.toggleHidden.bind(this)} style={style}>
        <span className="fa fa-eye"></span></a>
      </td>
      <td style={style}>
        {contentText(chapter)}
      </td>
      <td>
        <a href={chapterContentURL(chapter)} style={style}><span className="fa fa-external-link"></span></a>
      </td>
      <td>{toDateString(chapter.added)}</td>
    </tr>
  }
}


function urlPath(u) {
  var uri = url.parse(u, false, false)
  var out = uri.path
  return out
}

