
// @flow

import React from 'react'
import url from 'url'
import {cloneDeep} from 'lodash'

export class Chapters extends React.Component {
  render() {
    var chapters = this.props.chapters || []
    var onUpdate = this.props.onUpdate
    //var source = this.props.source

    var row = c => <ChapterRow chapter={c} />

    return <div>
      <table>
        <tr>
          <th></th>
          <th>Number</th>
          <th>Name</th>
          <th>URL</th>
        </tr>
        {chapters.map(row)}
      </table>
    </div>
  }
}

export class ChapterRow extends React.Component {

  constructor(props) {
    super(props)
    this.state = {editing: null}
  }

  render() {
    var content;
    if (this.state.editing) {
      content = this.renderEdit()
    }
    else {
      content = this.renderView()
    }
    return content
  }

  update(f) {
    return (e) => {
      var chapter = this.state.editing    
      f(chapter, e.target.value)
      this.setState({editing: chapter})
    }
  }

  //updateSource(f) {
    //return (e) => {
      //var source = this.state.source
      //f(source, e.target.value)
      //this.setState({source: source})
    //}
  //}

  renderEdit() {

    var chapter = this.state.editing

    var update = mkUpdate((setter) => {
      this.setState({editing: setter(chapter)})
    })

    var chapter = this.state.editing
    return <tr key={chapter.id}>
      <td colSpan="4">
        <div className="row">
          <div className="columns small-2">
            <label>Number</label>
            <input type="number" value={chapter.chapterNumber}
              onChange={update((c, v) => c.chapterNumber = v)}
            />
          </div>
          <div className="columns small-10">
            <label>Name</label>
            <input type="text" value={chapter.chapterName} 
              onChange={update((c, v) => c.chapterName = v)}/>
          </div>
        </div>
        <label>URL</label>
        <input type="text" value={chapter.chapterURL}
          onChange={update((c, v) => c.chapterURL = v)}
        />
        <button onClick={this.save.bind(this)}>Save</button>
      </td>
    </tr>
  }

  renderView() {
    var chapter = this.props.chapter
    return <tr key={chapter.id}>
      <td><a onClick={this.edit.bind(this)}>Edit</a></td>
      <td>{chapter.chapterNumber}</td>
      <td>{chapter.chapterName}</td>
      <td><a href={chapter.chapterURL}>{urlPath(chapter.chapterURL)}</a></td>
    </tr>
  }

  edit() {
    this.setState({editing: cloneDeep(this.props.chapter)})
  }

  save() {
    var chapter = this.state.editing
    console.log("SAVE!", chapter)
    this.setState({editing: null})
  }
}

function urlPath(u) {
  var uri = url.parse(u)
  var out = uri.path
  if (uri.query)
    out += uri.query
  return out
}

// Generate a change handler that takes setter functions
// it'll look like this
// > update((c, v) => c.something = v)
// 
function mkUpdate(save) {
  return function(setter) {
    return function(e) {
      var value = e.target.value

      function updateValue(current) {
        setter(current, value)
        return current
      }

      save(updateValue, value)
    }
  }
}

//data Chapter = Chapter {
  //id :: Maybe Text,

  //sourceId :: Text,

  //chapterNumber :: Int,
  //chapterName :: Text,
  //chapterURL :: Text,
  //chapterHidden :: Bool

//} deriving (Show, Generic)

