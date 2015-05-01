// @flow

var React = require('react')
var url = require('url')
var {cloneDeep} = require('lodash')
var {toDateString} = require('../helpers')
var {makeUpdate} = require('../data/update')

export class Chapters extends React.Component {
  render() {
    var chapters = this.props.chapters || []
    var onUpdate = this.props.onUpdate
    //var source = this.props.source

    var row = c => (
      <ChapterRow chapter={c} 
        onSaveChapter={this.props.onSaveChapter}
        onClearChapter={this.props.onClearChapter}
        onHiddenChange={this.props.onHiddenChange}
      />
    )

    return <div>
      <table>
        <tr>
          <th></th>
          <th></th>
          <th>Number</th>
          <th>Name</th>
          <th>URL</th>
          <th>Added</th>
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

  clear() {
    var chapter = this.props.chapter
    this.setState({editing: null})
    this.props.onClearChapter(chapter)
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

  renderEdit() {

    var chapter = this.state.editing || {}

    var update = makeUpdate(chapter, (v) => {
      this.setState({editing: v})
    })

    return <tr key={chapter.id}>
      <td colSpan="6">
        <div className="row">
          <div className="columns small-2">
            <label>Number</label>
            <input type="number" value={chapter.number}
              onChange={update((c, v) => c.number = v)}
            />
          </div>
          <div className="columns small-10">
            <label>Name</label>
            <input type="text" value={chapter.name} 
              onChange={update((c, v) => c.name = v)}/>
          </div>
        </div>
        <label>URL</label>
        <input type="text" value={chapter.url}
          onChange={update((c, v) => c.chapterURL = v)}
        />
        <div>
          <button onClick={this.save.bind(this)}>Save</button>
          <span> </span>
          <button className="secondary" onClick={this.clear.bind(this)}>Revert to Scan</button>
        </div>
      </td>
    </tr>
  }

  renderView() {
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
      <td style={style}>{chapter.name}</td>
      <td><a href={chapter.url} style={style}>{urlPath(chapter.url)}</a></td>
      <td>{toDateString(chapter.added)}</td>
    </tr>
  }

}

function urlPath(u) {
  var uri = url.parse(u, false, false)
  var out = uri.path
  return out
}

