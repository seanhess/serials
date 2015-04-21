// @flow

var React = require('react')
var url = require('url')
var {cloneDeep} = require('lodash')

export class Chapters extends React.Component {
  render() {
    var chapters = this.props.chapters || []
    var onUpdate = this.props.onUpdate
    //var source = this.props.source

    var row = c => <ChapterRow chapter={c} onSaveChapter={this.props.onSaveChapter}/>

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

  clear() {

  }

  renderEdit() {

    var chapter = this.state.editing || {}

    var update = mkUpdate((setter) => {
      this.setState({editing: setter(chapter)})
    })

    return <tr key={chapter.id}>
      <td colSpan="4">
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
          <button className="secondary" onClick={this.clear.bind(this)}>Clear Edits</button>
        </div>
      </td>
    </tr>
  }

  renderView() {
    var chapter = this.props.chapter
    return <tr key={chapter.id}>
      <td><a onClick={this.edit.bind(this)}>Edit</a></td>
      <td>{chapter.number}</td>
      <td>{chapter.name}</td>
      <td><a href={chapter.url}>{urlPath(chapter.url)}</a></td>
    </tr>
  }

  edit() {
    this.setState({editing: cloneDeep(this.props.chapter)})
  }

  save() {
    var chapter = this.state.editing
    this.setState({editing: null})
    this.props.onSaveChapter(chapter)
  }
}

function urlPath(u) {
  var uri = url.parse(u, false, false)
  var out = uri.path
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

