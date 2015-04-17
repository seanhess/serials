// @flow

var React = require('react')
var Promise = require('bluebird')

import {SourceModel, ScanModel, ChapterModel} from './model'
import {Chapters} from './chapters.js'

export class Source extends React.Component {

  static load(params) {
    if (params.id == "new") {
      return Promise.resolve({})
    }

    return Promise.join(
      SourceModel.find(params.id), 
      ChapterModel.findBySource(params.id),
      function(source, chapters) {
        return {source, chapters}
      }
    )
  }

  constructor(props) {
    super(props)
    this.state = {source: {}}
  }

  componentWillReceiveProps(props) {
    this.setState({source: props.source || {}})
  }

  updateSource(f) {
    return (e) => {
      var source = this.state.source
      f(source, e.target.value)
      this.setState({source: source})
    }
  }

  onSaveClick() {
    if (this.props.params.id == "new") {
      this.create()
    }

    else {
      this.save()
    }
  }

  save() {
    var source = this.state.source
    SourceModel.save(this.props.source.id, source)
    .then(() => window.location.hash = "/admin/sources")
  }

  create() {
    var source = this.state.source
    SourceModel.create(source)
    .then(() => window.location.hash = "/admin/sources")
  }

  toggleActive() {
    var source = this.state.source
    source.sourceDisabled = !source.sourceDisabled
    this.setState({source: source})
  }

  render() {
    var source = this.state.source || {}
    var chapters = this.props.chapters || []

    return <div>
      <h3>Source</h3>

      <div>
        <button className="" onClick={this.onSaveClick.bind(this)}>Save</button>
        <span> </span>
        <a className="secondary button" href="#/admin/sources">Cancel</a>
      </div>

      <label>Name</label>
      <input type="text" 
        value={source.sourceName} 
        onChange={this.updateSource((s, v) => s.sourceName = v)}
      />

      <label>URL</label>
      <input type="text" 
        value={source.sourceUrl}
        onChange={this.updateSource((s, v) => s.sourceUrl = v)}
      />

      <label>Active</label>
      <DisabledButton onClick={this.toggleActive.bind(this)} disabled={source.sourceDisabled} />

      <h4>{chapters.length} Chapters</h4>
      <Chapters chapters={chapters}/>
    </div>

  }
}

class DisabledButton {
  render() {

    if (this.props.disabled) {
      var text = "Disabled"
      var className = "secondary"
    }

    else {
      var text = "Active"
      var className = "success"
    }

    return <div>
      <button className={className} onClick={this.props.onClick}>{text}</button>
    </div>
  }
}
