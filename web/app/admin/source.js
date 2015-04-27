// @flow

var React = require('react')
var Promise = require('bluebird')
var Router = require('react-router')

var {SourceModel, ChapterModel, emptySource, toDateString} = require('../model')
var {Chapters} = require('./chapters.js')
var {ImportSettings} = require('./import.js')
var {DisabledButton, FormSection} = require('../comp')
var {coverStyle} = require('../cover')

export class Source extends React.Component {

  static load(params) {
    if (params.id == "new") {
      return Promise.resolve({source: emptySource()})
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
    this.state = {source: emptySource(), scanning: false, chapters: []}
  }

  componentWillReceiveProps(props) {
    this.setState({
      // store them locally so you can refresh them!
      source: props.source || emptySource(),
      chapters: props.chapters || []
    })
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

  onSaveChapter(chapter) {
    ChapterModel.save(chapter)
    .then(this.reloadChapters.bind(this))
  }

  reloadChapters() {
    return ChapterModel.findBySource(this.props.params.id)
    .then((chapters) => {
      this.setState({chapters: chapters})
    })
  }

  onClearChapter(chapter) {
    ChapterModel.clear(chapter)
    .then(this.reloadChapters.bind(this))
  }

  onHiddenChapter(chapter, hidden) {
    ChapterModel.hidden(chapter, hidden)
    .then(this.reloadChapters.bind(this))
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
    source.disabled = !source.disabled
    this.setState({source: source})
  }

  runScan() {
    this.setState({scanning: true})
    ChapterModel.importSource(this.props.params.id)
    .then(() => {
      this.setState({scanning: false})
    })
    .then(this.reloadChapters.bind(this))
  }

  deleteAllChapters() {
    ChapterModel.deleteBySource(this.props.params.id)
    .then(this.reloadChapters.bind(this))
  }

  onUpdateSettings(settings) {
    var source = this.state.source
    source.importSettings = settings
    this.setState({source: source})
  }

  render() {
    var source:Source = this.state.source || {}
    var chapters = this.state.chapters || []
    var lastScan = source.lastScan || {new: [], updated: []}

    console.log("HI", lastScan)

    var scanningDisabled = (this.state.scanning) ? "disabled" : ""
    var scanningText = (this.state.scanning) ? "Scanning..." : "Scan Now"

    return <div>
      <h3>Source</h3>
      <div className="right">
        <DisabledButton onClick={this.toggleActive.bind(this)} disabled={source.disabled} />
      </div>

      <div>
        <button className="" onClick={this.onSaveClick.bind(this)}>Save</button>
        <span> </span>
        <a className="secondary button" href="#/admin/sources">Cancel</a>
      </div>

      <FormSection title="Basic Settings">
        <div>
          <div style={{float: 'left', width: 220}}>
            <div style={coverStyle(source.imageUrl)}>
            </div>
          </div>

          <div style={{marginLeft: 220, minHeight: 305}}>
            <label>Name</label>
            <input type="text" 
              value={source.name} 
              onChange={this.updateSource((s, v) => s.name = v)}
            />
            <label>URL</label>
            <input type="text" 
              value={source.url}
              onChange={this.updateSource((s, v) => s.url = v)}
            />
            <label>Image URL</label>
            <input type="text" 
              value={source.imageUrl}
              onChange={this.updateSource((s, v) => s.imageUrl = v)}
            />
          </div>
        </div>

      </FormSection>

      <FormSection title="Import Settings">
        <ImportSettings settings={source.importSettings} onUpdate={this.onUpdateSettings.bind(this)} />
      </FormSection>

      <h4>{chapters.length} Chapters</h4>

      <div>
        Last Scan
        <ul>
          <li>Date: {toDateString(lastScan.date)}</li>
          <li>Total: {lastScan.total}</li>
          <li>New: {lastScan.new.length}</li>
          <li>Updated: {lastScan.updated.length}</li>
        </ul>
      </div>

      <div>
        <button className={scanningDisabled} onClick={this.runScan.bind(this)}>{scanningText}</button>
        <span> </span>
        <button className="secondary" onClick={this.deleteAllChapters.bind(this)}>Delete All</button>
      </div>

      <Chapters chapters={chapters} source={source} 
        onSaveChapter={this.onSaveChapter.bind(this)}
        onClearChapter={this.onClearChapter.bind(this)}
        onHiddenChange={this.onHiddenChapter.bind(this)}
      />
    </div>

  }
}
