// @flow

var React = require('react')
var Promise = require('bluebird')
var Router = require('react-router')

var {SourceModel, ScanModel, ChapterModel, emptySource} = require('./model')
var {Chapters} = require('./chapters.js')
var {ImportSettings} = require('./import.js')
var {DisabledButton, FormSection} = require('../comp')

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
    this.state = {source: emptySource(), scanning: false}
  }

  componentWillReceiveProps(props) {
    this.setState({source: props.source || emptySource()})
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
    console.log("SAVE CHAPTER", chapter)    
    ChapterModel.save(chapter)
    .then(function() {
      console.log("saved")
    })
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
    // do something amazing!
    this.setState({scanning: true})
    ChapterModel.importSource(this.props.params.id)
    .then(() => {
      console.log("- done")
      this.setState({scanning: false})
      console.log("HELLO", Router)
      console.log("- done2")
      console.log("CHECK", Router.refresh)
      ////Router.refresh()
      //window.refresh()
    })
  }

  onUpdateSettings(settings) {
    var source = this.state.source
    source.importSettings = settings
    this.setState({source: source})
  }

  render() {
    var source:Source = this.state.source || {}
    var chapters = this.props.chapters || []

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
        <div className="row">
          <div className="small-5 columns">
            <label>Name</label>
            <input type="text" 
              value={source.name} 
              onChange={this.updateSource((s, v) => s.name = v)}
            />
          </div>

          <div className="small-7 columns">
            <label>URL</label>
            <input type="text" 
              value={source.url}
              onChange={this.updateSource((s, v) => s.url = v)}
            />
          </div>
        </div>
      </FormSection>

      <FormSection title="Import Settings">
        <ImportSettings settings={source.importSettings} onUpdate={this.onUpdateSettings.bind(this)} />
      </FormSection>

      <h4>{chapters.length} Chapters</h4>

      <div>
        <button className={scanningDisabled} onClick={this.runScan.bind(this)}>{scanningText}</button>
      </div>

      <Chapters chapters={chapters} source={source} onSaveChapter={this.onSaveChapter.bind(this)}/>
    </div>

  }
}
