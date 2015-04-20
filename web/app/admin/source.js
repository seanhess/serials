// @flow

var React = require('react')
var Promise = require('bluebird')

import {SourceModel, ScanModel, ChapterModel, emptySource} from './model'
import {Chapters} from './chapters.js'
import {ImportSettings} from './import.js'

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
    this.state = {source: emptySource()}
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
    console.log("RUN SCAN!", this.props.params.id)
    ChapterModel.importSource(this.props.params.id)
    .then(function() {
      console.log("done?")
    })
  }

  onUpdateSettings(settings) {
    var source = this.state.source
    source.importSettings = settings
    this.setState({source: source})
  }

  render() {
    var source = this.state.source || {}
    var chapters = this.props.chapters || []

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
      <div><button onClick={this.runScan.bind(this)}>Scan Now</button></div>
      <Chapters chapters={chapters} source={source} onSaveChapter={this.onSaveChapter.bind(this)}/>
    </div>

  }
}

class FormSection {
  render() {

    var contentStyle = {
      padding: 15,
    }

    var headerStyle = {
      backgroundColor: '#F2F2F2',
      padding: 15,
    }

    var mainStyle = {
      border: 'solid 1px #D8D8D8',
      marginBottom: 20
    }

    return <div>
      <div style={mainStyle}>
        <div style={headerStyle}>{this.props.title}</div>
        <div style={contentStyle}>{this.props.children}</div>
      </div>
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

//function updateSource(f) {
  //return (e) => {
    //var source = this.state.source
    //f(source, e.target.value)
    //this.setState({source: source})
  //}
//}
