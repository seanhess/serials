// @flow

var React = require('react')

import {SourceModel} from './model'

export class Source extends React.Component {

  static load(params) {
    return SourceModel.find(params.id)
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

  save() {
    var source = this.state.source
    SourceModel.save(this.props.source.id, source)
    .then(() => window.location.hash = "/admin/sources")
  }

  render() {
    var {source} = this.state

    return <div>
      <h3>Source</h3>

      <div><button onClick={this.save.bind(this)}>Save</button></div>

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

    </div>
  }
}

