// @flow

var React = require('react')
var {SourceModel} = require('../model')
var {Link} = require('react-router')
var {sortBy} = require('lodash')

export class Sources extends React.Component {

  static load() {
    return SourceModel.findAll()
    .then(function(sources) {
      return {sources: sources}
    })
  }

  constructor(props) {
    super(props)
  }

  render() {
    var sources = this.props.sources || []

    var sorted = sortBy(sources, s => s.sourceDisabled)

    function renderRow(source) {
      var lastScan = source.lastScan || {}
      return <tr key={source.id}>
        <td style={{padding: 3, textAlign: 'center'}}><img src={source.imageUrl} style={{height: 35}}/></td>
        <td><Link to="source" params={{id: source.id}}>{source.name}</Link></td>
        <td><a href={source.url}>{source.url}</a></td>
        <td>{!source.disabled ? 'Active' : 'Disabled'}</td>
        <td>{lastScan.total}</td>
      </tr>
    }

    return <div>
      <h3>Sources</h3>
      <div>This is prototype software, there is no security whatsoever. Be careful with our data!</div>
      <ul>
        <li><a href="#/admin/import-log/500">Import Log</a></li>
      </ul>
      <table>
        <tr>
          <th>Image</th>
          <th>Name</th>
          <th>URL</th>
          <th>Active</th>
          <th>Chapters</th>
        </tr>
        {sorted.map(renderRow)}
      </table>
      <div><a href="#/admin/sources/new">Add Source</a></div>
    </div>
  }
}

