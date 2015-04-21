// @flow

var React = require('react')
import {SourceModel} from './model.js'
import {Link} from 'react-router'
import {sortBy} from 'lodash'

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
      return <tr key={source.id}>
        <td><Link to="source" params={{id: source.id}}>{source.name}</Link></td>
        <td><a href={source.url}>{source.url}</a></td>
        <td>{!source.disabled ? 'Active' : 'Disabled'}</td>
      </tr>
    }

    return <div>
      <h3>Sources</h3>
      <table>
        <tr>
          <th>Name</th>
          <th>URL</th>
          <th>Active</th>
        </tr>
        {sorted.map(renderRow)}
      </table>
      <div><a href="#/admin/sources/new">Add Source</a></div>
    </div>
  }
}

