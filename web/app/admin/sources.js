// @flow

import React from 'react'
import {SourceModel} from '../model/source'
import {AdminModel} from '../model/admin'
import {Link} from 'react-router'
import {sortBy} from 'lodash'

export class Sources extends React.Component {

  static load() {
    return {sources: SourceModel.findAll(), version: AdminModel.version()}
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
      <a className="button" href="#/admin/sources/new">Add Source</a>
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

      <div>
        <ul>
          <li><a href="#/admin/import-log/500">Import Log</a></li>
          <li>Version: <code style={{fontSize: 'smaller'}}>{this.props.version}</code></li>
        </ul>
      </div>
    </div>
  }
}

