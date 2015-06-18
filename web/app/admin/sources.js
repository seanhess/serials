// @flow

import React from 'react'
import {SourceModel} from '../model/source'
import {statusColor} from '../books/book-info'
import {Link} from 'react-router'
import {sortBy} from 'lodash'
import {displayIf} from '../style'
import {Routes} from '../router'

export class Sources extends React.Component {

  static load() {
    return {sources: SourceModel.findAll()}
  }

  render():?React.Element {
    var sources = this.props.sources || []

    var sorted = sortBy(sources, s => s.sourceDisabled)

    function renderRow(source) {
      var lastScan = source.lastScan || {}
      return <tr key={source.id}>
        <td style={{padding: 3, textAlign: 'center'}}>
          <Link to={Routes.book} params={source}>
            <img src={source.imageUrl} style={{height: 35}}/>
          </Link>
        </td>
        <td><Link to={Routes.source} params={{id: source.id}}>{source.name}</Link></td>
        <td><a href={source.url}>{source.url}</a></td>
        <td style={{color: statusColor(source.status)}}>{source.status}</td>
        <td><span className="fa fa-eye-slash" style={displayIf(source.hidden)}></span></td>
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
          <th>Hidden</th>
          <th>Chapters</th>
        </tr>
        {sorted.map(renderRow)}
      </table>

    </div>
  }
}

