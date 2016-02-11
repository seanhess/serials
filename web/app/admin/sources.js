// @flow

import React from 'react'
import {SourceModel, Status} from '../model/source'
import {statusColor} from '../books/book-info'
import {Link} from 'react-router'
import {sortBy} from 'lodash'
import {displayIf} from '../style'
import {Routes} from '../router'

// ok, so we've got some sources, baby
// just render a row really quick?

// proposals on top

export class Sources extends React.Component {

  static load() {
    return {
      sources: SourceModel.findAll()
    }
  }

  render():?React.Element {
    var sources = this.props.sources || []
    var proposed = sources.filter(s => s.status === Status.Proposed)
    var approved = sources.filter(s => s.status !== Status.Proposed)

    return <div>
      <h3>Sources</h3>
      <a className="button" href="#/sources/new">Add Source</a>

      <label>Proposals</label>
      <SourceList sources={proposed} />

      <label>Approved</label>
      <SourceList sources={approved} />
    </div>
  }
}

class SourceList extends React.Component {

  render():React.Element {

    return <table style={{width: '100%'}}>
      <tr>
        <th>Image</th>
        <th>Name</th>
        <th>URL</th>
        <th>Status</th>
      </tr>
      {this.props.sources.map(this.renderRow.bind(this))}
    </table>
  }

  renderRow(source):React.Element {
    var lastScan = source.lastScan || {}
    return <tr key={source.id}>
      <td style={{padding: 3, textAlign: 'center'}}>
        <Link to={Routes.book} params={source}>
          <img src={source.imageUrl} style={{height: 35}}/>
        </Link>
      </td>
      <td><Link to={Routes.source} params={{id: source.id}}>{source.name}</Link></td>
      <td><a href={source.url}>{source.url}</a></td>
      <td>
        <span style={{color: statusColor(source.status)}}>{source.status}</span>
        <span className="fa fa-eye-slash" style={displayIf(source.hidden)}></span>
      </td>
    </tr>
  }
}

