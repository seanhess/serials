// @flow

import React from 'react'
import {Link} from 'react-router'
import {Source, emptySource} from '../model/source'
import {Change} from '../model/change'
import {toDateString} from '../helpers'
import {Routes} from '../router'

export class SourceChanges extends React.Component {
  render():React.Element {
    var changes = this.props.changes || []
    var rows = changes.map(this.renderChange.bind(this))
    return <div>
      <table>
        <tr>
          <th>Date</th>
          <th>User</th>
        </tr>
        {rows}
      </table>
    </div>
  }

  renderChange(change:Change):React.Element {
    var user = change.createdBy
    return <tr key={change.id}>
      <td>
        <Link to={Routes.change} params={change}>
          {toDateString(change.createdAt)}
        </Link>
      </td>
      <td>{user.firstName} {user.lastName}</td>
    </tr>
  }
}
