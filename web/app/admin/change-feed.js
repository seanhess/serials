// @flow
import React from 'react'
import {Link} from 'react-router'
import {Change, allChanges} from '../model/change'
import {toDateString} from '../helpers'
import {Routes} from '../router'

export class ChangeFeed extends React.Component {

  props: {
    changes: Array<Change>;
  };

  static load() {
    return {changes: allChanges()}
  }

  render():React.Element {
    var changes = this.props.changes || []
    var rows = changes.map(this.renderChange.bind(this))
    return <div>
      <h2>Changes</h2>
      <table>
        <tr>
          <th>Date</th>
          <th>User</th>
          <th>Source</th>
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
      <td>
        <Link to={Routes.book} params={change.source}>
          <span> {change.source.name}</span>
        </Link>
      </td>
    </tr>
  }
}
