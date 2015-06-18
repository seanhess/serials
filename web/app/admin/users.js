// @flow

// @flow

import React from 'react'
import {Link} from 'react-router'
import {User, Users, userApiURL} from '../model/user'
import {reloadHandler} from '../data/load'
import {makeUpdate} from '../data/update'
import {toDateString} from '../helpers'
import {clickable} from '../style'
import {sortBy, reverse} from 'lodash'
import {Routes} from '../router'

// should farm them out to other display components
// should have model functions that do all the lifting
// but it needs to reload too ... hmm ... 

type UsersProps = {
  users: Array<User>
}

export class AdminUsers extends React.Component {

  props: UsersProps;

  static load(params) {
    return {users: Users.loadAll()}
  }

  delete(id:string) {
    Users.delete(id)
    .then(reloadHandler)
  }

  render():React.Element {

    var users = sortBy(this.props.users || [], u => u.created).reverse()

    return <div>
      <h2>Users</h2>
      <UsersList users={users}
        onDelete={this.delete.bind(this)}
      />
    </div>
  }
}

export class UsersList extends React.Component {
  render():React.Element {
    return <table>
      <tr>
        <th></th>
        <th>Name</th>
        <th>Email</th>
        <th>Books</th>
        <th>Created</th>
        <th></th>
      </tr>

      {this.props.users.map(this.renderRow.bind(this))}
    </table>
  }

  renderRow(user:User):React.Element {

    return <tr>
      <td><a href={userApiURL(user.id)}><span className="fa fa-code"></span></a></td>
      <td>{user.firstName} {user.lastName}</td>
      <td>{user.email}</td>
      <td><Link to={Routes.bookshelf} params={user}><span className="fa fa-book"></span></Link></td>
      <td>{toDateString(user.created)}</td>
      <td><a onClick={() => this.props.onDelete(user.id)} style={clickable}>
        <span className="fa fa-trash"></span>
      </a></td>
    </tr>
  }
}
