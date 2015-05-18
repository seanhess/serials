// @flow

import React from 'react'
import {Link} from 'react-router'
import {invitesAll, invitesAdd} from '../model/invite'
import {userApiURL} from '../model/user'
import {reloadHandler} from '../data/load'
import {makeUpdate} from '../data/update'

// should farm them out to other display components
// should have model functions that do all the lifting
// but it needs to reload too ... hmm ... 

type InvitesProps = {
  invites: Array<any>
}

export class Invites extends React.Component {

  props: InvitesProps;

  static load(params) {
    return {invites: invitesAll()}
  }

  addInvites(emails:Array<string>) {
    Promise.all(emails.map(e => invitesAdd(e)))
    .then(reloadHandler)
  }

  render():React.Element {

    var invites = this.props.invites || []

    return <div>
      <h2>Invites</h2>
      <InvitesList invites={invites} />
      <BulkInvites onAdd={this.addInvites.bind(this)}/>
    </div>
  }
}

export class InvitesList extends React.Component {
  render():React.Element {
    return <table>
      <tr>
        <th>Email</th>
        <th>Code</th>
        <th>User</th>
      </tr>

      {this.props.invites.map((invite) => {
        return <tr>
          <td>{invite.email}</td>
          <td><Link to="signup" params={{code: invite.code}}>{invite.code}</Link></td>
          <td><a href={userApiURL(invite.userId)}>{invite.userId}</a></td>
        </tr>
      })}
    </table>
  }
}

export class BulkInvites extends React.Component {

  props: {
    onAdd:(emails:Array<string>)=>void;
  };

  constructor(props:any) {
    super(props)
    this.state = {
      text: ""
    }
  }

  onClickAdd() {
    var emails = this.state.text.split(/[,\s]+/)
    this.props.onAdd(emails)
    this.setState({text: ""})
  }

  render():React.Element {

    var update = e => this.setState({text: e.target.value})

    return <div>
      <div><textarea rows="4" value={this.state.text} onChange={update} placeholder="comma or whitespace separated emails"></textarea></div>
      <div><button onClick={this.onClickAdd.bind(this)}>Add Invites</button></div>
    </div>
  }
}