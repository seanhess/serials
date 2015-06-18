// @flow

import React from 'react'
import {Link} from 'react-router'
import {invitesAll, invitesAdd, invitesSend, invitesDelete, Invite} from '../model/invite'
import {userApiURL} from '../model/user'
import {reloadHandler} from '../data/load'
import {makeUpdate} from '../data/update'
import {toDateString} from '../helpers'
import {sortBy, reverse} from 'lodash'
import {Routes} from '../router'

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

  sendInvite(code:string) {
    invitesSend(code)
    .then(reloadHandler)
  }

  deleteInvite(code:string) {
    invitesDelete(code)
    .then(reloadHandler)
  }

  render():React.Element {

    var invites = sortBy(this.props.invites || [], i => i.created).reverse()

    return <div>
      <h2>Invites</h2>
      <InvitesList invites={invites}
        onSend={this.sendInvite.bind(this)}
        onDelete={this.deleteInvite.bind(this)}
      />
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
        <th></th>
        <th>Sent</th>
        <th>Created</th>
        <th></th>
      </tr>

      {this.props.invites.map(this.renderRow.bind(this))}
    </table>
  }

  renderRow(invite:Invite):React.Element {

    var sent = " "

    if (invite.sent) {
      sent = toDateString(invite.sent)
    }

    return <tr>
      <td>{invite.email}</td>
      <td><Link to={Routes.signup} params={{code: invite.code}}>{invite.code}</Link></td>
      <td><UserCell invite={invite} onSend={this.props.onSend}/></td>
      <td><InvitesSend invite={invite} onSend={this.props.onSend}/></td>
      <td>{sent}</td>
      <td>{toDateString(invite.created)}</td>
      <td><a onClick={() => this.props.onDelete(invite.code)}>
        <span className="fa fa-trash"></span>
      </a></td>
    </tr>
  }
}

class UserCell extends React.Component {
  render():React.Element {
    var invite = this.props.invite

    if (invite.signup) {
      return <a href={userApiURL(invite.signup.userId)}>{toDateString(invite.userId)}</a>
    }

    return <span/>
  }
}

export class InvitesSend extends React.Component {
  render():React.Element {
    var invite = this.props.invite
    return <a onClick={this.props.onSend.bind(null, invite.code)}>
      <span className="fa fa-paper-plane"></span>
    </a>
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
