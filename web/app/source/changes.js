// @flow

import React from 'react'
import {Link} from 'react-router'
import {Source, Change, emptySource} from '../model/source'
import {toDateString} from '../helpers'
import {Routes} from '../router'

export class SourceChanges extends React.Component {
  render():React.Element {
    var changes = this.props.changes || []
    var rows = changes.map(this.renderChange.bind(this))
    return <div>
      <table>
        <tr>
          <th>Kind</th>
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
      <td>{change.kind}</td>
      <td>
        <Link to={Routes.change} params={change}>
          {toDateString(change.createdAt)}
        </Link>
      </td>
      <td>{user.firstName} {user.lastName}</td>
    </tr>
  }
}



//export class Test extends React.Component {
  //render() {
    //var source = this.props.source
    //var update = this.props.update
    //return <FormSection title="Book Details">
      //<label>Title</label>
      //<input type="text"
        //value={source.name}
        //onChange={update((s, v) => s.name = v)}
      ///>
    //</FormSection>
  //}
//}

//export class BookDetails extends React.Component {

  //props: {
    //source: Source;
    //update: Function
  //};

  //render():React.Element {
    //var source = this.props.source
    //var update = this.props.update
    //return <FormSection title="Book Details">
      //<label>Title</label>
      //<input type="text"
        //value={source.name}
        //onChange={update((s, v) => s.name = v)}
      ///>


    //</FormSection>
  //}
//}
