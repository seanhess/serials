// @flow

var React = require('react')

import {get} from 'axios'

export class Sources extends React.Component {

  static load() {
    console.log("LOAD")
  }

  constructor(props) {
    super(props)
    this.state = {sources: []}
  }

  render() {
    function renderRow(source) {
      return <tr>
        <td>{source.name}</td>
      </tr>
    }

    return <div>
      <h3>Sources</h3>
      <table>
        <tr>
          <th>Name</th>
        </tr>
        {this.state.sources.map(renderRow)}
      </table>
    </div>
  }
}

console.log("LOADING----")
get("http://localhost:3001/sources")
.then(function() {
  console.log("HELLO")
})
