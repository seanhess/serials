// @flow

var React = require('react')

import {SourceModel} from './model.js'
import {Link} from 'react-router'

export class Sources extends React.Component {

  static load() {
    console.log("LOAD sources")
    return SourceModel.findAll()
  }

  constructor(props) {
    super(props)
  }


  render() {
    var sources = this.props.sources || []

    function renderRow(source) {
      return <tr key={source.id}>
        <td><Link to="source" params={{id: source.id}}>{source.sourceName}</Link></td>
        <td><a href={source.sourceUrl}>{source.sourceUrl}</a></td>
      </tr>
    }

    return <div>
      <h3>Sources</h3>
      <table>
        <tr>
          <th>Name</th>
          <th>URL</th>
        </tr>
        {sources.map(renderRow)}
      </table>
    </div>
  }
}

