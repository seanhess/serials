// @flow

var React = require('react')

import {SourceModel} from './model.js'

export class Sources extends React.Component {

  static load() {
    return SourceModel.findAll()
  }

  constructor(props) {
    super(props)
  }

  render() {
    var sources = this.props.sources || []
    console.log("SOURCES", sources)

    function renderRow(source) {
      return <tr>
        <td>{source.sourceName}</td>
        <td>{source.sourceUrl}</td>
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

