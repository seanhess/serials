// @flow

var React = require('react')

export class Scans extends React.Component {
  render() {
    var scans = this.props.scans || []

    function row(scan) {
      return <tr key={scan.id}>
        <td>{scan.date}</td>
        <td><pre>{JSON.stringify(scan.links)}</pre></td>
      </tr>
    }

    return <div>
      <table>
        <td>
          <th>Date</th>
          <th>Links</th>
        </td>
        {scans.map(row)}
      </table>
    </div>
  }
}
