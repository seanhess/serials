
// @flow

import React from 'react'
import {Link} from 'react-router'

import {RouteHandler} from 'react-router'
import {MainContainer, Header} from '../layout/main'
import {importLog, version} from '../model/admin'

export class Admin extends React.Component {
  render():React.Element {
    return <div>
      <Header {...this.props}/>
      <MainContainer>
        <RouteHandler {...this.props}/>
      </MainContainer>
    </div>
  }
}

export class ImportLog extends React.Component {

  static load(params) {
    return {text: importLog(params.n)}
  }

  render():React.Element {
    var numLines = parseInt(this.props.params && this.props.params.n, 10)
    var moreUrl = "#/admin/import-log/" + (numLines+500)

    return <div>
      <h2>Import Log</h2>
      <div><pre style={{fontSize: '14px'}}>{this.props.text}</pre></div>
      <a href={moreUrl}>More</a>
    </div>
  }
}

export class AdminDashboard extends React.Component {

  static load() {
    return {version: version()}
  }

  render():React.Element {
    return <div>
      <div>
        <h3>Admin</h3>
        <div><Link to="sources">Sources</Link></div>
        <div><Link to="invites">Invites</Link></div>
        <div><a href="#/admin/import-log/500">Import Log</a></div>
        <div>Version: <code style={{fontSize: 'smaller'}}>{this.props.version}</code></div>
      </div>
    </div>
  }
}
