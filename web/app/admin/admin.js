
// @flow

import React from 'react'
import {Link} from 'react-router'

import {RouteHandler} from 'react-router'
import {Header} from '../layout/header'
import {importLog} from '../model/admin'
import {settings} from '../model/settings'
import {FormSection} from '../comp'
import {Routes} from '../router'

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

  render():React.Element {
    var sets = settings()
    return <div>
      <div>
        <h3>Admin</h3>

        <p className="row">
          <label className="columns small-12 medium-2">Endpoint</label>
          <code className="columns small-12 medium-10" style={{fontSize: 'smaller'}}>{sets.appEndpoint}</code>
        </p>

        <p className="row">
          <label className="columns small-12 medium-2">Version</label>
          <code className="columns small-12 medium-10" style={{fontSize: 'smaller'}}>{sets.version}</code>
        </p>

        <p className="row">
          <label className="columns small-12 medium-2">Environment</label>
          <code className="columns small-12 medium-10" style={{fontSize: 'smaller'}}>{sets.appEnvironment}</code>
        </p>

        <hr />

        <div>
          <FormSection title="Sources">
            <div>
              <Link className="button info" to={Routes.sources}>
                <span className="fa fa-book"></span>
                <span> Sources</span>
              </Link>

              <span> </span>

              <Link className="button secondary" to={Routes.changeFeed}>
                <span className="fa fa-list"></span>
                <span> Change Feed</span>
              </Link>
            </div>
            <div><a href="#/admin/import-log/500">Import Log</a></div>
          </FormSection>

          <FormSection title="Users">
            <Link className="button info" to={Routes.users}>
              <span className="fa fa-user"></span>
              <span> Users</span>
            </Link>
            <span> </span>
            <Link className="button info" to={Routes.invites}>
              <span className="fa fa-paper-plane"></span>
              <span> Invites</span>
            </Link>
          </FormSection>

        </div>

      </div>
    </div>
  }
}
